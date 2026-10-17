use arbitrary_int::{i4, u10};

use crate::{
    assembler::{
        AssemblerError, LabelMap,
        backends::{
            Lowered, Machine, Target,
            args::{MnemonicTable, decode, numbered_registers},
            lower_each,
        },
        get_address_value, get_immediate_value, get_offset_value,
    },
    lexer::token::{Condition, Register, Span},
    parser::{
        DefineMap, ParsedItem,
        operations::{Address, Immediate, Offset},
    },
};

const REGISTER_COUNT: u8 = 16;

pub struct BatPu2;

impl Target for BatPu2 {
    fn name(&self) -> &'static str {
        "batpu2-mattbatwings-none"
    }

    fn extensions(&self) -> &'static [&'static str] {
        &["smc"]
    }

    fn bytes_per_address(&self) -> usize {
        // Instruction memory holds 16 bit words
        2
    }

    fn insert_before(&self, defines: &mut DefineMap) -> Result<(), AssemblerError> {
        insert_before(defines)
    }

    fn lower(&self, items: &[ParsedItem]) -> (Vec<Lowered>, Vec<AssemblerError>) {
        // Every instruction is one 16 bit word
        lower_each(items, |instruction| {
            let op = decode(MNEMONICS, numbered_registers, instruction)?;
            let span = instruction.span.clone();
            Ok(vec![Machine::new(2, move |resolver| {
                Ok(
                    assemble_operation(resolver.defines, resolver.labels, op, span)?
                        .to_be_bytes()
                        .to_vec(),
                )
            })])
        })
    }
}

#[derive(Debug, Clone)]
enum Op {
    Nop,
    Hlt,
    Add3(Register, Register, Register),
    Sub3(Register, Register, Register),
    Nor3(Register, Register, Register),
    And3(Register, Register, Register),
    Xor3(Register, Register, Register),
    Rsh2(Register, Register),
    Ldi2(Register, Immediate),
    Adi2(Register, Immediate),
    Jmp(Address),
    Brh(Condition, Address),
    Cal(Address),
    Ret,
    Lod(Register, Register, Option<Offset>),
    Str(Register, Register, Option<Offset>),

    // Pseudo instructions
    Cmp2(Register, Register),
    Mov2(Register, Register),
    Lsh2(Register, Register),
    Not2(Register, Register),
    Neg2(Register, Register),
    Inc1(Register),
    Dec1(Register),
}

const MNEMONICS: &MnemonicTable<Op> = &[
    ("nop", |_| Ok(Op::Nop)),
    ("hlt", |_| Ok(Op::Hlt)),
    ("add", |a| {
        Ok(Op::Add3(a.register(0)?, a.register(1)?, a.register(2)?))
    }),
    ("sub", |a| {
        Ok(Op::Sub3(a.register(0)?, a.register(1)?, a.register(2)?))
    }),
    ("nor", |a| {
        Ok(Op::Nor3(a.register(0)?, a.register(1)?, a.register(2)?))
    }),
    ("and", |a| {
        Ok(Op::And3(a.register(0)?, a.register(1)?, a.register(2)?))
    }),
    ("xor", |a| {
        Ok(Op::Xor3(a.register(0)?, a.register(1)?, a.register(2)?))
    }),
    ("rsh", |a| Ok(Op::Rsh2(a.register(0)?, a.register(1)?))),
    ("ldi", |a| Ok(Op::Ldi2(a.register(0)?, a.immediate(1)?))),
    ("adi", |a| Ok(Op::Adi2(a.register(0)?, a.immediate(1)?))),
    ("jmp", |a| Ok(Op::Jmp(a.address(0)?))),
    ("brh", |a| {
        let condition = match a.condition(0)? {
            // Bare `z` is lexed separately so other targets can use it as a register
            Condition::Zero => Condition::Equal,
            condition => condition,
        };
        Ok(Op::Brh(condition, a.address(1)?))
    }),
    ("cal", |a| Ok(Op::Cal(a.address(0)?))),
    ("ret", |_| Ok(Op::Ret)),
    ("lod", |a| {
        let offset = if a.len() > 2 {
            Some(a.offset(2)?)
        } else {
            None
        };
        Ok(Op::Lod(a.register(0)?, a.register(1)?, offset))
    }),
    ("str", |a| {
        let offset = if a.len() > 2 {
            Some(a.offset(2)?)
        } else {
            None
        };
        Ok(Op::Str(a.register(0)?, a.register(1)?, offset))
    }),
    ("cmp", |a| Ok(Op::Cmp2(a.register(0)?, a.register(1)?))),
    ("mov", |a| Ok(Op::Mov2(a.register(0)?, a.register(1)?))),
    ("lsh", |a| Ok(Op::Lsh2(a.register(0)?, a.register(1)?))),
    ("not", |a| Ok(Op::Not2(a.register(0)?, a.register(1)?))),
    ("neg", |a| Ok(Op::Neg2(a.register(0)?, a.register(1)?))),
    ("inc", |a| Ok(Op::Inc1(a.register(0)?))),
    ("dec", |a| Ok(Op::Dec1(a.register(0)?))),
];

fn insert_before(defines: &mut DefineMap) -> Result<(), AssemblerError> {
    let ports = [
        "pixel_x",
        "pixel_y",
        "draw_pixel",
        "clear_pixel",
        "load_pixel",
        "buffer_screen",
        "clear_screen_buffer",
        "write_char",
        "buffer_chars",
        "clear_chars_buffer",
        "show_number",
        "clear_number",
        "signed_mode",
        "unsigned_mode",
        "rng",
        "controller_input",
    ];
    for (i, port) in ports.into_iter().enumerate() {
        defines.insert(port.to_string(), (i + 240) as f64);
    }

    Ok(())
}

fn assemble_operation(
    defines: &DefineMap,
    labels: &LabelMap,
    operation: Op,
    span: Span,
) -> Result<u16, AssemblerError> {
    // Instruction lowering
    let operation = match operation {
        Op::Cmp2(r1, r2) => Op::Sub3(r1, r2, Register::R0),
        Op::Mov2(r1, r2) => Op::Add3(r1, Register::R0, r2),
        Op::Lsh2(r1, r2) => Op::Add3(r1, r1, r2),
        Op::Not2(r1, r2) => Op::Nor3(r1, Register::R0, r2),
        Op::Neg2(r1, r2) => Op::Sub3(Register::R0, r1, r2),
        Op::Inc1(r1) => Op::Adi2(r1, Immediate::Value(1)),
        Op::Dec1(r1) => Op::Adi2(r1, Immediate::Value(-1)),
        _ => operation,
    };

    match operation {
        Op::Nop => Ok(0b0000 << 12),
        Op::Hlt => Ok(0b0001 << 12),
        Op::Add3(r1, r2, r3) => Ok(0b0010 << 12
            | (((r1.check(REGISTER_COUNT, &span)? & 0xF) as u16) << 8)
            | (((r2.check(REGISTER_COUNT, &span)? & 0xF) as u16) << 4)
            | ((r3.check(REGISTER_COUNT, &span)? & 0xF) as u16)),
        Op::Sub3(r1, r2, r3) => Ok(0b0011 << 12
            | (((r1.check(REGISTER_COUNT, &span)? & 0xF) as u16) << 8)
            | (((r2.check(REGISTER_COUNT, &span)? & 0xF) as u16) << 4)
            | ((r3.check(REGISTER_COUNT, &span)? & 0xF) as u16)),
        Op::Nor3(r1, r2, r3) => Ok(0b0100 << 12
            | (((r1.check(REGISTER_COUNT, &span)? & 0xF) as u16) << 8)
            | (((r2.check(REGISTER_COUNT, &span)? & 0xF) as u16) << 4)
            | ((r3.check(REGISTER_COUNT, &span)? & 0xF) as u16)),
        Op::And3(r1, r2, r3) => Ok(0b0101 << 12
            | (((r1.check(REGISTER_COUNT, &span)? & 0xF) as u16) << 8)
            | (((r2.check(REGISTER_COUNT, &span)? & 0xF) as u16) << 4)
            | ((r3.check(REGISTER_COUNT, &span)? & 0xF) as u16)),
        Op::Xor3(r1, r2, r3) => Ok(0b0110 << 12
            | (((r1.check(REGISTER_COUNT, &span)? & 0xF) as u16) << 8)
            | (((r2.check(REGISTER_COUNT, &span)? & 0xF) as u16) << 4)
            | ((r3.check(REGISTER_COUNT, &span)? & 0xF) as u16)),
        Op::Rsh2(r1, r2) => Ok(0b0111 << 12
            | (((r1.check(REGISTER_COUNT, &span)? & 0xF) as u16) << 8)
            | ((r2.check(REGISTER_COUNT, &span)? & 0xF) as u16)),
        Op::Ldi2(r1, immediate) => Ok(0b1000 << 12
            | (((r1.check(REGISTER_COUNT, &span)? & 0xF) as u16) << 8)
            | (check_immediate_value(get_immediate_value(&span, defines, immediate)?, &span)?)),
        Op::Adi2(r1, immediate) => Ok(0b1001 << 12
            | (((r1.check(REGISTER_COUNT, &span)? & 0xF) as u16) << 8)
            | (check_immediate_value(get_immediate_value(&span, defines, immediate)?, &span)?)),
        Op::Jmp(address) => Ok(0b1010 << 12
            | check_address_value(get_address_value(&span, defines, labels, address)?, &span)?),
        Op::Brh(condition, address) => {
            let condition = match condition {
                Condition::Equal => 0b00,
                Condition::NotEqual => 0b01,
                Condition::GreaterEqual => 0b10,
                Condition::Less => 0b11,
                _ => return Err(AssemblerError::InvalidCondition(span, condition)),
            };

            Ok(0b1011 << 12
                | (condition << 10)
                | check_address_value(get_address_value(&span, defines, labels, address)?, &span)?)
        }
        Op::Cal(address) => Ok(0b1100 << 12
            | (check_address_value(get_address_value(&span, defines, labels, address)?, &span)?
                & 0b0000_0011_1111_1111)),
        Op::Ret => Ok(0b1101_0000_0000_0000),
        Op::Lod(r1, r2, offset) => {
            let offset = match offset {
                Some(offset) => {
                    check_offset_value(get_offset_value(&span, defines, offset)?, &span)?
                }
                None => 0,
            };

            Ok(0b1110 << 12
                | (((r1.check(REGISTER_COUNT, &span)? & 0xF) as u16) << 8)
                | (((r2.check(REGISTER_COUNT, &span)? & 0xF) as u16) << 4)
                | offset)
        }
        Op::Str(r1, r2, offset) => {
            let offset = match offset {
                Some(offset) => {
                    check_offset_value(get_offset_value(&span, defines, offset)?, &span)?
                }
                None => 0,
            };

            Ok(0b1111 << 12
                | (((r1.check(REGISTER_COUNT, &span)? & 0xF) as u16) << 8)
                | (((r2.check(REGISTER_COUNT, &span)? & 0xF) as u16) << 4)
                | offset)
        }
        // Pseudo instructions were rewritten above
        _ => Err(AssemblerError::InvalidOperands(
            span,
            format!("{operation:?} is not encodable"),
        )),
    }
}

fn check_immediate_value(immediate: i128, span: &Span) -> Result<u16, AssemblerError> {
    if immediate < -128 || immediate > 255 {
        return Err(AssemblerError::ImmediateOutOfRange(span.clone(), immediate));
    }

    Ok((immediate as u8) as u16)
}

fn check_offset_value(offset: i128, span: &Span) -> Result<u16, AssemblerError> {
    let val: i8 = offset
        .try_into()
        .map_err(|_| AssemblerError::OffsetOutOfRange(span.clone(), offset))?;

    let val = i4::try_new(val)
        .map_err(|_| AssemblerError::OffsetOutOfRange(span.clone(), offset))?
        .value();

    Ok((val as u16) & 0xF)
}

fn check_address_value(address: i128, span: &Span) -> Result<u16, AssemblerError> {
    let val: u16 = address
        .try_into()
        .map_err(|_| AssemblerError::AddressOutOfRange(span.clone(), address))?;

    let val = u10::try_new(val)
        .map_err(|_| AssemblerError::AddressOutOfRange(span.clone(), address))?
        .value();

    Ok((val as u16) & 0b0011_1111_1111)
}
