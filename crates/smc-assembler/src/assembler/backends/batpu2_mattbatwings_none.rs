use crate::{
    assembler::{
        AssemblerError, LabelMap,
        backends::{
            Lowered, Machine, Register, Target,
            args::{MnemonicTable, Syntax, decode, numbered_register},
            fields::Field,
            lower_each,
        },
        get_address_value, get_immediate_value, get_offset_value,
    },
    isa::batpu2::{Condition, Instruction, REGISTER_COUNT},
    lexer::token::Span,
    parser::{
        DefineMap, ParsedItem,
        operations::{Address, Immediate, Offset, Operand, SpannedOperand},
    },
};

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
            let op = decode(MNEMONICS, &SYNTAX, instruction)?;
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

const IMMEDIATE: Field = Field::AnySign(8);
const ADDRESS: Field = Field::Unsigned(10);
const OFFSET: Field = Field::Signed(4);

const SYNTAX: Syntax = Syntax {
    register: numbered_register,
    character,
};

fn condition(operand: &SpannedOperand) -> Result<Condition, AssemblerError> {
    let invalid = |text: String| AssemblerError::InvalidCondition(operand.span.clone(), text);
    let text = match &operand.operand {
        Operand::Name(text) | Operand::Symbol(text) => text.to_ascii_lowercase(),
        other => return Err(invalid(other.to_string())),
    };

    match text.as_str() {
        "eq" | "=" | "z" | "zero" => Ok(Condition::Equal),
        "ne" | "!=" | "nz" | "notzero" => Ok(Condition::NotEqual),
        "ge" | ">=" | "c" | "carry" => Ok(Condition::GreaterEqual),
        "lt" | "<" | "nc" | "notcarry" => Ok(Condition::Less),
        _ => Err(invalid(text)),
    }
}

/// The BatPU-2 character display's encoding. Tau uses it too.
pub(super) fn character(c: char) -> Option<i128> {
    match c {
        ' ' => Some(0),
        'a'..='z' => Some(c as i128 - 'a' as i128 + 1),
        'A'..='Z' => Some(c as i128 - 'A' as i128 + 1),
        '.' => Some(27),
        '!' => Some(28),
        '?' => Some(29),
        _ => None,
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
        Ok(Op::Brh(condition(a.operand(0)?)?, a.address(1)?))
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

    let r = |register: Register| register.check(REGISTER_COUNT, &span);
    let immediate = |immediate| -> Result<u8, AssemblerError> {
        let value = get_immediate_value(&span, defines, immediate)?;
        Ok(IMMEDIATE.immediate(value, &span)? as u8)
    };
    let address = |address| -> Result<u16, AssemblerError> {
        let value = get_address_value(&span, defines, labels, address)?;
        Ok(ADDRESS.address(value, &span)? as u16)
    };
    let offset = |offset: Option<Offset>| -> Result<i8, AssemblerError> {
        let Some(offset) = offset else {
            return Ok(0);
        };
        let value = get_offset_value(&span, defines, offset)?;
        // Range checked, so it fits
        OFFSET.offset(value, &span)?;
        Ok(value as i8)
    };

    let instruction = match operation {
        Op::Nop => Instruction::Nop,
        Op::Hlt => Instruction::Hlt,
        Op::Add3(a, b, c) => Instruction::Add {
            a: r(a)?,
            b: r(b)?,
            c: r(c)?,
        },
        Op::Sub3(a, b, c) => Instruction::Sub {
            a: r(a)?,
            b: r(b)?,
            c: r(c)?,
        },
        Op::Nor3(a, b, c) => Instruction::Nor {
            a: r(a)?,
            b: r(b)?,
            c: r(c)?,
        },
        Op::And3(a, b, c) => Instruction::And {
            a: r(a)?,
            b: r(b)?,
            c: r(c)?,
        },
        Op::Xor3(a, b, c) => Instruction::Xor {
            a: r(a)?,
            b: r(b)?,
            c: r(c)?,
        },
        Op::Rsh2(a, c) => Instruction::Rsh { a: r(a)?, c: r(c)? },
        Op::Ldi2(a, imm) => Instruction::Ldi {
            a: r(a)?,
            immediate: immediate(imm)?,
        },
        Op::Adi2(a, imm) => Instruction::Adi {
            a: r(a)?,
            immediate: immediate(imm)?,
        },
        Op::Jmp(addr) => Instruction::Jmp {
            address: address(addr)?,
        },
        Op::Brh(condition, addr) => Instruction::Brh {
            condition,
            address: address(addr)?,
        },
        Op::Cal(addr) => Instruction::Cal {
            address: address(addr)?,
        },
        Op::Ret => Instruction::Ret,
        Op::Lod(a, b, o) => Instruction::Lod {
            a: r(a)?,
            b: r(b)?,
            offset: offset(o)?,
        },
        Op::Str(a, b, o) => Instruction::Str {
            a: r(a)?,
            b: r(b)?,
            offset: offset(o)?,
        },
        // Pseudo instructions were rewritten above
        _ => {
            return Err(AssemblerError::InvalidOperands(
                span,
                format!("Cannot encode `{operation:?}`"),
            ));
        }
    };

    Ok(instruction.encode())
}
