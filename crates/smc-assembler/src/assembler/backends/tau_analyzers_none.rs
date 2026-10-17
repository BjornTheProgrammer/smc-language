use crate::{
    assembler::{
        AssemblerError,
        backends::{
            Lowered, Machine, Register, Target,
            args::{Args, MnemonicTable, Syntax, decode, numbered_register},
            batpu2_mattbatwings_none,
            fields::Field,
            lower_each,
        },
    },
    lexer::token::Span,
    parser::{
        ParsedItem,
        operations::{Address, Immediate, Operand},
    },
};

const REGISTER_COUNT: u8 = 4;

pub struct TauAnalyzersNone;

impl Target for TauAnalyzersNone {
    fn name(&self) -> &'static str {
        "tau-analyzers-none"
    }

    fn extensions(&self) -> &'static [&'static str] {
        &["tasm"]
    }

    fn lower(&self, items: &[ParsedItem]) -> (Vec<Lowered>, Vec<AssemblerError>) {
        lower_each(items, |instruction| {
            let op = decode(MNEMONICS, &SYNTAX, instruction)?;
            Ok(vec![lower_operation(op, &instruction.span)?])
        })
    }
}

#[derive(Debug, Clone)]
enum Op {
    Add2(Register, Register),
    Sub2(Register, Register),
    Xor2(Register, Register),
    And2(Register, Register),
    Or2(Register, Register),
    Cmp2(Register, Register),
    Cpy2(Register, Register),
    Adc2(Register, Register),
    Mld2(Register, Register),
    Mst2(Register, Register),
    Pld2(Register, Register),
    Pst2(Register, Register),
    Rsh1(Register),
    Inv1(Register),
    Inc1(Register),
    Dec1(Register),
    Clr1(Register),
    Ldi2(Register, Immediate),
    Adi2(Register, Immediate),
    Cpi2(Register, Immediate),
    Ani2(Register, Immediate),
    Jmp(Address),
    Cal(Address),
    Bkl,
    Bkr,
    Hlt,
    Ret,
    Skp(SkipFlag),
}

const IMMEDIATE: Field = Field::AnySign(8);
const ADDRESS: Field = Field::Unsigned(10);

const SYNTAX: Syntax = Syntax {
    register: numbered_register,
    character: batpu2_mattbatwings_none::character,
};

#[derive(Debug, Clone)]
enum SkipFlag {
    Never,
    IfZero,
    IfNotZero,
    IfNotNegative,
}

const MNEMONICS: &MnemonicTable<Op> = &[
    ("add", |a| Ok(Op::Add2(a.register(0)?, a.register(1)?))),
    ("sub", |a| Ok(Op::Sub2(a.register(0)?, a.register(1)?))),
    ("xor", |a| Ok(Op::Xor2(a.register(0)?, a.register(1)?))),
    ("and", |a| Ok(Op::And2(a.register(0)?, a.register(1)?))),
    ("or", |a| Ok(Op::Or2(a.register(0)?, a.register(1)?))),
    ("cmp", |a| Ok(Op::Cmp2(a.register(0)?, a.register(1)?))),
    ("cpy", |a| Ok(Op::Cpy2(a.register(0)?, a.register(1)?))),
    ("adc", |a| Ok(Op::Adc2(a.register(0)?, a.register(1)?))),
    ("mld", |a| {
        Ok(Op::Mld2(a.register(0)?, a.memory_register(1)?))
    }),
    ("mst", |a| {
        Ok(Op::Mst2(a.memory_register(0)?, a.register(1)?))
    }),
    ("pld", |a| Ok(Op::Pld2(a.register(0)?, a.register(1)?))),
    ("pst", |a| Ok(Op::Pst2(a.register(0)?, a.register(1)?))),
    ("rsh", |a| Ok(Op::Rsh1(a.register(0)?))),
    ("inv", |a| Ok(Op::Inv1(a.register(0)?))),
    ("inc", |a| Ok(Op::Inc1(a.register(0)?))),
    ("dec", |a| Ok(Op::Dec1(a.register(0)?))),
    ("clr", |a| Ok(Op::Clr1(a.register(0)?))),
    ("ldi", |a| Ok(Op::Ldi2(a.register(0)?, a.immediate(1)?))),
    ("adi", |a| Ok(Op::Adi2(a.register(0)?, a.immediate(1)?))),
    ("cpi", |a| Ok(Op::Cpi2(a.register(0)?, a.immediate(1)?))),
    ("ani", |a| Ok(Op::Ani2(a.register(0)?, a.immediate(1)?))),
    ("jmp", |a| Ok(Op::Jmp(a.address(0)?))),
    ("cal", |a| Ok(Op::Cal(a.address(0)?))),
    ("bkl", |_| Ok(Op::Bkl)),
    ("bkr", |_| Ok(Op::Bkr)),
    ("hlt", |_| Ok(Op::Hlt)),
    ("ret", |_| Ok(Op::Ret)),
    ("skp", decode_skip),
];

fn decode_skip(args: &Args) -> Result<Op, AssemblerError> {
    if args.len() == 0 {
        return Ok(Op::Skp(SkipFlag::Never));
    }

    let operand = args.operand(0)?;
    let flag = match &operand.operand {
        Operand::Symbol(symbol) if symbol == "!" || symbol == "-" => SkipFlag::Never,
        Operand::Symbol(symbol) if symbol == "!0" => SkipFlag::IfNotZero,
        Operand::Symbol(symbol) if symbol == "!-" => SkipFlag::IfNotNegative,
        Operand::Number(0) => SkipFlag::IfZero,
        other => {
            return Err(AssemblerError::InvalidOperands(
                operand.span.clone(),
                format!("invalid skip condition {other:?}"),
            ));
        }
    };

    Ok(Op::Skp(flag))
}

fn assemble_2reg(
    span: &Span,
    opcode: u8,
    register: Register,
    register1: Register,
) -> Result<Machine, AssemblerError> {
    // XXXX + DD + SS
    Ok(Machine::bytes(vec![
        opcode << 4
            | (register.check(REGISTER_COUNT, span)? & 0b11) << 2
            | (register1.check(REGISTER_COUNT, span)? & 0b11),
    ]))
}

fn assemble_1reg(
    span: &Span,
    operation: u8,
    register: Register,
) -> Result<Machine, AssemblerError> {
    // Format: 1100 + DD + OO
    Ok(Machine::bytes(vec![
        0b1100 << 4 | (register.check(REGISTER_COUNT, span)? & 0b11) << 2 | (operation & 0b11),
    ]))
}

fn assemble_immediate(
    span: &Span,
    operation: u8,
    register: Register,
    immediate: Immediate,
) -> Result<Machine, AssemblerError> {
    // Format: 1101 + DD + OO + IIIIIIII
    let first_byte =
        0b1101 << 4 | (register.check(REGISTER_COUNT, span)? & 0b11) << 2 | (operation & 0b11);

    Ok(Machine::new(2, move |resolver| {
        let immediate = IMMEDIATE.immediate(resolver.immediate(immediate)?, resolver.span)?;
        Ok(vec![first_byte, immediate as u8])
    }))
}

fn assemble_branch(operation: u8, address: Address) -> Machine {
    // Format: 1110 + II + OO + IIIIIIII
    // II: upper 2 bits of 10-bit immediate
    // OO: operation code
    // IIIIIIII: lower 8 bits of 10-bit immediate
    Machine::new(2, move |resolver| {
        let address_checked = ADDRESS.address(resolver.address(address)?, resolver.span)?;

        let imm_upper = ((address_checked >> 8) & 0b11) as u8; // Upper 2 bits
        let imm_lower = (address_checked & 0xFF) as u8; // Lower 8 bits

        let first_byte = 0b1110 << 4 | imm_upper << 2 | (operation & 0b11);

        Ok(vec![first_byte, imm_lower])
    })
}

fn assemble_implicit(operation: u8) -> Machine {
    // Format: 1111 + OOOO
    Machine::bytes(vec![0b1111 << 4 | (operation & 0b1111)])
}

fn assemble_skip(flag: SkipFlag) -> Machine {
    // Format: 1111 + 1FFF
    let flag_bits = match flag {
        SkipFlag::Never => 0b000,
        SkipFlag::IfZero => 0b001,
        SkipFlag::IfNotZero => 0b010,
        // 0b011 skips if negative, which has no syntax yet
        SkipFlag::IfNotNegative => 0b100,
    };
    Machine::bytes(vec![0b1111_1 << 3 | (flag_bits & 0b111)])
}

fn lower_operation(operation: Op, span: &Span) -> Result<Machine, AssemblerError> {
    Ok(match operation {
        // 2 operand instructions
        Op::Add2(r1, r2) => assemble_2reg(span, 0b0000, r1, r2)?,
        Op::Sub2(r1, r2) => assemble_2reg(span, 0b0001, r1, r2)?,
        Op::Xor2(r1, r2) => assemble_2reg(span, 0b0010, r1, r2)?,
        Op::And2(r1, r2) => assemble_2reg(span, 0b0011, r1, r2)?,
        Op::Or2(r1, r2) => assemble_2reg(span, 0b0100, r1, r2)?,
        Op::Cmp2(r1, r2) => assemble_2reg(span, 0b0101, r1, r2)?,
        Op::Cpy2(r1, r2) => assemble_2reg(span, 0b0110, r1, r2)?,
        Op::Adc2(r1, r2) => assemble_2reg(span, 0b0111, r1, r2)?,
        Op::Mld2(r1, r2) => assemble_2reg(span, 0b1000, r1, r2)?,
        Op::Mst2(r1, r2) => assemble_2reg(span, 0b1001, r1, r2)?,
        Op::Pld2(r1, r2) => assemble_2reg(span, 0b1010, r1, r2)?,
        Op::Pst2(r1, r2) => assemble_2reg(span, 0b1011, r1, r2)?,

        // 1 operand instructions
        Op::Rsh1(r1) => assemble_1reg(span, 0b00, r1)?,
        Op::Inv1(r1) => assemble_1reg(span, 0b01, r1)?,
        Op::Inc1(r1) => assemble_1reg(span, 0b10, r1)?,
        Op::Dec1(r1) => assemble_1reg(span, 0b11, r1)?,

        // Immediate instructions
        Op::Ldi2(r1, imm) => assemble_immediate(span, 0b00, r1, imm)?,
        Op::Adi2(r1, imm) => assemble_immediate(span, 0b01, r1, imm)?,
        Op::Cpi2(r1, imm) => assemble_immediate(span, 0b10, r1, imm)?,
        Op::Ani2(r1, imm) => assemble_immediate(span, 0b11, r1, imm)?,
        Op::Clr1(r1) => assemble_immediate(span, 0b11, r1, Immediate::Value(0))?,

        Op::Jmp(addr) => assemble_branch(0b00, addr),
        Op::Cal(addr) => assemble_branch(0b01, addr),

        // Implicit instructions (except skip)
        Op::Bkl => assemble_implicit(0b0000),
        Op::Bkr => assemble_implicit(0b0001),
        Op::Hlt => assemble_implicit(0b0010),
        Op::Ret => assemble_implicit(0b0011),

        // Skip instructions
        Op::Skp(flag) => assemble_skip(flag),
    })
}
