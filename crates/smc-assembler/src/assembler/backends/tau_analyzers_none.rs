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
    isa::tau::{Instruction, REGISTER_COUNT, Skip},
    lexer::token::Span,
    parser::{
        ParsedItem,
        operations::{Address, Immediate, Operand},
    },
};

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
    Skp(Skip),
}

const IMMEDIATE: Field = Field::AnySign(8);
const ADDRESS: Field = Field::Unsigned(10);

const SYNTAX: Syntax = Syntax {
    register: numbered_register,
    character: batpu2_mattbatwings_none::character,
};

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
        return Ok(Op::Skp(Skip::Never));
    }

    let operand = args.operand(0)?;
    let flag = match &operand.operand {
        Operand::Symbol(symbol) if symbol == "!" || symbol == "-" => Skip::Never,
        Operand::Symbol(symbol) if symbol == "!0" => Skip::IfNotZero,
        Operand::Symbol(symbol) if symbol == "!-" => Skip::IfNotNegative,
        Operand::Number(0) => Skip::IfZero,
        other => {
            return Err(AssemblerError::InvalidOperands(
                operand.span.clone(),
                format!("Invalid skip `{other}`"),
            ));
        }
    };

    Ok(Op::Skp(flag))
}

fn lower_operation(operation: Op, span: &Span) -> Result<Machine, AssemblerError> {
    use Instruction::*;
    let r = |register: Register| register.check(REGISTER_COUNT, span);
    let fixed = |instruction: Instruction| Machine::bytes(instruction.encode());
    // The immediate or address is only known once labels and defines resolve
    let immediate = |d: Register, imm: Immediate, make: fn(u8, u8) -> Instruction| {
        let d = r(d)?;
        Ok::<_, AssemblerError>(Machine::new(2, move |resolver| {
            let value = IMMEDIATE.immediate(resolver.immediate(imm)?, resolver.span)?;
            Ok(make(d, value as u8).encode())
        }))
    };
    let branch = |addr: Address, make: fn(u16) -> Instruction| {
        Machine::new(2, move |resolver| {
            let address = ADDRESS.address(resolver.address(addr)?, resolver.span)?;
            Ok(make(address as u16).encode())
        })
    };

    Ok(match operation {
        Op::Add2(d, s) => fixed(Add { d: r(d)?, s: r(s)? }),
        Op::Sub2(d, s) => fixed(Sub { d: r(d)?, s: r(s)? }),
        Op::Xor2(d, s) => fixed(Xor { d: r(d)?, s: r(s)? }),
        Op::And2(d, s) => fixed(And { d: r(d)?, s: r(s)? }),
        Op::Or2(d, s) => fixed(Or { d: r(d)?, s: r(s)? }),
        Op::Cmp2(d, s) => fixed(Cmp { d: r(d)?, s: r(s)? }),
        Op::Cpy2(d, s) => fixed(Cpy { d: r(d)?, s: r(s)? }),
        Op::Adc2(d, s) => fixed(Adc { d: r(d)?, s: r(s)? }),
        Op::Mld2(d, s) => fixed(Mld { d: r(d)?, s: r(s)? }),
        Op::Mst2(d, s) => fixed(Mst { d: r(d)?, s: r(s)? }),
        Op::Pld2(d, s) => fixed(Pld { d: r(d)?, s: r(s)? }),
        Op::Pst2(d, s) => fixed(Pst { d: r(d)?, s: r(s)? }),

        Op::Rsh1(d) => fixed(Rsh { d: r(d)? }),
        Op::Inv1(d) => fixed(Inv { d: r(d)? }),
        Op::Inc1(d) => fixed(Inc { d: r(d)? }),
        Op::Dec1(d) => fixed(Dec { d: r(d)? }),

        Op::Ldi2(d, imm) => immediate(d, imm, |d, immediate| Ldi { d, immediate })?,
        Op::Adi2(d, imm) => immediate(d, imm, |d, immediate| Adi { d, immediate })?,
        Op::Cpi2(d, imm) => immediate(d, imm, |d, immediate| Cpi { d, immediate })?,
        Op::Ani2(d, imm) => immediate(d, imm, |d, immediate| Ani { d, immediate })?,
        Op::Clr1(d) => immediate(d, Immediate::Value(0), |d, immediate| Ani { d, immediate })?,

        Op::Jmp(addr) => branch(addr, |address| Jmp { address }),
        Op::Cal(addr) => branch(addr, |address| Cal { address }),

        Op::Bkl => fixed(Bkl),
        Op::Bkr => fixed(Bkr),
        Op::Hlt => fixed(Hlt),
        Op::Ret => fixed(Ret),
        Op::Skp(skip) => fixed(Skp(skip)),
    })
}
