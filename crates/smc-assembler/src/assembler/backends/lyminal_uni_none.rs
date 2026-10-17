//! Lyminal: a 1 byte, 3 operand ISA with four opcodes (A_D, SUB, MEM, BYT).
//!
//! Every assembly instruction is lowered to one or more machine instructions of the form
//! `OO XX YY ZZ`, optionally preceded by a `BYT` prefix that turns the last operand into an
//! 8 bit immediate. Register A reads -1 unless the previous instruction wrote it ("A mode"),
//! which the lowering uses for moves, increments, jumps and so on.

use crate::{
    assembler::{
        AssemblerError,
        backends::{
            Lowered, Machine, Register, Resolver, Target,
            args::{Args, MnemonicTable, Syntax, ascii, decode, numbered_register},
        },
    },
    lexer::token::Span,
    parser::{
        ParsedItem,
        operations::{Address, Immediate, Operand},
    },
};

const REGISTER_COUNT: u8 = 4;

pub struct LyminalUniNone;

impl Target for LyminalUniNone {
    fn name(&self) -> &'static str {
        "lyminal-uni-none"
    }

    fn extensions(&self) -> &'static [&'static str] {
        &["lasm"]
    }

    fn lower(&self, items: &[ParsedItem]) -> (Vec<Lowered>, Vec<AssemblerError>) {
        lower_program(items)
    }
}

#[derive(Debug, PartialEq, Clone)]
enum Op {
    Add3(Register, Register, Register),
    AddI3(Register, Register, Immediate),
    Sub3(Register, Register, Register),
    SubI3(Register, Register, Immediate),
    And3(Register, Register, Register),
    AndI3(Register, Register, Immediate),
    Mov2(Register, Register),
    MovI2(Register, Immediate),
    Ldi2(Register, Immediate),
    Lsi2(Register, Immediate),
    Not2(Register, Register),
    Inc1(Register),
    Inc2(Register, Register),
    Dec1(Register),
    Dec2(Register, Register),
    Mld2(Register, Register),
    Mst2(Register, Register),
    MstI2(Register, Immediate),
    Swp3(Register, Register, Register),
    SwpI3(Register, Register, Immediate),
    Jmp(Address),
    JmpR(Register),
    Jal2(Register, Register),
    JalI2(Register, Address),
    Pc1(Register),
    Pred(PredicateFlag),
}

#[derive(Debug, PartialEq, Clone)]
enum PredicateFlag {
    Carry,
    NotCarry,
    Zero,
    NotZero,
}

const MNEMONICS: &MnemonicTable<Op> = &[
    ("add", |a| register_or_immediate(a, Op::Add3, Op::AddI3)),
    ("sub", |a| register_or_immediate(a, Op::Sub3, Op::SubI3)),
    ("and", |a| register_or_immediate(a, Op::And3, Op::AndI3)),
    ("mov", |a| {
        let x = a.register(0)?;
        match a.is_register(1) {
            true => Ok(Op::Mov2(x, a.register(1)?)),
            false => Ok(Op::MovI2(x, a.immediate(1)?)),
        }
    }),
    ("ldi", |a| Ok(Op::Ldi2(a.register(0)?, a.immediate(1)?))),
    ("lsi", |a| Ok(Op::Lsi2(a.register(0)?, a.immediate(1)?))),
    ("not", |a| Ok(Op::Not2(a.register(0)?, a.register(1)?))),
    ("inc", |a| match a.len() {
        0..=1 => Ok(Op::Inc1(a.register(0)?)),
        _ => Ok(Op::Inc2(a.register(0)?, a.register(1)?)),
    }),
    ("dec", |a| match a.len() {
        0..=1 => Ok(Op::Dec1(a.register(0)?)),
        _ => Ok(Op::Dec2(a.register(0)?, a.register(1)?)),
    }),
    ("mld", |a| {
        Ok(Op::Mld2(a.register(0)?, a.memory_register(1)?))
    }),
    ("mst", |a| {
        let x = a.memory_register(0)?;
        match a.is_register(1) {
            true => Ok(Op::Mst2(x, a.register(1)?)),
            false => Ok(Op::MstI2(x, a.immediate(1)?)),
        }
    }),
    ("swp", |a| {
        let (x, y) = (a.register(0)?, a.memory_register(1)?);
        match a.is_register(2) {
            true => Ok(Op::Swp3(x, y, a.register(2)?)),
            false => Ok(Op::SwpI3(x, y, a.immediate(2)?)),
        }
    }),
    ("jmp", |a| match a.is_register(0) {
        true => Ok(Op::JmpR(a.register(0)?)),
        false => Ok(Op::Jmp(a.address(0)?)),
    }),
    ("jal", |a| {
        let x = a.register(0)?;
        match a.is_register(1) {
            true => Ok(Op::Jal2(x, a.register(1)?)),
            false => Ok(Op::JalI2(x, a.address(1)?)),
        }
    }),
    ("pc", |a| Ok(Op::Pc1(a.register(0)?))),
    ("pic", |_| Ok(Op::Pred(PredicateFlag::Carry))),
    ("pnc", |_| Ok(Op::Pred(PredicateFlag::NotCarry))),
    ("piz", |_| Ok(Op::Pred(PredicateFlag::Zero))),
    ("pnz", |_| Ok(Op::Pred(PredicateFlag::NotZero))),
];

/// `op x y z` or `op x y imm`, depending on the last operand
fn register_or_immediate(
    args: &Args,
    register: fn(Register, Register, Register) -> Op,
    immediate: fn(Register, Register, Immediate) -> Op,
) -> Result<Op, AssemblerError> {
    let (x, y) = (args.register(0)?, args.register(1)?);
    match args.is_register(2) {
        true => Ok(register(x, y, args.register(2)?)),
        false => Ok(immediate(x, y, args.immediate(2)?)),
    }
}

const SYNTAX: Syntax = Syntax {
    register,
    character: ascii,
};

/// Registers by name, X, Y, Z and A, as well as `rN`. They're only registers where a
/// register is expected, so they stay usable as define names elsewhere.
fn register(operand: &Operand) -> Option<Register> {
    match operand {
        Operand::Name(name) if name.eq_ignore_ascii_case("x") => Some(Register(X)),
        Operand::Name(name) if name.eq_ignore_ascii_case("y") => Some(Register(Y)),
        Operand::Name(name) if name.eq_ignore_ascii_case("z") => Some(Register(Z)),
        Operand::Name(name) if name.eq_ignore_ascii_case("a") => Some(Register(A)),
        _ => numbered_register(operand),
    }
}

// Opcodes
const AD: u8 = 0b00;
const SUB: u8 = 0b01;
const MEM: u8 = 0b10;
const BYT: u8 = 0b11;

// Register addresses
const X: u8 = 0b00;
const Y: u8 = 0b01;
const Z: u8 = 0b10;
const A: u8 = 0b11;

/// Value of a `BYT` prefixed immediate, resolved once labels are known
#[derive(Debug, Clone)]
enum Value {
    Imm(Immediate),
    /// Negated immediate, so `ADD` can be done with `SUB`
    NegImm(Immediate),
    Addr(Address),
}

/// A single machine instruction
#[derive(Debug, Clone)]
enum Base {
    /// `op x y z`
    Op(u8, u8, u8, u8),
    /// `BYT` prefix followed by `op x y imm`
    Imm(u8, u8, u8, Value),
    /// Load small immediate (-2..=2) into a register, using A or the SUB orthogonality patch
    Lsi(u8, Immediate),
}

impl Base {
    /// Whether this puts the next instruction in A mode
    fn writes_a(&self) -> bool {
        match *self {
            // MEM only loads when the destination and address differ
            Base::Op(MEM, x, y, _) | Base::Imm(MEM, x, y, _) => x == A && x != y,
            Base::Op(_, x, ..) | Base::Imm(_, x, ..) | Base::Lsi(x, _) => x == A,
        }
    }

    /// Whether the result would change if executed in A mode
    fn depends_on_a_mode(&self) -> bool {
        match *self {
            Base::Op(AD, _, y, z) => y == A || z == A,
            Base::Op(SUB, _, y, z) => y == z || y == A || z == A,
            Base::Op(MEM, x, y, z) if x == y && y == z => false,
            Base::Op(MEM, x, y, z) => y == A || (x != z && z == A),
            Base::Op(..) => false,
            Base::Imm(_, _, y, _) => y == A,
            Base::Lsi(..) => true,
        }
    }
}

/// One step of lowering an operation
#[derive(Debug, Clone)]
enum Step {
    /// Another operation, lowered in turn
    Lower(Op),
    /// A machine instruction
    Emit(Base),
}

use Step::{Emit, Lower};

/// Guards against lowerings that expand into each other forever
const MAX_LOWERING_DEPTH: usize = 8;

fn reg(span: &Span, register: Register) -> Result<u8, AssemblerError> {
    register.check(REGISTER_COUNT, span)
}

fn invalid(span: &Span, message: &str) -> AssemblerError {
    AssemblerError::InvalidOperands(span.clone(), message.to_string())
}

fn small_literal(imm: &Immediate, range: std::ops::RangeInclusive<i128>) -> Option<i128> {
    match imm {
        Immediate::Value(v) if range.contains(v) => Some(*v),
        _ => None,
    }
}

/// Lowers an operation all the way to machine instructions. `a_mode` is whether the
/// instruction before it wrote A.
fn lower_fully(op: &Op, a_mode: bool, span: &Span) -> Result<Vec<Base>, AssemblerError> {
    let mut out = Vec::new();
    expand(op, a_mode, span, &mut out, 0)?;
    Ok(out)
}

fn expand(
    op: &Op,
    initial_a_mode: bool,
    span: &Span,
    out: &mut Vec<Base>,
    depth: usize,
) -> Result<(), AssemblerError> {
    if depth > MAX_LOWERING_DEPTH {
        return Err(invalid(span, "internal error: lowering does not terminate"));
    }

    // A mode comes from whatever machine instruction was emitted last
    let a_mode = out.last().map_or(initial_a_mode, Base::writes_a);
    for step in lower(op, a_mode, span)? {
        match step {
            Lower(op) => expand(&op, initial_a_mode, span, out, depth + 1)?,
            Emit(base) => out.push(base),
        }
    }

    Ok(())
}

/// Lowers an operation one level, into machine instructions or other operations.
/// Only literal immediates may affect the choice, so the result has the same size before
/// and after labels and defines are resolved.
fn lower(op: &Op, a_mode: bool, span: &Span) -> Result<Vec<Step>, AssemblerError> {
    use Op::*;
    let r = |register: Register| reg(span, register);
    let a = Register(A);

    Ok(match op.clone() {
        // Aliases
        Ldi2(x, imm) => vec![Lower(MovI2(x, imm))],
        Inc1(x) => vec![Lower(Inc2(x, x))],
        Dec1(x) => vec![Lower(Dec2(x, x))],

        Add3(x, y, z) => {
            let (x, y, z) = (r(x)?, r(y)?, r(z)?);
            // A_D adds when the first source has the lower (or equal) address
            vec![Emit(Base::Op(AD, x, y.min(z), y.max(z)))]
        }
        AddI3(x, y, imm) => match small_literal(&imm, -1..=1) {
            Some(-1) if y == a && a_mode => vec![Lower(Dec2(x, y))],
            Some(0) if y == a && a_mode => vec![Lower(Mov2(x, y))],
            Some(1) if y == a && a_mode => vec![Lower(Inc2(x, y))],
            // y - -imm
            _ => vec![Emit(Base::Imm(SUB, r(x)?, r(y)?, Value::NegImm(imm)))],
        },
        Sub3(x, y, z) => {
            if y != z {
                vec![Emit(Base::Op(SUB, r(x)?, r(y)?, r(z)?))]
            } else if y == a && a_mode {
                // SUB A A would be !A
                vec![Lower(AndI3(x, a, Immediate::Value(0)))]
            } else {
                vec![Lower(Lsi2(x, Immediate::Value(0)))]
            }
        }
        SubI3(x, y, imm) => match small_literal(&imm, -1..=1) {
            Some(v) if y == a && a_mode => vec![Lower(AddI3(x, y, Immediate::Value(-v)))],
            _ => vec![Emit(Base::Imm(SUB, r(x)?, r(y)?, Value::Imm(imm)))],
        },
        And3(x, y, z) => {
            if y == z {
                vec![Lower(Mov2(x, y))]
            } else {
                let (x, y, z) = (r(x)?, r(y)?, r(z)?);
                // A_D ands when the first source has the higher address
                vec![Emit(Base::Op(AD, x, y.max(z), y.min(z)))]
            }
        }
        AndI3(x, y, imm) => vec![Emit(Base::Imm(AD, r(x)?, r(y)?, Value::Imm(imm)))],
        Mov2(x, y) => match (r(x)?, r(y)?) {
            (x, y) if x == y => Vec::new(),
            // A mode patch: A + 1 - 1
            (x, A) if a_mode => vec![Emit(Base::Op(SUB, x, Y, Y))],
            (_, A) => vec![Lower(Lsi2(x, Immediate::Value(-1)))],
            // -1 & y
            (x, y) => vec![Emit(Base::Op(AD, x, A, y))],
        },
        MovI2(x, imm) => match small_literal(&imm, -2..=2) {
            Some(_) => vec![Lower(Lsi2(x, imm))],
            // -1 & imm
            None => vec![Lower(AndI3(x, a, imm))],
        },
        Lsi2(x, imm) => vec![Emit(Base::Lsi(r(x)?, imm))],
        Not2(x, y) => match (r(x)?, r(y)?) {
            // A mode patch: !A
            (x, A) if a_mode => vec![Emit(Base::Op(SUB, x, A, A))],
            (_, A) => vec![Lower(Lsi2(x, Immediate::Value(0)))],
            // -1 - y
            (x, y) => vec![Emit(Base::Op(SUB, x, A, y))],
        },
        Inc2(x, y) => match (r(x)?, r(y)?) {
            // A mode patch: A + 2 - 1
            (x, A) if a_mode => vec![Emit(Base::Op(SUB, x, Z, Z))],
            (_, A) => vec![Lower(Lsi2(x, Immediate::Value(0)))],
            // y - -1
            (x, y) => vec![Emit(Base::Op(SUB, x, y, A))],
        },
        Dec2(x, y) => match (r(x)?, r(y)?) {
            // A mode patch: A + 0 - 1
            (x, A) if a_mode => vec![Emit(Base::Op(SUB, x, X, X))],
            (_, A) => vec![Lower(Lsi2(x, Immediate::Value(-2)))],
            // y + -1
            (x, y) => vec![Emit(Base::Op(AD, x, y, A))],
        },
        Mld2(x, y) => match (r(x)?, r(y)?) {
            (A, A) => return Err(invalid(span, "MLD A [A] is not encodable")),
            // Load through A, then move it out in A mode
            (rx, ry) if rx == ry => vec![Lower(Mld2(a, y)), Lower(Mov2(x, a))],
            (x, y) => vec![Emit(Base::Op(MEM, x, y, x))],
        },
        Mst2(x, y) => match (r(x)?, r(y)?) {
            (x, y) if x == y => {
                return Err(invalid(
                    span,
                    "MST [r] r stores a register to its own address, which is not encodable",
                ));
            }
            (x, y) => vec![Emit(Base::Op(MEM, x, x, y))],
        },
        MstI2(x, imm) => {
            let x = r(x)?;
            vec![Emit(Base::Imm(MEM, x, x, Value::Imm(imm)))]
        }
        Swp3(x, y, z) => match (r(x)?, r(y)?, r(z)?) {
            (x, y, _) if x == y => {
                return Err(invalid(
                    span,
                    "SWP destination and address must be different registers",
                ));
            }
            (x, _, z) if x == z => {
                return Err(invalid(
                    span,
                    "SWP destination and stored value must be different registers",
                ));
            }
            (x, y, z) => vec![Emit(Base::Op(MEM, x, y, z))],
        },
        SwpI3(x, y, imm) => match (r(x)?, r(y)?) {
            (x, y) if x == y => {
                return Err(invalid(
                    span,
                    "SWP destination and address must be different registers",
                ));
            }
            (x, y) => vec![Emit(Base::Imm(MEM, x, y, Value::Imm(imm)))],
        },
        // Jumps store to address -1 (the program counter), which A reads as outside A mode
        Jmp(addr) => vec![Emit(Base::Imm(MEM, A, A, Value::Addr(addr)))],
        JmpR(x) => vec![Emit(Base::Op(MEM, A, A, r(x)?))],
        Jal2(x, y) => match (r(x)?, r(y)?) {
            (x, y) if x == y => {
                return Err(invalid(
                    span,
                    "JAL link and target must be different registers",
                ));
            }
            (x, y) => vec![Emit(Base::Op(MEM, x, A, y))],
        },
        JalI2(x, addr) => vec![Emit(Base::Imm(MEM, r(x)?, A, Value::Addr(addr)))],
        Pc1(x) => {
            let x = r(x)?;
            vec![Emit(Base::Op(MEM, x, A, x))]
        }
        Pred(flag) => {
            let f = match flag {
                PredicateFlag::Carry => 0b00,
                PredicateFlag::NotCarry => 0b01,
                PredicateFlag::Zero => 0b10,
                PredicateFlag::NotZero => 0b11,
            };
            vec![Emit(Base::Op(MEM, f, f, f))]
        }
    })
}

fn writes_to_a(op: &Op) -> bool {
    destination(op) == Some(Register(A))
}

/// Register written by the operation. Excludes JAL and PC, which may not use A at all.
fn destination(op: &Op) -> Option<Register> {
    use Op::*;
    match op {
        Add3(x, ..)
        | AddI3(x, ..)
        | Sub3(x, ..)
        | SubI3(x, ..)
        | And3(x, ..)
        | AndI3(x, ..)
        | Mov2(x, _)
        | MovI2(x, _)
        | Ldi2(x, _)
        | Lsi2(x, _)
        | Not2(x, _)
        | Inc1(x)
        | Inc2(x, _)
        | Dec1(x)
        | Dec2(x, _)
        | Mld2(x, _)
        | Swp3(x, ..)
        | SwpI3(x, ..) => Some(*x),
        _ => None,
    }
}

/// Registers read by the operation. Excludes JMP, JAL and PC, which may not use A at all.
fn sources(op: &Op) -> Vec<Register> {
    use Op::*;
    match op {
        Add3(_, y, z) | Sub3(_, y, z) | And3(_, y, z) | Swp3(_, y, z) => vec![*y, *z],
        AddI3(_, y, _) | SubI3(_, y, _) | AndI3(_, y, _) | SwpI3(_, y, _) => vec![*y],
        Mov2(_, y) | Not2(_, y) | Inc2(_, y) | Dec2(_, y) | Mld2(_, y) => vec![*y],
        Inc1(x) | Dec1(x) | MstI2(x, _) => vec![*x],
        Mst2(x, y) => vec![*x, *y],
        _ => Vec::new(),
    }
}

fn uses_a_in_jump(op: &Op) -> bool {
    use Op::*;
    let a = Register(A);
    match op {
        JmpR(x) | JalI2(x, _) | Pc1(x) => *x == a,
        Jal2(x, y) => *x == a || *y == a,
        _ => false,
    }
}

/// Lowers the program, checking the rules around register A and predicates as it goes
fn lower_program(items: &[ParsedItem]) -> (Vec<Lowered>, Vec<AssemblerError>) {
    let mut lowered = Vec::new();
    let mut errors = Vec::new();
    let mut previous: Option<Op> = None;
    let mut previous_span: Option<&Span> = None;

    for item in items {
        let instruction = match item {
            ParsedItem::Label(name, _) => {
                if let (Some(op), Some(span)) = (&previous, previous_span) {
                    if writes_to_a(op) {
                        errors.push(invalid(
                            span,
                            "A is written right before a label, so it can't be read on the next line",
                        ));
                    }
                }
                lowered.push(Lowered::Label(name.clone()));
                previous = None;
                continue;
            }
            ParsedItem::Instruction(instruction) => instruction,
        };
        let span = &instruction.span;
        let decoded = match decode(MNEMONICS, &SYNTAX, instruction) {
            Ok(op) => op,
            Err(error) => {
                errors.push(error);
                continue;
            }
        };
        let op = &decoded;
        let a_mode = previous.as_ref().is_some_and(writes_to_a);
        let reads_a = sources(op).contains(&Register(A));

        if uses_a_in_jump(op) {
            errors.push(invalid(span, "A can't be used in JMP, JAL or PC"));
        }

        if reads_a && !a_mode {
            errors.push(invalid(
                span,
                "A is read here, but not written on the previous line",
            ));
        }

        match lower_fully(op, a_mode, span) {
            Ok(bases) => {
                // Writing A and ignoring it is fine, unless this line relies on A reading -1
                if a_mode && !reads_a && bases.first().is_some_and(Base::depends_on_a_mode) {
                    errors.push(invalid(
                        span,
                        "the previous line writes A, which changes how this line behaves; read A here or write to another register",
                    ));
                }

                if matches!(previous, Some(Op::Pred(_))) && bases.len() != 1 {
                    errors.push(invalid(
                        span,
                        "a predicated line must assemble to a single instruction",
                    ));
                }

                lowered.push(Lowered::Operation {
                    span: span.clone(),
                    machines: bases.into_iter().map(Base::into_machine).collect(),
                });
            }
            Err(error) => errors.push(error),
        }

        previous = Some(decoded);
        previous_span = Some(span);
    }

    (lowered, errors)
}

impl Base {
    fn into_machine(self) -> Machine {
        match self {
            Base::Op(op, x, y, z) => Machine::bytes(vec![instruction(op, x, y, z)]),
            Base::Imm(op, x, y, value) => Machine::new(2, move |resolver| {
                let imm = resolve(resolver, value)?;
                // BYT IIIIII holds the upper 6 bits, the next instruction's last operand the lower 2
                Ok(vec![BYT << 6 | imm >> 2, instruction(op, x, y, imm & 0b11)])
            }),
            Base::Lsi(x, imm) => Machine::new(1, move |resolver| {
                match resolver.immediate(imm)? {
                    // -1 + -1
                    -2 => Ok(vec![instruction(AD, x, A, A)]),
                    // Orthogonality patch: IMM - 1
                    v @ -1..=2 => Ok(vec![instruction(SUB, x, (v + 1) as u8, (v + 1) as u8)]),
                    v => Err(AssemblerError::ImmediateOutOfRange(
                        resolver.span.clone(),
                        v,
                    )),
                }
            }),
        }
    }
}

fn resolve(resolver: &Resolver, value: Value) -> Result<u8, AssemblerError> {
    let (value, negate) = match value {
        Value::Imm(imm) => (resolver.immediate(imm)?, false),
        Value::NegImm(imm) => (resolver.immediate(imm)?, true),
        Value::Addr(addr) => {
            let address = resolver.address(addr)?;
            if !(0..=255).contains(&address) {
                return Err(AssemblerError::AddressOutOfRange(
                    resolver.span.clone(),
                    address,
                ));
            }
            return Ok(address as u8);
        }
    };

    if !(-128..=255).contains(&value) {
        return Err(AssemblerError::ImmediateOutOfRange(
            resolver.span.clone(),
            value,
        ));
    }

    let value = if negate { -value } else { value };
    Ok(value.rem_euclid(256) as u8)
}

fn instruction(op: u8, x: u8, y: u8, z: u8) -> u8 {
    op << 6 | x << 4 | y << 2 | z
}
