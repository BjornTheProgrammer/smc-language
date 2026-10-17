//! Lyminal: a 1 byte, 3 operand ISA with four opcodes (A_D, SUB, MEM, BYT).
//!
//! Every assembly instruction is lowered to one or more machine instructions of the form
//! `OO XX YY ZZ`, optionally preceded by a `BYT` prefix that turns the last operand into an
//! 8 bit immediate. Register A reads -1 unless the previous instruction wrote it ("A mode"),
//! which the lowering uses for moves, increments, jumps and so on.

use crate::{
    assembler::{
        AssemblerError, LabelMap, backends::Target, get_address_value, get_immediate_value,
    },
    lexer::token::{Register, Span},
    parser::{
        DefineMap, ParsedItem,
        operations::{Address, Immediate, OperationWithArgs, PredicateFlag},
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

    fn validate(&self, items: &[ParsedItem]) -> Vec<AssemblerError> {
        validate(items)
    }

    fn instruction_byte_size(
        &self,
        op: &OperationWithArgs,
        previous: Option<&OperationWithArgs>,
    ) -> usize {
        instruction_byte_size(op, previous)
    }

    fn assemble_operation(
        &self,
        defines: &DefineMap,
        labels: &LabelMap,
        op: OperationWithArgs,
        previous: Option<&OperationWithArgs>,
        span: Span,
    ) -> Result<Vec<u8>, AssemblerError> {
        assemble_operation(defines, labels, op, previous, span)
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
    fn byte_size(&self) -> usize {
        match self {
            Base::Op(..) | Base::Lsi(..) => 1,
            Base::Imm(..) => 2,
        }
    }

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
    Lower(OperationWithArgs),
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
fn lower_fully(
    op: &OperationWithArgs,
    a_mode: bool,
    span: &Span,
) -> Result<Vec<Base>, AssemblerError> {
    let mut out = Vec::new();
    expand(op, a_mode, span, &mut out, 0)?;
    Ok(out)
}

fn expand(
    op: &OperationWithArgs,
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
fn lower(op: &OperationWithArgs, a_mode: bool, span: &Span) -> Result<Vec<Step>, AssemblerError> {
    use OperationWithArgs::*;
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
        _ => {
            return Err(AssemblerError::UnsupportedOperation(
                span.clone(),
                op.clone(),
            ));
        }
    })
}

fn writes_to_a(op: &OperationWithArgs) -> bool {
    destination(op) == Some(Register(A))
}

/// Register written by the operation. Excludes JAL and PC, which may not use A at all.
fn destination(op: &OperationWithArgs) -> Option<Register> {
    use OperationWithArgs::*;
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
fn sources(op: &OperationWithArgs) -> Vec<Register> {
    use OperationWithArgs::*;
    match op {
        Add3(_, y, z) | Sub3(_, y, z) | And3(_, y, z) | Swp3(_, y, z) => vec![*y, *z],
        AddI3(_, y, _) | SubI3(_, y, _) | AndI3(_, y, _) | SwpI3(_, y, _) => vec![*y],
        Mov2(_, y) | Not2(_, y) | Inc2(_, y) | Dec2(_, y) | Mld2(_, y) => vec![*y],
        Inc1(x) | Dec1(x) | MstI2(x, _) => vec![*x],
        Mst2(x, y) => vec![*x, *y],
        _ => Vec::new(),
    }
}

fn uses_a_in_jump(op: &OperationWithArgs) -> bool {
    use OperationWithArgs::*;
    let a = Register(A);
    match op {
        JmpR(x) | JalI2(x, _) | Pc1(x) => *x == a,
        Jal2(x, y) => *x == a || *y == a,
        _ => false,
    }
}

/// Checks the rules around register A and predicates, which span several instructions
fn validate(items: &[ParsedItem]) -> Vec<AssemblerError> {
    let mut errors = Vec::new();
    let mut previous: Option<&OperationWithArgs> = None;
    let mut previous_span: Option<&Span> = None;

    for item in items {
        let spanned = match item {
            ParsedItem::Label(_, _) => {
                if let (Some(op), Some(span)) = (previous, previous_span) {
                    if writes_to_a(op) {
                        errors.push(invalid(span, "A is written right before a label, so it can't be read on the next line"));
                    }
                }
                previous = None;
                continue;
            }
            ParsedItem::Operation(spanned) => spanned,
        };
        let (op, span) = (&spanned.op, &spanned.span);
        let a_mode = previous.is_some_and(writes_to_a);
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

        if let Ok(bases) = lower_fully(op, a_mode, span) {
            // Writing A and ignoring it is fine, unless this line relies on A reading -1
            if a_mode && !reads_a && bases.first().is_some_and(Base::depends_on_a_mode) {
                errors.push(invalid(span, "the previous line writes A, which changes how this line behaves; read A here or write to another register"));
            }

            if matches!(previous, Some(OperationWithArgs::Pred(_))) && bases.len() != 1 {
                errors.push(invalid(
                    span,
                    "a predicated line must assemble to a single instruction",
                ));
            }
        }

        previous = Some(op);
        previous_span = Some(span);
    }

    errors
}

fn resolve(
    span: &Span,
    defines: &DefineMap,
    labels: &LabelMap,
    value: Value,
) -> Result<u8, AssemblerError> {
    let (value, negate) = match value {
        Value::Imm(imm) => (get_immediate_value(span, defines, imm)?, false),
        Value::NegImm(imm) => (get_immediate_value(span, defines, imm)?, true),
        Value::Addr(addr) => {
            let address = get_address_value(span, defines, labels, addr)?;
            if !(0..=255).contains(&address) {
                return Err(AssemblerError::AddressOutOfRange(span.clone(), address));
            }
            return Ok(address as u8);
        }
    };

    if !(-128..=255).contains(&value) {
        return Err(AssemblerError::ImmediateOutOfRange(span.clone(), value));
    }

    let value = if negate { -value } else { value };
    Ok(value.rem_euclid(256) as u8)
}

fn instruction(op: u8, x: u8, y: u8, z: u8) -> u8 {
    op << 6 | x << 4 | y << 2 | z
}

fn assemble_operation(
    defines: &DefineMap,
    labels: &LabelMap,
    operation: OperationWithArgs,
    previous: Option<&OperationWithArgs>,
    span: Span,
) -> Result<Vec<u8>, AssemblerError> {
    let a_mode = previous.is_some_and(writes_to_a);
    let mut result = Vec::new();

    for base in lower_fully(&operation, a_mode, &span)? {
        match base {
            Base::Op(op, x, y, z) => result.push(instruction(op, x, y, z)),
            Base::Imm(op, x, y, value) => {
                let imm = resolve(&span, defines, labels, value)?;
                // BYT IIIIII holds the upper 6 bits, the next instruction's last operand the lower 2
                result.push(BYT << 6 | imm >> 2);
                result.push(instruction(op, x, y, imm & 0b11));
            }
            Base::Lsi(x, imm) => match get_immediate_value(&span, defines, imm)? {
                // -1 + -1
                -2 => result.push(instruction(AD, x, A, A)),
                // Orthogonality patch: IMM - 1
                v @ -1..=2 => result.push(instruction(SUB, x, (v + 1) as u8, (v + 1) as u8)),
                v => return Err(AssemblerError::ImmediateOutOfRange(span, v)),
            },
        }
    }

    Ok(result)
}

fn instruction_byte_size(op: &OperationWithArgs, previous: Option<&OperationWithArgs>) -> usize {
    let a_mode = previous.is_some_and(writes_to_a);
    // Invalid operations still take a byte here; their error is reported when assembling
    lower_fully(op, a_mode, &Span::new(0, 0))
        .map(|bases| bases.iter().map(Base::byte_size).sum())
        .unwrap_or(1)
}
