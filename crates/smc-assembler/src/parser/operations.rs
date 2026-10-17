use crate::lexer::token::{Condition, Register, Span};

#[derive(Debug, PartialEq, Clone)]
pub enum Immediate {
    Value(i128),
    Define(String),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Address {
    Value(i128),
    Define(String),
    Label(String),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Offset {
    Value(i128),
    Define(String),
}

/// An instruction operand, before a target gives it meaning. A `Name` can be a define or
/// a register name, depending on the target.
#[derive(Debug, PartialEq, Clone)]
pub enum Operand {
    Register(Register),
    Number(i128),
    Name(String),
    Label(String),
    Condition(Condition),
    /// A bracketed operand, `[r1]`
    Memory(Box<SpannedOperand>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct SpannedOperand {
    pub operand: Operand,
    pub span: Span,
}

/// A mnemonic and its operands, as written. Targets decode these into their own operations.
#[derive(Debug, PartialEq, Clone)]
pub struct Instruction {
    pub mnemonic: String,
    pub operands: Vec<SpannedOperand>,
    pub span: Span,
}
