use crate::lexer::token::{Condition, Register, Span};
use arbitrary_int::{u4, u10};

#[derive(Debug, PartialEq, Clone)]
pub enum Immediate {
    Value(i8),
    Define(String),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Address {
    Value(u10),
    Define(String),
    Label(String),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Offset {
    Value(u4),
    Define(String),
}

#[derive(Debug, PartialEq, Clone)]
pub enum LoweredOperationWithArgs {
    Nop,
    Hlt,
    Add(Register, Register, Register),
    Sub(Register, Register, Register),
    Nor(Register, Register, Register),
    And(Register, Register, Register),
    Xor(Register, Register, Register),
    Rsh(Register, Register),
    Ldi(Register, Immediate),
    Adi(Register, Immediate),
    Jmp(Address),
    Brh(Condition, Address),
    Cal(Address),
    Ret,
    Lod(Register, Register, Option<Offset>),
    Str(Register, Register, Option<Offset>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum PseudoOperationWithArgs {
    Cmp(Register, Register), // CMP A B -> SUB A B r0
    Mov(Register, Register), // MOV A C -> ADD A r0 C
    Lsh(Register, Register), // LSH A C -> ADD A A C
    Inc(Register),           // INC A -> ADI A 1
    Dec(Register),           // DEC A -> ADI A -1
    Not(Register, Register), // NOT A C -> NOR A r0 C
    Neg(Register, Register), // NEG A C -> SUB r0 A C
}

#[derive(Debug, PartialEq, Clone)]
pub enum OperationWithArgs {
    Lowered(LoweredOperationWithArgs, Span),
    Pseudo(PseudoOperationWithArgs, Span),
}
