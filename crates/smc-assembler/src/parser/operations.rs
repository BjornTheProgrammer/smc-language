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

#[derive(Debug, PartialEq, Clone)]
pub enum SkipFlag {
    Never,
    IfZero,
    IfNotZero,
    IfNegative,
    IfNotNegative,
    Always,
}

#[derive(Debug, PartialEq, Clone)]
pub enum PredicateFlag {
    Carry,
    NotCarry,
    Zero,
    NotZero,
}

#[derive(Debug, PartialEq, Clone)]
pub enum OperationWithArgs {
    Add3(Register, Register, Register),
    AddI3(Register, Register, Immediate),
    Add2(Register, Register),

    Sub3(Register, Register, Register),
    SubI3(Register, Register, Immediate),
    Sub2(Register, Register),

    Nor3(Register, Register, Register),

    And3(Register, Register, Register),
    AndI3(Register, Register, Immediate),
    And2(Register, Register),

    Xor3(Register, Register, Register),
    Xor2(Register, Register),

    Or2(Register, Register),
    Cmp2(Register, Register),
    Cpy2(Register, Register),
    Adc2(Register, Register),
    Mld2(Register, Register),
    Mst2(Register, Register),
    MstI2(Register, Immediate),
    Pld2(Register, Register),
    Pst2(Register, Register),
    Mov2(Register, Register),
    MovI2(Register, Immediate),
    Lsh2(Register, Register),
    Not2(Register, Register),
    Neg2(Register, Register),

    Rsh2(Register, Register),
    Rsh1(Register),

    Inv1(Register),
    Inc2(Register, Register),
    Inc1(Register),
    Dec2(Register, Register),
    Dec1(Register),
    Clr1(Register),

    Ldi2(Register, Immediate),
    Adi2(Register, Immediate),
    Cpi2(Register, Immediate),
    Ani2(Register, Immediate),
    Lsi2(Register, Immediate),

    Swp3(Register, Register, Register),
    SwpI3(Register, Register, Immediate),

    Jmp(Address),
    JmpR(Register),
    Jal2(Register, Register),
    JalI2(Register, Address),
    Pc1(Register),
    Cal(Address),

    Pred(PredicateFlag),

    Nop,
    Hlt,
    Bkl,
    Bkr,
    Ret,

    Skp(SkipFlag),

    Brh(Condition, Address),
    Lod(Register, Register, Option<Offset>),
    Str(Register, Register, Option<Offset>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct SpannedOperation {
    pub op: OperationWithArgs,
    pub span: Span,
}

impl SpannedOperation {
    pub fn new(op: OperationWithArgs, span: Span) -> Self {
        Self { op, span }
    }
}
