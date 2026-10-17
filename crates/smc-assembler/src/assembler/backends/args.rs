//! Decoding generic instructions into a target's own operations, through a mnemonic table.

use std::cell::Cell;

use crate::{
    assembler::{AssemblerError, backends::Register},
    lexer::token::Span,
    parser::operations::{Address, Immediate, Instruction, Offset, Operand, SpannedOperand},
};

/// Decodes one mnemonic's operands into an operation
pub type Decoder<Op> = fn(&Args) -> Result<Op, AssemblerError>;

/// A target's mnemonics, each with the decoder for its operands
pub type MnemonicTable<Op> = [(&'static str, Decoder<Op>)];

/// How a target reads operands whose meaning differs between targets
pub struct Syntax {
    /// The register an operand names, if any
    pub register: fn(&Operand) -> Option<Register>,
    /// The value of a character literal, if the target can encode that character
    pub character: fn(char) -> Option<i128>,
}

/// Registers written `r0`, `r1`, ...
pub fn numbered_register(operand: &Operand) -> Option<Register> {
    let Operand::Name(name) = operand else {
        return None;
    };
    let digits = name.strip_prefix(['r', 'R'])?;
    if digits.is_empty() || !digits.bytes().all(|b| b.is_ascii_digit()) {
        return None;
    }
    digits.parse().ok().map(Register)
}

/// Characters as their ASCII codes
pub fn ascii(character: char) -> Option<i128> {
    character.is_ascii().then_some(character as i128)
}

/// Decodes an instruction with the table entry for its mnemonic, rejecting leftover operands
pub fn decode<Op>(
    table: &MnemonicTable<Op>,
    syntax: &Syntax,
    instruction: &Instruction,
) -> Result<Op, AssemblerError> {
    let (_, decoder) = table
        .iter()
        .find(|(name, _)| name.eq_ignore_ascii_case(&instruction.mnemonic))
        .ok_or_else(|| {
            AssemblerError::UnsupportedOperation(
                instruction.span.clone(),
                instruction.mnemonic.clone(),
            )
        })?;

    let args = Args {
        instruction,
        syntax,
        used: Cell::new(0),
    };
    let op = decoder(&args)?;

    if let Some(extra) = instruction.operands.get(args.used.get()) {
        return Err(AssemblerError::InvalidOperands(
            extra.span.clone(),
            format!("Unexpected operand `{}`", extra.operand),
        ));
    }

    Ok(op)
}

/// An instruction's operands, read by index. Any operand a decoder doesn't read is
/// reported as unexpected.
pub struct Args<'a> {
    instruction: &'a Instruction,
    syntax: &'a Syntax,
    used: Cell<usize>,
}

impl Args<'_> {
    pub fn len(&self) -> usize {
        self.instruction.operands.len()
    }

    pub fn span(&self) -> &Span {
        &self.instruction.span
    }

    fn get(&self, index: usize, expected: &str) -> Result<&SpannedOperand, AssemblerError> {
        self.used.set(self.used.get().max(index + 1));
        self.instruction.operands.get(index).ok_or_else(|| {
            AssemblerError::InvalidOperands(
                self.instruction.span.clone(),
                format!("Expected {expected} as operand {}", index + 1),
            )
        })
    }

    fn mismatch(&self, operand: &SpannedOperand, expected: &str) -> AssemblerError {
        AssemblerError::InvalidOperands(
            operand.span.clone(),
            format!("Expected {expected}, but received `{}`", operand.operand),
        )
    }

    /// Whether operand `index` exists and names a register, without reading it
    pub fn is_register(&self, index: usize) -> bool {
        self.instruction
            .operands
            .get(index)
            .is_some_and(|operand| (self.syntax.register)(&operand.operand).is_some())
    }

    pub fn register(&self, index: usize) -> Result<Register, AssemblerError> {
        let operand = self.get(index, "register")?;
        (self.syntax.register)(&operand.operand).ok_or_else(|| self.mismatch(operand, "register"))
    }

    /// A register used as an address, `[r1]`. The brackets are optional.
    pub fn memory_register(&self, index: usize) -> Result<Register, AssemblerError> {
        let operand = self.get(index, "memory register")?;
        let inner = match &operand.operand {
            Operand::Memory(inner) => inner,
            _ => operand,
        };
        (self.syntax.register)(&inner.operand)
            .ok_or_else(|| self.mismatch(inner, "memory register"))
    }

    pub fn immediate(&self, index: usize) -> Result<Immediate, AssemblerError> {
        let operand = self.get(index, "immediate")?;
        match &operand.operand {
            Operand::Number(n) => Ok(Immediate::Value(*n)),
            Operand::Char(c) => (self.syntax.character)(*c)
                .map(Immediate::Value)
                .ok_or_else(|| {
                    AssemblerError::InvalidOperands(
                        operand.span.clone(),
                        format!("Unsupported character {c:?}"),
                    )
                }),
            Operand::Name(name) => Ok(Immediate::Define(name.clone())),
            _ => Err(self.mismatch(operand, "immediate (number, character, or define)")),
        }
    }

    pub fn address(&self, index: usize) -> Result<Address, AssemblerError> {
        let operand = self.get(index, "address")?;
        match &operand.operand {
            Operand::Number(n) => Ok(Address::Value(*n)),
            Operand::Name(name) => Ok(Address::Define(name.clone())),
            Operand::Label(name) => Ok(Address::Label(name.clone())),
            _ => Err(self.mismatch(operand, "address (number, label, or define)")),
        }
    }

    pub fn offset(&self, index: usize) -> Result<Offset, AssemblerError> {
        let operand = self.get(index, "offset")?;
        match &operand.operand {
            Operand::Number(n) => Ok(Offset::Value(*n)),
            Operand::Name(name) => Ok(Offset::Define(name.clone())),
            _ => Err(self.mismatch(operand, "offset (number or define)")),
        }
    }

    /// The operand as written, for targets with operand kinds of their own
    pub fn operand(&self, index: usize) -> Result<&SpannedOperand, AssemblerError> {
        self.get(index, "operand")
    }
}
