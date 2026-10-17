//! Decoding generic instructions into a target's own operations, through a mnemonic table.

use std::cell::Cell;

use crate::{
    assembler::AssemblerError,
    lexer::token::{Condition, Register, Span},
    parser::operations::{Address, Immediate, Instruction, Offset, Operand, SpannedOperand},
};

/// Decodes one mnemonic's operands into an operation
pub type Decoder<Op> = fn(&Args) -> Result<Op, AssemblerError>;

/// A target's mnemonics, each with the decoder for its operands
pub type MnemonicTable<Op> = [(&'static str, Decoder<Op>)];

/// Names a target gives to registers, beyond `r0`, `r1`, ...
pub type RegisterNames = fn(&Operand) -> Option<Register>;

/// Only `rN` registers
pub fn numbered_registers(operand: &Operand) -> Option<Register> {
    match operand {
        Operand::Register(register) => Some(*register),
        _ => None,
    }
}

/// Decodes an instruction with the table entry for its mnemonic, rejecting leftover operands
pub fn decode<Op>(
    table: &MnemonicTable<Op>,
    registers: RegisterNames,
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
        registers,
        used: Cell::new(0),
    };
    let op = decoder(&args)?;

    if let Some(extra) = instruction.operands.get(args.used.get()) {
        return Err(AssemblerError::InvalidOperands(
            extra.span.clone(),
            format!("unexpected operand for {}", instruction.mnemonic),
        ));
    }

    Ok(op)
}

/// An instruction's operands, read by index. Any operand a decoder doesn't read is
/// reported as unexpected.
pub struct Args<'a> {
    instruction: &'a Instruction,
    registers: RegisterNames,
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
                format!(
                    "{} expects {expected} as operand {}",
                    self.instruction.mnemonic,
                    index + 1
                ),
            )
        })
    }

    fn mismatch(&self, operand: &SpannedOperand, expected: &str) -> AssemblerError {
        AssemblerError::InvalidOperands(
            operand.span.clone(),
            format!("expected {expected}, but received {:?}", operand.operand),
        )
    }

    /// Whether operand `index` exists and names a register, without reading it
    pub fn is_register(&self, index: usize) -> bool {
        self.instruction
            .operands
            .get(index)
            .is_some_and(|operand| (self.registers)(&operand.operand).is_some())
    }

    pub fn register(&self, index: usize) -> Result<Register, AssemblerError> {
        let operand = self.get(index, "a register")?;
        (self.registers)(&operand.operand).ok_or_else(|| self.mismatch(operand, "a register"))
    }

    /// A register used as an address, `[r1]`. The brackets are optional.
    pub fn memory_register(&self, index: usize) -> Result<Register, AssemblerError> {
        let operand = self.get(index, "a memory register")?;
        let inner = match &operand.operand {
            Operand::Memory(inner) => inner,
            _ => operand,
        };
        (self.registers)(&inner.operand).ok_or_else(|| self.mismatch(inner, "a memory register"))
    }

    pub fn immediate(&self, index: usize) -> Result<Immediate, AssemblerError> {
        let operand = self.get(index, "an immediate")?;
        match &operand.operand {
            Operand::Number(n) => Ok(Immediate::Value(*n)),
            Operand::Name(name) => Ok(Immediate::Define(name.clone())),
            _ => Err(self.mismatch(operand, "an immediate (number or define)")),
        }
    }

    pub fn address(&self, index: usize) -> Result<Address, AssemblerError> {
        let operand = self.get(index, "an address")?;
        match &operand.operand {
            Operand::Number(n) => Ok(Address::Value(*n)),
            Operand::Name(name) => Ok(Address::Define(name.clone())),
            Operand::Label(name) => Ok(Address::Label(name.clone())),
            _ => Err(self.mismatch(operand, "an address (number, label, or define)")),
        }
    }

    pub fn offset(&self, index: usize) -> Result<Offset, AssemblerError> {
        let operand = self.get(index, "an offset")?;
        match &operand.operand {
            Operand::Number(n) => Ok(Offset::Value(*n)),
            Operand::Name(name) => Ok(Offset::Define(name.clone())),
            _ => Err(self.mismatch(operand, "an offset (number or define)")),
        }
    }

    pub fn condition(&self, index: usize) -> Result<Condition, AssemblerError> {
        let operand = self.get(index, "a condition")?;
        match &operand.operand {
            Operand::Condition(condition) => Ok(condition.clone()),
            _ => Err(self.mismatch(operand, "a condition")),
        }
    }

    /// The operand as written, for targets with operand kinds of their own
    pub fn operand(&self, index: usize) -> Result<&SpannedOperand, AssemblerError> {
        self.get(index, "an operand")
    }
}
