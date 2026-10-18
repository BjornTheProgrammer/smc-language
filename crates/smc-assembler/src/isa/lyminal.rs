//! Lyminal's encoding: every instruction is one byte, `OO XX YY ZZ`.
//!
//! A `BYT` prefix in front of an instruction carries the upper 6 bits of an 8 bit
//! immediate, and the lower 2 bits take the place of the instruction's Z operand.
//! What instructions do is left to the assembler and emulators.

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum Opcode {
    /// A_D: adds, or ands when the first source has the higher address
    AddAnd = 0b00,
    Sub = 0b01,
    Mem = 0b10,
    /// Prefix carrying the upper bits of the next instruction's immediate
    Byt = 0b11,
}

impl Opcode {
    pub const fn decode(byte: u8) -> Self {
        match byte >> 6 {
            0b00 => Opcode::AddAnd,
            0b01 => Opcode::Sub,
            0b10 => Opcode::Mem,
            _ => Opcode::Byt,
        }
    }
}

pub const REGISTER_COUNT: u8 = 4;

// Register addresses
pub const X: u8 = 0b00;
pub const Y: u8 = 0b01;
pub const Z: u8 = 0b10;
/// Reads -1 unless the previous instruction wrote it ("A mode")
pub const A: u8 = 0b11;

/// One machine instruction. Operands are 2 bit register addresses.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Instruction {
    pub opcode: Opcode,
    pub x: u8,
    pub y: u8,
    pub z: u8,
}

impl Instruction {
    pub const fn new(opcode: Opcode, x: u8, y: u8, z: u8) -> Self {
        debug_assert!(x < 4 && y < 4 && z < 4, "operands are 2 bits");
        Self { opcode, x, y, z }
    }

    pub const fn decode(byte: u8) -> Self {
        Self {
            opcode: Opcode::decode(byte),
            x: (byte >> 4) & 0b11,
            y: (byte >> 2) & 0b11,
            z: byte & 0b11,
        }
    }

    pub const fn encode(self) -> u8 {
        (self.opcode as u8) << 6 | self.x << 4 | self.y << 2 | self.z
    }
}

/// Splits an 8 bit immediate into its `BYT` prefix and the 2 bits that replace Z
pub const fn split_immediate(immediate: u8) -> (u8, u8) {
    ((Opcode::Byt as u8) << 6 | immediate >> 2, immediate & 0b11)
}

/// The immediate made by a `BYT` prefix and the Z operand of the instruction after it
pub const fn join_immediate(prefix: u8, z: u8) -> u8 {
    (prefix & 0b11_1111) << 2 | z
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fields_are_laid_out_oo_xx_yy_zz() {
        // JMP X: store X to the program counter, addressed through A
        assert_eq!(
            Instruction::new(Opcode::Mem, A, A, X).encode(),
            0b10_11_11_00
        );
        assert_eq!(
            Instruction::decode(0b01_00_01_10),
            Instruction::new(Opcode::Sub, X, Y, Z)
        );
    }

    #[test]
    fn every_byte_round_trips() {
        for byte in 0..=u8::MAX {
            assert_eq!(Instruction::decode(byte).encode(), byte);
        }
    }

    #[test]
    fn every_immediate_round_trips() {
        for immediate in 0..=u8::MAX {
            let (prefix, z) = split_immediate(immediate);
            assert_eq!(Opcode::decode(prefix), Opcode::Byt);
            assert_eq!(join_immediate(prefix, z), immediate);
        }
    }
}
