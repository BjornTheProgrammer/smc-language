//! BatPU-2's encoding: every instruction is a 16 bit word, `OOOO AAAA BBBB CCCC`.
//!
//! Fields are named by where they sit, not by what the instruction does with them.

pub const REGISTER_COUNT: u8 = 16;

/// Branch conditions, by the flags they test
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum Condition {
    /// Zero set
    Equal = 0b00,
    /// Zero clear
    NotEqual = 0b01,
    /// Carry set
    GreaterEqual = 0b10,
    /// Carry clear
    Less = 0b11,
}

impl Condition {
    const fn decode(bits: u16) -> Self {
        match bits & 0b11 {
            0b00 => Condition::Equal,
            0b01 => Condition::NotEqual,
            0b10 => Condition::GreaterEqual,
            _ => Condition::Less,
        }
    }
}

/// One instruction. Registers are 4 bits, addresses 10 and offsets -8..=7.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Instruction {
    Nop,
    Hlt,
    Add { a: u8, b: u8, c: u8 },
    Sub { a: u8, b: u8, c: u8 },
    Nor { a: u8, b: u8, c: u8 },
    And { a: u8, b: u8, c: u8 },
    Xor { a: u8, b: u8, c: u8 },
    Rsh { a: u8, c: u8 },
    Ldi { a: u8, immediate: u8 },
    Adi { a: u8, immediate: u8 },
    Jmp { address: u16 },
    Brh { condition: Condition, address: u16 },
    Cal { address: u16 },
    Ret,
    Lod { a: u8, b: u8, offset: i8 },
    Str { a: u8, b: u8, offset: i8 },
}

const fn registers(opcode: u16, a: u8, b: u8, c: u8) -> u16 {
    debug_assert!(a < 16 && b < 16 && c < 16, "registers are 4 bits");
    opcode << 12 | (a as u16) << 8 | (b as u16) << 4 | c as u16
}

const fn immediate(opcode: u16, a: u8, immediate: u8) -> u16 {
    debug_assert!(a < 16, "registers are 4 bits");
    opcode << 12 | (a as u16) << 8 | immediate as u16
}

const fn address(opcode: u16, address: u16) -> u16 {
    debug_assert!(address < 1024, "addresses are 10 bits");
    opcode << 12 | address
}

const fn offset(opcode: u16, a: u8, b: u8, offset: i8) -> u16 {
    debug_assert!(-8 <= offset && offset < 8, "offsets are 4 bits");
    registers(opcode, a, b, offset as u8 & 0xF)
}

impl Instruction {
    pub const fn encode(self) -> u16 {
        match self {
            Instruction::Nop => 0b0000 << 12,
            Instruction::Hlt => 0b0001 << 12,
            Instruction::Add { a, b, c } => registers(0b0010, a, b, c),
            Instruction::Sub { a, b, c } => registers(0b0011, a, b, c),
            Instruction::Nor { a, b, c } => registers(0b0100, a, b, c),
            Instruction::And { a, b, c } => registers(0b0101, a, b, c),
            Instruction::Xor { a, b, c } => registers(0b0110, a, b, c),
            Instruction::Rsh { a, c } => registers(0b0111, a, 0, c),
            Instruction::Ldi { a, immediate: i } => immediate(0b1000, a, i),
            Instruction::Adi { a, immediate: i } => immediate(0b1001, a, i),
            Instruction::Jmp { address: addr } => address(0b1010, addr),
            Instruction::Brh {
                condition,
                address: addr,
            } => address(0b1011, addr) | (condition as u16) << 10,
            Instruction::Cal { address: addr } => address(0b1100, addr),
            Instruction::Ret => 0b1101 << 12,
            Instruction::Lod { a, b, offset: o } => offset(0b1110, a, b, o),
            Instruction::Str { a, b, offset: o } => offset(0b1111, a, b, o),
        }
    }

    /// Every word decodes to something; bits an instruction doesn't use are ignored
    pub const fn decode(word: u16) -> Self {
        let a = (word >> 8 & 0xF) as u8;
        let b = (word >> 4 & 0xF) as u8;
        let c = (word & 0xF) as u8;
        let immediate = word as u8;
        let address = word & 0x3FF;
        // Sign extends the 4 bit offset
        let offset = (c << 4) as i8 >> 4;

        match word >> 12 {
            0b0000 => Instruction::Nop,
            0b0001 => Instruction::Hlt,
            0b0010 => Instruction::Add { a, b, c },
            0b0011 => Instruction::Sub { a, b, c },
            0b0100 => Instruction::Nor { a, b, c },
            0b0101 => Instruction::And { a, b, c },
            0b0110 => Instruction::Xor { a, b, c },
            0b0111 => Instruction::Rsh { a, c },
            0b1000 => Instruction::Ldi { a, immediate },
            0b1001 => Instruction::Adi { a, immediate },
            0b1010 => Instruction::Jmp { address },
            0b1011 => Instruction::Brh {
                condition: Condition::decode(word >> 10),
                address,
            },
            0b1100 => Instruction::Cal { address },
            0b1101 => Instruction::Ret,
            0b1110 => Instruction::Lod { a, b, offset },
            _ => Instruction::Str { a, b, offset },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fields_are_laid_out_by_position() {
        let add = Instruction::Add { a: 1, b: 2, c: 3 };
        assert_eq!(add.encode(), 0x2123);
        let brh = Instruction::Brh {
            condition: Condition::GreaterEqual,
            address: 0x3FF,
        };
        assert_eq!(brh.encode(), 0b1011_10_11_1111_1111);
        let lod = Instruction::Lod {
            a: 4,
            b: 5,
            offset: -1,
        };
        assert_eq!(lod.encode(), 0xE45F);
        assert_eq!(Instruction::decode(0xE45F), lod);
    }

    #[test]
    fn every_word_round_trips_through_its_instruction() {
        for word in 0..=u16::MAX {
            let instruction = Instruction::decode(word);
            assert_eq!(Instruction::decode(instruction.encode()), instruction);
        }
    }
}
