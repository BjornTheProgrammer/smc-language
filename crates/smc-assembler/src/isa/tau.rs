//! Tau's encoding: instructions are one byte, `OOOO DDSS`, or two when they carry an
//! immediate or address.
//!
//! The top nibble picks the format: `0000..=1011` take two registers, `1100` one register,
//! `1101` a register and an immediate byte, `1110` a 10 bit address, and `1111` nothing.

pub const REGISTER_COUNT: u8 = 4;

/// When `SKP` skips the next instruction
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum Skip {
    Never = 0b000,
    IfZero = 0b001,
    IfNotZero = 0b010,
    IfNegative = 0b011,
    IfNotNegative = 0b100,
}

/// One instruction. Registers are 2 bits and addresses 10.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Instruction {
    Add { d: u8, s: u8 },
    Sub { d: u8, s: u8 },
    Xor { d: u8, s: u8 },
    And { d: u8, s: u8 },
    Or { d: u8, s: u8 },
    Cmp { d: u8, s: u8 },
    Cpy { d: u8, s: u8 },
    Adc { d: u8, s: u8 },
    Mld { d: u8, s: u8 },
    Mst { d: u8, s: u8 },
    Pld { d: u8, s: u8 },
    Pst { d: u8, s: u8 },
    Rsh { d: u8 },
    Inv { d: u8 },
    Inc { d: u8 },
    Dec { d: u8 },
    Ldi { d: u8, immediate: u8 },
    Adi { d: u8, immediate: u8 },
    Cpi { d: u8, immediate: u8 },
    Ani { d: u8, immediate: u8 },
    Jmp { address: u16 },
    Cal { address: u16 },
    Bkl,
    Bkr,
    Hlt,
    Ret,
    Skp(Skip),
}

const TWO_REGISTER: [fn(u8, u8) -> Instruction; 12] = [
    |d, s| Instruction::Add { d, s },
    |d, s| Instruction::Sub { d, s },
    |d, s| Instruction::Xor { d, s },
    |d, s| Instruction::And { d, s },
    |d, s| Instruction::Or { d, s },
    |d, s| Instruction::Cmp { d, s },
    |d, s| Instruction::Cpy { d, s },
    |d, s| Instruction::Adc { d, s },
    |d, s| Instruction::Mld { d, s },
    |d, s| Instruction::Mst { d, s },
    |d, s| Instruction::Pld { d, s },
    |d, s| Instruction::Pst { d, s },
];

fn byte(format: u8, d: u8, low: u8) -> u8 {
    debug_assert!(d < 4 && low < 4, "fields are 2 bits");
    format << 4 | d << 2 | low
}

fn immediate(operation: u8, d: u8, immediate: u8) -> Vec<u8> {
    vec![byte(0b1101, d, operation), immediate]
}

fn branch(operation: u8, address: u16) -> Vec<u8> {
    debug_assert!(address < 1024, "addresses are 10 bits");
    vec![byte(0b1110, (address >> 8) as u8, operation), address as u8]
}

impl Instruction {
    /// Bytes taken by the instruction starting with `first`
    pub const fn size(first: u8) -> usize {
        match first >> 4 {
            0b1101 | 0b1110 => 2,
            _ => 1,
        }
    }

    pub fn encode(self) -> Vec<u8> {
        use Instruction::*;
        match self {
            Add { d, s } => vec![byte(0b0000, d, s)],
            Sub { d, s } => vec![byte(0b0001, d, s)],
            Xor { d, s } => vec![byte(0b0010, d, s)],
            And { d, s } => vec![byte(0b0011, d, s)],
            Or { d, s } => vec![byte(0b0100, d, s)],
            Cmp { d, s } => vec![byte(0b0101, d, s)],
            Cpy { d, s } => vec![byte(0b0110, d, s)],
            Adc { d, s } => vec![byte(0b0111, d, s)],
            Mld { d, s } => vec![byte(0b1000, d, s)],
            Mst { d, s } => vec![byte(0b1001, d, s)],
            Pld { d, s } => vec![byte(0b1010, d, s)],
            Pst { d, s } => vec![byte(0b1011, d, s)],
            Rsh { d } => vec![byte(0b1100, d, 0b00)],
            Inv { d } => vec![byte(0b1100, d, 0b01)],
            Inc { d } => vec![byte(0b1100, d, 0b10)],
            Dec { d } => vec![byte(0b1100, d, 0b11)],
            Ldi { d, immediate: i } => immediate(0b00, d, i),
            Adi { d, immediate: i } => immediate(0b01, d, i),
            Cpi { d, immediate: i } => immediate(0b10, d, i),
            Ani { d, immediate: i } => immediate(0b11, d, i),
            Jmp { address } => branch(0b00, address),
            Cal { address } => branch(0b01, address),
            Bkl => vec![0b1111_0000],
            Bkr => vec![0b1111_0001],
            Hlt => vec![0b1111_0010],
            Ret => vec![0b1111_0011],
            Skp(skip) => vec![0b1111_1000 | skip as u8],
        }
    }

    /// Decodes the instruction at the start of `bytes`. `None` for unused encodings, or
    /// when a two byte instruction is cut off.
    pub fn decode(bytes: &[u8]) -> Option<Self> {
        use Instruction::*;
        let first = *bytes.first()?;
        let format = first >> 4;
        let d = first >> 2 & 0b11;
        let low = first & 0b11;
        let second = || bytes.get(1).copied();

        Some(match format {
            0b0000..=0b1011 => TWO_REGISTER[format as usize](d, low),
            0b1100 => [Rsh { d }, Inv { d }, Inc { d }, Dec { d }][low as usize],
            0b1101 => {
                let immediate = second()?;
                [
                    Ldi { d, immediate },
                    Adi { d, immediate },
                    Cpi { d, immediate },
                    Ani { d, immediate },
                ][low as usize]
            }
            0b1110 => {
                let address = (d as u16) << 8 | second()? as u16;
                match low {
                    0b00 => Jmp { address },
                    0b01 => Cal { address },
                    _ => return None,
                }
            }
            _ => match first & 0b1111 {
                0b0000 => Bkl,
                0b0001 => Bkr,
                0b0010 => Hlt,
                0b0011 => Ret,
                0b1000 => Skp(Skip::Never),
                0b1001 => Skp(Skip::IfZero),
                0b1010 => Skp(Skip::IfNotZero),
                0b1011 => Skp(Skip::IfNegative),
                0b1100 => Skp(Skip::IfNotNegative),
                _ => return None,
            },
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn formats_are_laid_out_as_documented() {
        assert_eq!(Instruction::Sub { d: 2, s: 3 }.encode(), [0b0001_10_11]);
        assert_eq!(
            Instruction::Adi {
                d: 1,
                immediate: 0xAB
            }
            .encode(),
            [0b1101_01_01, 0xAB]
        );
        assert_eq!(
            Instruction::Cal { address: 0x2CD }.encode(),
            [0b1110_10_01, 0xCD]
        );
        assert_eq!(
            Instruction::Skp(Skip::IfNotNegative).encode(),
            [0b1111_1_100]
        );
    }

    #[test]
    fn every_used_encoding_round_trips() {
        let mut used = 0;
        for first in 0..=u8::MAX {
            for second in 0..=u8::MAX {
                let bytes = &[first, second][..Instruction::size(first)];
                if let Some(instruction) = Instruction::decode(bytes) {
                    assert_eq!(instruction.encode(), bytes);
                    used += 1;
                }
                if bytes.len() == 1 {
                    break;
                }
            }
        }
        // 12 * 16 + 16 one register, 16 * 256 immediate, 2 * 4 * 256 branch, 9 implicit
        assert_eq!(used, 208 + 4096 + 2048 + 9);
    }

    #[test]
    fn unused_encodings_and_cut_off_instructions_decode_to_nothing() {
        assert_eq!(Instruction::decode(&[0b1110_00_10, 0]), None);
        assert_eq!(Instruction::decode(&[0b1111_0100]), None);
        assert_eq!(Instruction::decode(&[0b1111_1101]), None);
        assert_eq!(Instruction::decode(&[0b1101_00_00]), None);
        assert_eq!(Instruction::decode(&[]), None);
    }
}
