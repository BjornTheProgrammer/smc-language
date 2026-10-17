//! Range checking values against the instruction fields they're encoded into.

use std::ops::RangeInclusive;

use crate::{assembler::AssemblerError, lexer::token::Span};

/// An instruction field of some number of bits, which decides which values fit
#[derive(Debug, Clone, Copy)]
pub enum Field {
    /// Unsigned values, such as a 10 bit address: 0..=1023
    Unsigned(u32),
    /// Two's complement values, such as a 4 bit offset: -8..=7
    Signed(u32),
    /// Signed or unsigned values, such as an 8 bit immediate: -128..=255
    AnySign(u32),
}

impl Field {
    fn bits(self) -> u32 {
        match self {
            Field::Unsigned(bits) | Field::Signed(bits) | Field::AnySign(bits) => bits,
        }
    }

    pub fn range(self) -> RangeInclusive<i128> {
        let bits = self.bits();
        match self {
            Field::Unsigned(_) => 0..=(1 << bits) - 1,
            Field::Signed(_) => -(1 << (bits - 1))..=(1 << (bits - 1)) - 1,
            Field::AnySign(_) => -(1 << (bits - 1))..=(1 << bits) - 1,
        }
    }

    /// The field's bits for `value`, or `None` if it doesn't fit
    pub fn encode(self, value: i128) -> Option<u32> {
        self.range()
            .contains(&value)
            .then(|| value.rem_euclid(1 << self.bits()) as u32)
    }

    pub fn immediate(self, value: i128, span: &Span) -> Result<u32, AssemblerError> {
        self.encode(value)
            .ok_or_else(|| AssemblerError::ImmediateOutOfRange(span.clone(), value))
    }

    pub fn address(self, value: i128, span: &Span) -> Result<u32, AssemblerError> {
        self.encode(value)
            .ok_or_else(|| AssemblerError::AddressOutOfRange(span.clone(), value))
    }

    pub fn offset(self, value: i128, span: &Span) -> Result<u32, AssemblerError> {
        self.encode(value)
            .ok_or_else(|| AssemblerError::OffsetOutOfRange(span.clone(), value))
    }
}

#[cfg(test)]
mod tests {
    use super::Field;

    #[test]
    fn ranges() {
        assert_eq!(Field::Unsigned(10).range(), 0..=1023);
        assert_eq!(Field::Signed(4).range(), -8..=7);
        assert_eq!(Field::AnySign(8).range(), -128..=255);
    }

    #[test]
    fn encodes_twos_complement() {
        assert_eq!(Field::Signed(4).encode(-1), Some(0b1111));
        assert_eq!(Field::AnySign(8).encode(-128), Some(0x80));
        assert_eq!(Field::AnySign(8).encode(255), Some(0xFF));
        assert_eq!(Field::Unsigned(10).encode(1024), None);
        assert_eq!(Field::Signed(4).encode(8), None);
    }
}
