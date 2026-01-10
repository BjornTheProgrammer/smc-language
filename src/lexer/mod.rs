use std::str::FromStr;

use anyhow::Result;
use arbitrary_int::{i4, u10};
use thiserror::Error;

use crate::lexer::token::{
    AddressOrSymbol, Condition, ImmediateOrSymbol, OffsetOrSymbol, Register, Span, Token, TokenSpan,
};

pub mod token;

#[derive(Error, Debug)]
pub enum LexerError {
    #[error("Unknown instruction {1}")]
    UnknownInstruction(Span, String),

    #[error("Invalid number {1}")]
    InvalidNumber(Span, String),

    #[error("Invalid register {1}")]
    InvalidRegister(Span, String),

    #[error("Unexpected character {1}")]
    UnexpectedCharacter(Span, char),

    #[error("Expected character {1}")]
    ExpectedCharacter(Span, char),

    #[error("Unknown condition {1}")]
    UnknownCondition(Span, String),

    #[error("Invalid offset {1}")]
    InvalidOffset(Span, String),
}

#[macro_export]
macro_rules! parse_args {
    // Entry point: parse all args and collect them
    ($self:expr, $($args:tt),+) => {{
        parse_args!(@parse $self, [], $($args),+)
    }};

    // Base case: single argument remaining, return the accumulated tuple
    (@parse $self:expr, [$($acc:expr),*], $last:tt) => {{
        $self.skip_whitespace();
        let val = parse_args!(@read $self, $last)?;
        Ok(($($acc,)* val))
    }};

    // Recursive case: parse one argument, add to accumulator, continue with rest
    (@parse $self:expr, [$($acc:expr),*], $first:tt, $($rest:tt),+) => {{
        $self.skip_whitespace();
        let val = parse_args!(@read $self, $first)?;
        $self.skip_whitespace();
        let _ = $self.skip_char(b',');
        parse_args!(@parse $self, [$($acc,)* val], $($rest),+)
    }};

    // Helper to read based on type
    (@read $self:expr, reg) => {
        $self.read_register()
    };

    (@read $self:expr, imm) => {
        $self.read_immediate_or_symbol()
    };

    (@read $self:expr, address) => {
        $self.read_address_or_symbol()
    };

    (@read $self:expr, condition) => {
        $self.read_condition()
    };

    (@read $self:expr, offset) => {
        $self.read_offset_or_symbol()
    };
}

pub struct Lexer<'a> {
    input: &'a [u8],
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input: input.as_bytes(),
            pos: 0,
        }
    }

    /// Peek at the next byte without consuming
    fn peek(&self) -> Option<u8> {
        self.input.get(self.pos).copied()
    }

    /// Consume and return the next byte
    fn advance(&mut self) -> Option<u8> {
        let res = self.peek();
        if res.is_some() {
            self.pos += 1;
        }
        res
    }

    fn skip_whitespace(&mut self) {
        while let Some(b) = self.peek() {
            if b.is_ascii_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }

    pub fn next_token(&mut self) -> Result<Option<TokenSpan>, LexerError> {
        self.skip_whitespace();
        let start = self.pos;
        let c = match self.advance() {
            Some(c) => c,
            None => return Ok(Some(TokenSpan::new(Token::Eof, Span::new(start, self.pos)))),
        };

        match c {
            b'#' => {
                while let Some(b) = self.peek() {
                    if b != b'\n' {
                        self.advance();
                    } else {
                        break;
                    }
                }
                Ok(None)
            }
            b'.' => {
                let ident = self.read_identifier()?;
                Ok(Some(TokenSpan::new(
                    Token::Label(ident),
                    Span::new(start, self.pos),
                )))
            }
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                while let Some(b) = self.peek() {
                    if b.is_ascii_alphanumeric() || b == b'_' {
                        self.advance();
                    } else {
                        break;
                    }
                }

                let slice = std::str::from_utf8(&self.input[start..self.pos])
                    .unwrap()
                    .to_lowercase();

                let span = Span::new(start, self.pos);

                match slice.as_str() {
                    "nop" => Ok(Some(TokenSpan::new(Token::Nop, span))),
                    "hlt" => Ok(Some(TokenSpan::new(Token::Hlt, span))),
                    "ret" => Ok(Some(TokenSpan::new(Token::Ret, span))),
                    "add" | "sub" | "nor" | "and" | "xor" => {
                        let (r1, r2, r3) = parse_args!(self, reg, reg, reg)?;

                        let span = Span::new(start, self.pos);

                        match slice.as_str() {
                            "add" => Ok(Some(TokenSpan::new(Token::Add(r1, r2, r3), span))),
                            "sub" => Ok(Some(TokenSpan::new(Token::Sub(r1, r2, r3), span))),
                            "nor" => Ok(Some(TokenSpan::new(Token::Nor(r1, r2, r3), span))),
                            "and" => Ok(Some(TokenSpan::new(Token::And(r1, r2, r3), span))),
                            "xor" => Ok(Some(TokenSpan::new(Token::Xor(r1, r2, r3), span))),
                            _ => unreachable!(),
                        }
                    }
                    "rsh" => {
                        let (r1, r2) = parse_args!(self, reg, reg)?;
                        let span = Span::new(start, self.pos);

                        Ok(Some(TokenSpan::new(Token::Rsh(r1, r2), span)))
                    }
                    "ldi" | "adi" => {
                        let (r1, imm1) = parse_args!(self, reg, imm)?;
                        let span = Span::new(start, self.pos);

                        match slice.as_str() {
                            "ldi" => Ok(Some(TokenSpan::new(Token::Ldi(r1, imm1), span))),
                            "adi" => Ok(Some(TokenSpan::new(Token::Adi(r1, imm1), span))),
                            _ => unreachable!(),
                        }
                    }
                    "jmp" | "cal" => {
                        let address = parse_args!(self, address)?;
                        let span = Span::new(start, self.pos);
                        match slice.as_str() {
                            "jmp" => Ok(Some(TokenSpan::new(Token::Jmp(address), span))),
                            "cal" => Ok(Some(TokenSpan::new(Token::Cal(address), span))),
                            _ => unreachable!(),
                        }
                    }
                    "brh" => {
                        let (cond1, addr1) = parse_args!(self, condition, address)?;
                        let span = Span::new(start, self.pos);

                        Ok(Some(TokenSpan::new(Token::Brh(cond1, addr1), span)))
                    }

                    "lod" | "str" => {
                        let (r1, r2) = parse_args!(self, reg, reg)?;

                        // Check if there's a comma (indicating an offset follows)
                        self.skip_whitespace();
                        let offset = if self.peek() == Some(b',') {
                            self.advance(); // consume the comma
                            self.read_offset_or_symbol()?
                        } else {
                            OffsetOrSymbol::NotPresent
                        };

                        let span = Span::new(start, self.pos);

                        match slice.as_str() {
                            "lod" => Ok(Some(TokenSpan::new(Token::Lod(r1, r2, offset), span))),
                            "str" => Ok(Some(TokenSpan::new(Token::Str(r1, r2, offset), span))),
                            _ => unreachable!(),
                        }
                    }

                    // Pseudo Operations
                    "cmp" | "mov" | "lsh" | "not" | "neg" => {
                        let (r1, r2) = parse_args!(self, reg, reg)?;
                        let span = Span::new(start, self.pos);
                        match slice.as_str() {
                            "cmp" => Ok(Some(TokenSpan::new(Token::Cmp(r1, r2), span))),
                            "mov" => Ok(Some(TokenSpan::new(Token::Mov(r1, r2), span))),
                            "lsh" => Ok(Some(TokenSpan::new(Token::Lsh(r1, r2), span))),
                            "not" => Ok(Some(TokenSpan::new(Token::Not(r1, r2), span))),
                            "neg" => Ok(Some(TokenSpan::new(Token::Neg(r1, r2), span))),
                            _ => unreachable!(),
                        }
                    }

                    "inc" | "dec" => {
                        let r1 = parse_args!(self, reg)?;
                        let span = Span::new(start, self.pos);
                        match slice.as_str() {
                            "inc" => Ok(Some(TokenSpan::new(Token::Inc(r1), span))),
                            "dec" => Ok(Some(TokenSpan::new(Token::Dec(r1), span))),
                            _ => unreachable!(),
                        }
                    }

                    "define" => {
                        let ident = self.read_identifier()?;
                        let value = self.read_number()?;
                        let span = Span::new(start, self.pos);
                        Ok(Some(TokenSpan::new(Token::Define(ident, value), span)))
                    }
                    _ => Err(LexerError::UnknownInstruction(
                        Span::new(start, self.pos),
                        slice.to_string(),
                    )),
                }
            }
            _ => Ok(None),
        }
    }

    fn read_identifier(&mut self) -> Result<String, LexerError> {
        self.skip_whitespace();
        let start = self.pos;

        // First character must be a letter or underscore
        match self.peek() {
            Some(b'a'..=b'z') | Some(b'A'..=b'Z') | Some(b'_') => {
                self.advance();
            }
            Some(c) => {
                return Err(LexerError::UnexpectedCharacter(
                    Span::new(start, self.pos),
                    c as char,
                ));
            }
            None => {
                return Err(LexerError::UnexpectedCharacter(
                    Span::new(start, self.pos),
                    '\0',
                ));
            }
        }

        // Subsequent characters can be letters, digits, or underscores
        while let Some(b) = self.peek() {
            if b.is_ascii_alphanumeric() || b == b'_' {
                self.advance();
            } else {
                break;
            }
        }

        // Convert the byte slice to a UTF-8 string
        let slice = std::str::from_utf8(&self.input[start..self.pos]).unwrap(); // Safe because we only read ASCII characters

        Ok(slice.to_string())
    }

    fn read_offset_or_symbol(&mut self) -> Result<OffsetOrSymbol, LexerError> {
        self.skip_whitespace();

        let start = self.pos;

        if let Some(c) = self.peek() {
            if c.is_ascii_digit() || c == b'-' {
                let num = self.read_number::<i8>()?;
                Ok(OffsetOrSymbol::Offset(i4::from_i8(num)))
            } else if c.is_ascii_alphabetic() || c == b'_' {
                Ok(OffsetOrSymbol::Symbol(self.read_identifier()?))
            } else {
                Err(LexerError::InvalidOffset(
                    Span::new(start, self.pos),
                    "Expected number or symbol".to_string(),
                ))
            }
        } else {
            Err(LexerError::InvalidOffset(
                Span::new(start, self.pos),
                "Unexpected end of input".to_string(),
            ))
        }
    }

    fn read_immediate_or_symbol(&mut self) -> Result<ImmediateOrSymbol, LexerError> {
        self.skip_whitespace();

        let start = self.pos;

        if let Some(c) = self.peek() {
            if c.is_ascii_digit() {
                // It's a number
                Ok(ImmediateOrSymbol::Immediate(self.read_number::<i8>()?))
            } else if c.is_ascii_alphabetic() || c == b'_' {
                // It's a symbol
                Ok(ImmediateOrSymbol::Symbol(self.read_identifier()?))
            } else {
                Err(LexerError::InvalidNumber(
                    Span::new(start, self.pos),
                    "Expected number or symbol".to_string(),
                ))
            }
        } else {
            Err(LexerError::InvalidNumber(
                Span::new(start, self.pos),
                "Unexpected end of input".to_string(),
            ))
        }
    }

    fn read_address_or_symbol(&mut self) -> Result<AddressOrSymbol, LexerError> {
        self.skip_whitespace();

        let start = self.pos;

        if let Some(c) = self.peek() {
            if c.is_ascii_digit() {
                // It's a number
                let num = self.read_number::<u16>()?;
                Ok(AddressOrSymbol::Address(u10::from_u16(num)))
            } else if c.is_ascii_alphabetic() || c == b'_' {
                // It's a symbol
                Ok(AddressOrSymbol::Symbol(self.read_identifier()?))
            } else {
                Err(LexerError::InvalidNumber(
                    Span::new(start, self.pos),
                    "Expected number or symbol".to_string(),
                ))
            }
        } else {
            Err(LexerError::InvalidNumber(
                Span::new(start, self.pos),
                "Unexpected end of input".to_string(),
            ))
        }
    }

    fn read_condition(&mut self) -> Result<Condition, LexerError> {
        self.skip_whitespace();
        let start = self.pos;

        while let Some(b) = self.peek() {
            if b.is_ascii_alphanumeric() {
                self.advance();
            } else {
                break;
            }
        }

        let slice = std::str::from_utf8(&self.input[start..self.pos])
            .unwrap()
            .to_lowercase();

        match slice.as_str() {
            "eq" => Ok(Condition::Equal),
            "ne" => Ok(Condition::NotEqual),
            "ge" => Ok(Condition::GreaterEqual),
            "lt" => Ok(Condition::Less),
            _ => Err(LexerError::UnknownCondition(
                Span::new(start, self.pos),
                slice.to_string(),
            )),
        }
    }

    fn skip_char(&mut self, character: u8) -> Result<(), LexerError> {
        let start = self.pos;

        if self.peek() == Some(character) {
            self.advance();
            Ok(())
        } else {
            Err(LexerError::ExpectedCharacter(
                Span::new(start, self.pos),
                character as char,
            ))
        }
    }

    fn read_number<N: FromStr>(&mut self) -> Result<N, LexerError> {
        self.skip_whitespace();
        let start = self.pos;

        while let Some(c) = self.peek() {
            if c.is_ascii_digit() || c == b'.' || c == b'-' {
                self.advance();
            } else {
                break;
            }
        }

        let slice = std::str::from_utf8(&self.input[start..self.pos])
            .unwrap()
            .to_lowercase();

        match slice.parse::<N>() {
            Ok(value) => Ok(value),
            Err(_) => Err(LexerError::InvalidNumber(
                Span::new(start, self.pos),
                slice.to_string(),
            )),
        }
    }

    fn read_register(&mut self) -> Result<Register, LexerError> {
        let start = self.pos;
        let value = self.advance();
        match value {
            Some(b'r') => return Ok(Register::from_u8(self.read_number::<u8>()?)),
            Some(_) => {
                return Err(LexerError::InvalidRegister(
                    Span::new(start, self.pos),
                    "Registers should start with 'r'".to_string(),
                ));
            }
            None => {
                return Err(LexerError::InvalidRegister(
                    Span::new(start, self.pos),
                    "A register must be specified!".to_string(),
                ));
            }
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Result<TokenSpan, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.next_token() {
                Ok(Some(TokenSpan {
                    token: Token::Eof,
                    span,
                })) => return None,
                Ok(Some(token)) => return Some(Ok(token)),
                Ok(None) => continue,
                Err(e) => return Some(Err(e)),
            }
        }
    }
}
