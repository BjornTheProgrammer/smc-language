use std::str::FromStr;

use crate::lexer::token::{Condition, Keyword, Operation, Register, Span, Token, TokenSpan};
use anyhow::Result;
use smc_macros::match_keywords;
use thiserror::Error;

pub mod token;

pub struct Lexer<'a> {
    input: &'a [u8],
    pos: usize,
    finished: bool,
}

#[derive(Error, Debug, Clone)]
pub enum LexerError {
    #[error("Invalid number `{1}`")]
    InvalidNumber(Span, String),

    #[error("Unexpected character `{1}`")]
    UnexpectedCharacter(Span, char),

    #[error("Expected character `{1}`")]
    ExpectedCharacter(Span, char),

    #[error("Unknown condition `{1}`")]
    UnknownCondition(Span, String),

    #[error("Invalid offset `{1}`")]
    InvalidOffset(Span, String),

    #[error("Invalid ISA code `{1}`")]
    InvalidIsaCode(Span, String),

    #[error("Invalid register number `{1}`")]
    InvalidRegisterNumber(Span, String),
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input: input.as_bytes(),
            pos: 0,
            finished: false,
        }
    }

    /// Peek at the next byte without consuming
    fn peek(&self, amount: usize) -> Option<u8> {
        self.input.get(self.pos + amount).copied()
    }

    /// Consume and return the next byte
    fn advance(&mut self) -> Option<u8> {
        let res = self.peek(0);
        if res.is_some() {
            self.pos += 1;
        }
        res
    }

    fn skip_whitespace(&mut self) -> bool {
        let mut whitespace_exists = false;
        while let Some(b) = self.peek(0) {
            if b.is_ascii_whitespace() {
                whitespace_exists = true;
                self.advance();
            } else {
                break;
            }
        }

        return whitespace_exists;
    }

    pub fn read_identifier(&mut self) -> String {
        let mut identifier = String::new();
        while let Some(b) = self.peek(0) {
            if b.is_ascii_alphanumeric() || b == b'_' {
                identifier.push(b as char);
                self.advance();
            } else {
                break;
            }
        }
        identifier
    }

    fn skip_comments(&mut self) {
        loop {
            if self.peek(0) == Some(b'#')
                || (self.peek(0) == Some(b'/') && self.peek(1) == Some(b'/'))
            {
                while let Some(b) = self.peek(0) {
                    self.advance();
                    if b == b'\n' {
                        break;
                    }
                }
                self.skip_whitespace();
            } else {
                break;
            }
        }
    }

    pub fn next_token(&mut self) -> Result<TokenSpan, LexerError> {
        self.skip_whitespace();
        self.skip_comments();

        let start = self.pos;

        let keyword: Option<_> = match_keywords!(
            "Nop" => Keyword::Operation(Operation::Nop),
            "Hlt" => Keyword::Operation(Operation::Hlt),
            "Add" => Keyword::Operation(Operation::Add),
            "Sub" => Keyword::Operation(Operation::Sub),
            "Nor" => Keyword::Operation(Operation::Nor),
            "And" => Keyword::Operation(Operation::And),
            "Xor" => Keyword::Operation(Operation::Xor),
            "Rsh" => Keyword::Operation(Operation::Rsh),
            "Ldi" => Keyword::Operation(Operation::Ldi),
            "Adi" => Keyword::Operation(Operation::Adi),
            "Jmp" => Keyword::Operation(Operation::Jmp),
            "Brh" => Keyword::Operation(Operation::Brh),
            "Cal" => Keyword::Operation(Operation::Cal),
            "Ret" => Keyword::Operation(Operation::Ret),
            "Lod" => Keyword::Operation(Operation::Lod),
            "Str" => Keyword::Operation(Operation::Str),
            "Cmp" => Keyword::Operation(Operation::Cmp),
            "Mov" => Keyword::Operation(Operation::Mov),
            "Lsh" => Keyword::Operation(Operation::Lsh),
            "Inc" => Keyword::Operation(Operation::Inc),
            "Dec" => Keyword::Operation(Operation::Dec),
            "Not" => Keyword::Operation(Operation::Not),
            "Neg" => Keyword::Operation(Operation::Neg),
            "Or" => Keyword::Operation(Operation::Or),
            "Cpy" => Keyword::Operation(Operation::Cpy),
            "Adc" => Keyword::Operation(Operation::Adc),
            "Mld" => Keyword::Operation(Operation::Mld),
            "Mst" => Keyword::Operation(Operation::Mst),
            "Pld" => Keyword::Operation(Operation::Pld),
            "Pst" => Keyword::Operation(Operation::Pst),
            "Inv" => Keyword::Operation(Operation::Inv),
            "Cpi" => Keyword::Operation(Operation::Cpi),
            "Ani" => Keyword::Operation(Operation::Ani),
            "Bkl" => Keyword::Operation(Operation::Bkl),
            "Bkr" => Keyword::Operation(Operation::Bkr),
            "Skp" => Keyword::Operation(Operation::Skp),
            "Clr" => Keyword::Operation(Operation::Clr),

            "eq" => Keyword::Condition(Condition::Equal),
            "ne" => Keyword::Condition(Condition::NotEqual),
            "ge" => Keyword::Condition(Condition::GreaterEqual),
            "lt" => Keyword::Condition(Condition::Less),

            "=" => Keyword::Condition(Condition::Equal),
            "!=" => Keyword::Condition(Condition::NotEqual),
            ">=" => Keyword::Condition(Condition::GreaterEqual),
            "<" => Keyword::Condition(Condition::Less),

            "!" => Keyword::Condition(Condition::Not),
            "!0" => Keyword::Condition(Condition::NotZero),
            "!-" => Keyword::Condition(Condition::NotNegative),

            "z" => Keyword::Condition(Condition::Equal),
            "nz" => Keyword::Condition(Condition::NotEqual),
            "c" => Keyword::Condition(Condition::GreaterEqual),
            "nc" => Keyword::Condition(Condition::Less),

            "zero" => Keyword::Condition(Condition::Equal),
            "notzero" => Keyword::Condition(Condition::NotEqual),
            "carry" => Keyword::Condition(Condition::GreaterEqual),
            "notcarry" => Keyword::Condition(Condition::Less),

            "define" => Keyword::Define,
        );

        let token = match keyword {
            Some((size, keyword)) => {
                for _ in 0..size {
                    self.advance();
                }

                TokenSpan::new(Token::Keyword(keyword), Span::new(start, start + size))
            }
            None => match self.advance() {
                Some(b'\'' | b'"') => match self.peek(1) {
                    Some(b'\'' | b'"') => {
                        let token = TokenSpan::new(
                        Token::Number(char_to_isa_code(self.advance().expect("Should be impossible for a character to not exist after checking for the character after")).ok_or(LexerError::InvalidIsaCode(Span::new(start, self.pos), String::from("Invalid ISA code")))? as f64),
                            Span::new(start, self.pos),
                        );

                        self.advance();

                        token
                    }
                    Some(_) | None => {
                        let original_char = self.peek(0).unwrap();
                        self.advance();
                        self.advance();
                        return Err(LexerError::ExpectedCharacter(
                            Span::new(start, self.pos),
                            original_char as char,
                        ));
                    }
                },
                Some(b'.') => TokenSpan::new(
                    Token::Label(self.read_identifier()),
                    Span::new(start, self.pos),
                ),
                Some(b'-') => {
                    if self.skip_whitespace() {
                        TokenSpan::new(
                            Token::Keyword(Keyword::Condition(Condition::Not)),
                            Span::new(start, start),
                        )
                    } else {
                        self.pos -= 1;
                        let value: f64 = self.read_number()?;
                        TokenSpan::new(Token::Number(value), Span::new(start, self.pos))
                    }
                }
                Some(b'0'..=b'9') => {
                    self.pos -= 1;
                    let value: f64 = self.read_number()?;
                    TokenSpan::new(Token::Number(value), Span::new(start, self.pos))
                }
                None => TokenSpan {
                    token: Token::Eof,
                    span: Span::new(self.pos, self.pos),
                },
                Some(b'r' | b'R') if self.peek(0).is_some_and(|b| b.is_ascii_digit()) => {
                    // Parse register number directly without string allocation
                    let mut reg_num: u8 = 0;
                    let mut has_overflow = false;

                    while let Some(b) = self.peek(0) {
                        if b.is_ascii_digit() {
                            self.advance();
                            if let Some(new_val) = reg_num
                                .checked_mul(10)
                                .and_then(|v| v.checked_add(b - b'0'))
                            {
                                reg_num = new_val;
                            } else {
                                has_overflow = true;
                            }
                        } else {
                            break;
                        }
                    }

                    let span = Span::new(start, self.pos);

                    if self
                        .peek(0)
                        .is_some_and(|b| b.is_ascii_alphabetic() || b == b'_')
                    {
                        self.pos = start;
                        TokenSpan::new(
                            Token::Identifier(self.read_identifier()),
                            Span::new(start, self.pos),
                        )
                    } else if has_overflow {
                        let raw: String = self.input[start..self.pos]
                            .iter()
                            .map(|&b| b as char)
                            .collect();
                        return Err(LexerError::InvalidRegisterNumber(span, raw));
                    } else {
                        TokenSpan::new(Token::Register(Register(reg_num)), span)
                    }
                }
                Some(b'a'..=b'z') | Some(b'A'..=b'Z') => {
                    self.pos -= 1;
                    TokenSpan::new(
                        Token::Identifier(self.read_identifier()),
                        Span::new(start, self.pos),
                    )
                }
                Some(b',') => TokenSpan::new(Token::Comma, Span::new(start, self.pos)),
                Some(c) => {
                    return Err(LexerError::UnexpectedCharacter(
                        Span::new(start, self.pos),
                        c as char,
                    ));
                }
            },
        };
        Ok(token)
    }

    fn read_number<N: FromStr>(&mut self) -> Result<N, LexerError> {
        self.skip_whitespace();
        let start = self.pos;

        // Check for binary prefix (0b)
        if self.peek(0) == Some(b'0') && matches!(self.peek(1), Some(b'b') | Some(b'B')) {
            self.advance(); // consume '0'
            self.advance(); // consume 'b'

            let binary_start = self.pos;
            while let Some(c) = self.peek(0) {
                if c == b'0' || c == b'1' || c == b'_' {
                    self.advance();
                } else {
                    break;
                }
            }

            let slice: String = self.input[binary_start..self.pos]
                .iter()
                .filter(|&&b| b != b'_')
                .map(|&b| b as char)
                .collect();

            if slice.is_empty() {
                return Err(LexerError::InvalidNumber(
                    Span::new(start, self.pos),
                    "0b".to_string(),
                ));
            }

            match i64::from_str_radix(&slice, 2) {
                Ok(value) => match value.to_string().parse::<N>() {
                    Ok(v) => Ok(v),
                    Err(_) => Err(LexerError::InvalidNumber(
                        Span::new(start, self.pos),
                        format!("0b{}", slice),
                    )),
                },
                Err(_) => Err(LexerError::InvalidNumber(
                    Span::new(start, self.pos),
                    format!("0b{}", slice),
                )),
            }
        // Check for hex prefix (0x)
        } else if self.peek(0) == Some(b'0') && matches!(self.peek(1), Some(b'x') | Some(b'X')) {
            self.advance(); // consume '0'
            self.advance(); // consume 'x'

            let hex_start = self.pos;
            while let Some(c) = self.peek(0) {
                if c.is_ascii_hexdigit() || c == b'_' {
                    self.advance();
                } else {
                    break;
                }
            }

            let slice: String = self.input[hex_start..self.pos]
                .iter()
                .filter(|&&b| b != b'_')
                .map(|&b| b as char)
                .collect();

            if slice.is_empty() {
                return Err(LexerError::InvalidNumber(
                    Span::new(start, self.pos),
                    "0x".to_string(),
                ));
            }

            match i64::from_str_radix(&slice, 16) {
                Ok(value) => match value.to_string().parse::<N>() {
                    Ok(v) => Ok(v),
                    Err(_) => Err(LexerError::InvalidNumber(
                        Span::new(start, self.pos),
                        format!("0x{}", slice),
                    )),
                },
                Err(_) => Err(LexerError::InvalidNumber(
                    Span::new(start, self.pos),
                    format!("0x{}", slice),
                )),
            }
        } else {
            // Optional negative sign at start only
            if self.peek(0) == Some(b'-') {
                self.advance();
            }

            // Read integer part
            while let Some(c) = self.peek(0) {
                if c.is_ascii_digit() || c == b'_' {
                    self.advance();
                } else {
                    break;
                }
            }

            // Read decimal part
            if self.peek(0) == Some(b'.') && self.peek(1).is_some_and(|b| b.is_ascii_digit()) {
                self.advance(); // consume '.'
                while let Some(c) = self.peek(0) {
                    if c.is_ascii_digit() || c == b'_' {
                        self.advance();
                    } else {
                        break;
                    }
                }
            }

            let slice: String = self.input[start..self.pos]
                .iter()
                .filter(|&&b| b != b'_')
                .map(|&b| b as char)
                .collect();

            match slice.parse::<N>() {
                Ok(value) => Ok(value),
                Err(_) => Err(LexerError::InvalidNumber(Span::new(start, self.pos), slice)),
            }
        }
    }
}

fn char_to_isa_code(c: u8) -> Option<u8> {
    match c {
        b' ' => Some(0),
        b'a'..=b'z' => Some(c - b'a' + 1),
        b'A'..=b'Z' => Some(c - b'A' + 1),
        b'.' => Some(27),
        b'!' => Some(28),
        b'?' => Some(29),
        _ => None,
    }
}

impl Iterator for Lexer<'_> {
    type Item = Result<TokenSpan, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.finished {
                return None;
            }

            match self.next_token() {
                Ok(
                    token @ TokenSpan {
                        token: Token::Eof,
                        span: _,
                    },
                ) => {
                    self.finished = true;
                    return Some(Ok(token));
                }
                Ok(token) => return Some(Ok(token)),
                Err(e) => return Some(Err(e)),
            }
        }
    }
}
