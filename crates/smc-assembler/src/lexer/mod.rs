use std::str::FromStr;

use crate::lexer::token::{Span, Token, TokenSpan};
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

    #[error("Invalid offset `{1}`")]
    InvalidOffset(Span, String),
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
                || self.peek(0) == Some(b';')
            {
                while let Some(b) = self.peek(0) {
                    self.advance();
                    if b == b'\n' {
                        break;
                    }
                }
                self.skip_whitespace();
            } else if self.peek(0) == Some(b'/') && self.peek(1) == Some(b'*') {
                self.advance();
                self.advance();
                while let Some(b) = self.peek(0) {
                    if b == b'*' && self.peek(1) == Some(b'/') {
                        self.advance();
                        self.advance();
                        break;
                    }
                    self.advance();
                }
                self.skip_whitespace();
            } else {
                break;
            }
        }
    }

    /// Skips whitespace and comments, returning whether they contained a line break
    fn skip_trivia(&mut self) -> bool {
        let start = self.pos;
        self.skip_whitespace();
        self.skip_comments();
        self.input[start..self.pos].contains(&b'\n')
    }

    pub fn next_token(&mut self) -> Result<TokenSpan, LexerError> {
        if self.skip_trivia() {
            return Ok(TokenSpan::new(
                Token::Newline,
                Span::new(self.pos, self.pos),
            ));
        }

        let start = self.pos;

        let keyword: Option<_> = match_keywords!(
            "define" => Token::Define,

            "=" => Token::Symbol("=".to_string()),
            "!=" => Token::Symbol("!=".to_string()),
            ">=" => Token::Symbol(">=".to_string()),
            "<" => Token::Symbol("<".to_string()),
            "!" => Token::Symbol("!".to_string()),
            "!0" => Token::Symbol("!0".to_string()),
            "!-" => Token::Symbol("!-".to_string()),
        );

        let token = match keyword {
            Some((size, token)) => {
                for _ in 0..size {
                    self.advance();
                }

                TokenSpan::new(token, Span::new(start, start + size))
            }
            None => match self.advance() {
                Some(b'\'' | b'"') => match self.char_literal() {
                    Some((character, length)) => {
                        // The character, or the escape, and the closing quote
                        for _ in 0..=length {
                            self.advance();
                        }

                        TokenSpan::new(Token::Char(character as char), Span::new(start, self.pos))
                    }
                    None => {
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
                    // Leave the whitespace, which may be a line break, for the next token
                    if self.peek(0).is_some_and(|b| b.is_ascii_whitespace()) {
                        TokenSpan::new(Token::Symbol("-".to_string()), Span::new(start, self.pos))
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
                Some(b'a'..=b'z') | Some(b'A'..=b'Z') => {
                    self.pos -= 1;
                    TokenSpan::new(
                        Token::Identifier(self.read_identifier()),
                        Span::new(start, self.pos),
                    )
                }
                Some(b',') => TokenSpan::new(Token::Comma, Span::new(start, self.pos)),
                Some(b'[') => TokenSpan::new(Token::LBracket, Span::new(start, self.pos)),
                Some(b']') => TokenSpan::new(Token::RBracket, Span::new(start, self.pos)),
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

    /// The character in a literal whose opening quote was just read, and how many bytes it
    /// takes before the closing quote. An escape like `'\n'` takes two; a lone `'\'` is
    /// still a backslash.
    fn char_literal(&self) -> Option<(u8, usize)> {
        let closes = |offset| matches!(self.peek(offset), Some(b'\'' | b'"'));
        if self.peek(0) == Some(b'\\')
            && closes(2)
            && let Some(character) = self.peek(1).and_then(escaped)
        {
            return Some((character, 2));
        }
        match self.peek(0) {
            Some(character) if closes(1) => Some((character, 1)),
            _ => None,
        }
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

/// The character an escape like `\n` stands for, given the byte after the backslash
fn escaped(byte: u8) -> Option<u8> {
    match byte {
        b'n' => Some(b'\n'),
        b't' => Some(b'\t'),
        b'r' => Some(b'\r'),
        b'0' => Some(0),
        b'\\' | b'\'' | b'"' => Some(byte),
        _ => None,
    }
}
