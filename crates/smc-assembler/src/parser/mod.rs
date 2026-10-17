use std::collections::{HashMap, HashSet};

use thiserror::Error;

use crate::lexer::{
    LexerError,
    token::{Span, Token, TokenSpan},
};

pub mod operations;

use operations::{Instruction, Operand, SpannedOperand};

pub type DefineMap = HashMap<String, f64>;
pub type DefineSpanMap = HashMap<String, Span>;
pub type LabelSpanMap = HashMap<String, Span>;
pub type ReferenceMap = HashMap<String, Vec<Span>>;

#[derive(Error, Debug, Clone)]
pub enum ParserError {
    #[error("Syntax Error: {0}")]
    SyntaxError(#[from] LexerError),

    #[error("Semantic Error: Duplicate define `{1}`")]
    DuplicateDefine(Span, String),

    #[error("Semantic Error: Duplicate label `.{1}`")]
    DuplicateLabel(Span, String),

    #[error("Semantic Error: Expected {1}, but received {2:?}")]
    ExpectedButReceived(Span, String, Token),

    #[error("Semantic Error: Unexpected end of file")]
    UnexpectedEof(Span),
}

#[derive(Debug, Clone)]
pub enum ParsedItem {
    Label(String, Span),
    Instruction(Instruction),
}

#[derive(Debug, Clone)]
pub struct ParserResult {
    pub defines: DefineMap,
    pub items: Vec<ParsedItem>,
    pub errors: Vec<ParserError>,
    pub define_spans: DefineSpanMap,
    pub label_spans: LabelSpanMap,
    pub define_references: ReferenceMap,
    pub label_references: ReferenceMap,
}

/// Parses the syntax shared by every target: labels, defines, and instructions made of a
/// mnemonic and operands. Each statement ends at a line break. What a mnemonic means, and
/// which operands it takes, is up to the target.
#[derive(Debug, Clone)]
pub struct Parser {
    tokens: Vec<Result<TokenSpan, LexerError>>,
    pos: usize,
    last_span: Span,
    recovery_mode: bool,
    define_references: ReferenceMap,
    label_references: ReferenceMap,
}

impl Parser {
    pub fn new(tokens: Vec<Result<TokenSpan, LexerError>>) -> Self {
        Parser {
            tokens,
            pos: 0,
            last_span: Span::new(0, 0),
            recovery_mode: false,
            define_references: ReferenceMap::new(),
            label_references: ReferenceMap::new(),
        }
    }

    fn peek(&self, offset: usize) -> Result<TokenSpan, LexerError> {
        self.tokens
            .get(self.pos + offset)
            .cloned()
            .unwrap_or_else(|| self.tokens.last().unwrap().clone())
    }

    fn advance(&mut self) -> Result<TokenSpan, LexerError> {
        let token = self.peek(0);
        self.pos += 1;
        if let Ok(TokenSpan { span, .. }) = &token {
            self.last_span = span.clone();
        }
        token
    }

    fn make_span(&self, start: &Span) -> Span {
        Span::new(start.start(), self.last_span.end())
    }

    fn enter_recovery(&mut self, error: ParserError, errors: &mut Vec<ParserError>) {
        self.recovery_mode = true;
        errors.push(error);
    }

    /// Tokens that end a statement, and where parsing resumes after an error. A label later
    /// on the same line is an operand, not a new label.
    fn ends_statement(token: &Result<TokenSpan, LexerError>) -> bool {
        matches!(
            token,
            Ok(TokenSpan {
                token: Token::Newline | Token::Define | Token::Eof,
                ..
            })
        )
    }

    fn expect<T, F>(
        &mut self,
        offset: usize,
        expected: &str,
        extractor: F,
    ) -> Result<T, ParserError>
    where
        F: FnOnce(&Token) -> Option<T>,
    {
        match self.peek(offset) {
            Ok(TokenSpan { token, span }) => extractor(&token)
                .ok_or_else(|| ParserError::ExpectedButReceived(span, expected.to_string(), token)),
            Err(e) => Err(ParserError::SyntaxError(e)),
        }
    }

    fn expect_number(&mut self, offset: usize) -> Result<f64, ParserError> {
        self.expect(offset, "number", |token| match token {
            Token::Number(n) => Some(*n),
            _ => None,
        })
    }

    fn expect_identifier(&mut self, offset: usize) -> Result<String, ParserError> {
        self.expect(offset, "identifier", |token| match token {
            Token::Identifier(id) => Some(id.clone()),
            _ => None,
        })
    }

    fn reference(map: &mut ReferenceMap, name: &str, span: Span) {
        map.entry(name.to_string()).or_default().push(span);
    }

    pub fn parse(mut self) -> ParserResult {
        let mut defines = DefineMap::new();
        let mut define_spans = DefineSpanMap::new();
        let mut label_spans = LabelSpanMap::new();
        let mut labels: HashSet<String> = HashSet::new();
        let mut items = Vec::new();
        let mut errors = Vec::new();

        loop {
            let token = self.advance();

            // Skip tokens during error recovery until we hit a sync point
            if self.recovery_mode {
                if Self::ends_statement(&token) {
                    self.recovery_mode = false;
                } else {
                    continue;
                }
            }

            match token {
                Ok(TokenSpan {
                    token: Token::Eof, ..
                }) => break,

                Ok(TokenSpan {
                    token: Token::Newline,
                    ..
                }) => {}

                Ok(TokenSpan {
                    token: Token::Label(name),
                    span,
                }) => {
                    if !labels.insert(name.clone()) {
                        self.enter_recovery(
                            ParserError::DuplicateLabel(span.clone(), name.clone()),
                            &mut errors,
                        );
                    } else {
                        items.push(ParsedItem::Label(name.clone(), span.clone()));
                        label_spans.insert(name, span);
                    }
                }

                Ok(TokenSpan {
                    token: Token::Define,
                    span: _,
                }) => match self.parse_define() {
                    Ok((name, value, name_span)) => {
                        if defines.contains_key(&name) {
                            errors.push(ParserError::DuplicateDefine(name_span, name));
                        } else {
                            defines.insert(name.clone(), value);
                            define_spans.insert(name, name_span);
                        }
                    }
                    Err(e) => self.enter_recovery(e, &mut errors),
                },

                Ok(TokenSpan {
                    token: Token::Identifier(mnemonic),
                    span,
                }) => match self.parse_instruction(mnemonic, &span) {
                    Ok(instruction) => items.push(ParsedItem::Instruction(instruction)),
                    Err(e) => self.enter_recovery(e, &mut errors),
                },

                Ok(TokenSpan { token, span }) => {
                    self.enter_recovery(
                        ParserError::ExpectedButReceived(
                            span,
                            "instruction or label".to_string(),
                            token,
                        ),
                        &mut errors,
                    );
                }

                Err(e) => {
                    self.enter_recovery(ParserError::SyntaxError(e), &mut errors);
                }
            }
        }

        ParserResult {
            defines,
            items,
            errors,
            define_spans,
            label_spans,
            define_references: self.define_references,
            label_references: self.label_references,
        }
    }

    fn parse_define(&mut self) -> Result<(String, f64, Span), ParserError> {
        let name = self.expect_identifier(0)?;
        let name_span = match self.peek(0) {
            Ok(TokenSpan { span, .. }) => span,
            Err(e) => return Err(ParserError::SyntaxError(e)),
        };
        let value = self.expect_number(1)?;
        self.advance()?;
        self.advance()?;
        Ok((name, value, name_span))
    }

    /// Parses operands up to the end of the line. Commas between operands are optional.
    fn parse_instruction(
        &mut self,
        mnemonic: String,
        start: &Span,
    ) -> Result<Instruction, ParserError> {
        let mut operands = Vec::new();

        while !Self::ends_statement(&self.peek(0)) {
            match self.peek(0) {
                Ok(TokenSpan {
                    token: Token::Comma,
                    ..
                }) => {
                    self.advance()?;
                }
                Ok(TokenSpan {
                    token: Token::LBracket,
                    span,
                }) => {
                    self.advance()?;
                    let inner = self.parse_operand()?;
                    self.expect(0, "`]`", |token| match token {
                        Token::RBracket => Some(()),
                        _ => None,
                    })?;
                    self.advance()?;
                    operands.push(SpannedOperand {
                        operand: Operand::Memory(Box::new(inner)),
                        span: self.make_span(&span),
                    });
                }
                _ => operands.push(self.parse_operand()?),
            }
        }

        Ok(Instruction {
            mnemonic,
            operands,
            span: self.make_span(start),
        })
    }

    fn parse_operand(&mut self) -> Result<SpannedOperand, ParserError> {
        let TokenSpan { token, span } = self.advance()?;

        let operand = match token {
            Token::Number(n) => Operand::Number(n as i128),
            Token::Char(c) => Operand::Char(c),
            Token::Symbol(symbol) => Operand::Symbol(symbol),
            Token::Identifier(name) => {
                Self::reference(&mut self.define_references, &name, span.clone());
                Operand::Name(name)
            }
            Token::Label(name) => {
                Self::reference(&mut self.label_references, &name, span.clone());
                Operand::Label(name)
            }
            token => {
                return Err(ParserError::ExpectedButReceived(
                    span,
                    "operand".to_string(),
                    token,
                ));
            }
        };

        Ok(SpannedOperand { operand, span })
    }
}
