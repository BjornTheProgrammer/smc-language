use std::collections::HashMap;

use thiserror::Error;

use crate::{
    lexer::{
        LexerError,
        token::{Condition, Keyword, Operation, Register, Span, Token, TokenSpan},
    },
    parser::operations::{Address, Immediate, Offset, OperationWithArgs, SpannedOperation},
};

pub mod operations;

#[derive(Debug, Clone)]
pub struct Parser {
    tokens: Vec<Result<TokenSpan, LexerError>>,
    recovery_mode: bool,
    pos: usize,
    last_span: Span,
}

#[derive(Debug, Clone)]
pub enum ParsedItem {
    Operation(OperationWithArgs),
    Label(String),
    Define(String, f32),
}

pub type DefineMap = HashMap<String, f64>;
pub type LabelMap = HashMap<String, usize>;

#[derive(Error, Debug, Clone)]
pub enum ParserError {
    #[error("Syntax Error: {0}")]
    SyntaxError(#[from] LexerError),

    #[error("Semantic Error: Duplicate define {1}")]
    DuplicateDefine(Span, String),

    #[error("Semantic Error: Duplicate label {1}")]
    DuplicateLabel(Span, String),

    #[error("Semantic Error: Expected {1}, but recieved {2:?}")]
    ExpectedButReceived(Span, String, Token),

    #[error("Semantic Error: Unexpected Eof")]
    UnexpectedEof(Span),
}

pub struct ParserResult {
    pub defines: DefineMap,
    pub labels: LabelMap,
    pub operations: Vec<SpannedOperation>,
    pub errors: Vec<ParserError>,
}

impl Parser {
    pub fn new(tokens: Vec<Result<TokenSpan, LexerError>>) -> Self {
        Parser {
            tokens,
            recovery_mode: false,
            pos: 0,
            last_span: Span::new(0, 0),
        }
    }

    fn peek(&self, amount: usize) -> Result<TokenSpan, LexerError> {
        match self.tokens.get(self.pos + amount).cloned() {
            Some(token) => token,
            None => self.tokens[self.tokens.len() - 1].clone(),
        }
    }

    fn advance(&mut self) -> Result<TokenSpan, LexerError> {
        let token = self.peek(0);
        self.pos += 1;

        match token {
            Ok(TokenSpan { token: _, ref span }) => {
                self.last_span = span.clone();
            }
            Err(_) => (),
        }

        token
    }

    fn expect_number(&mut self, offset: usize, errors: &mut Vec<ParserError>) -> Option<f64> {
        match self.peek(offset) {
            Ok(TokenSpan {
                token: Token::Number(n),
                ..
            }) => Some(n),
            Ok(TokenSpan { token, span }) => {
                self.recovery_mode = true;
                errors.push(ParserError::ExpectedButReceived(
                    span,
                    "number".to_string(),
                    token,
                ));
                None
            }
            Err(err) => {
                self.recovery_mode = true;
                errors.push(ParserError::SyntaxError(err));
                None
            }
        }
    }

    fn expect_condition(
        &mut self,
        offset: usize,
        errors: &mut Vec<ParserError>,
    ) -> Option<Condition> {
        match self.peek(offset) {
            Ok(TokenSpan {
                token: Token::Keyword(Keyword::Condition(cond)),
                ..
            }) => Some(cond),
            Ok(TokenSpan { token, span }) => {
                self.recovery_mode = true;
                errors.push(ParserError::ExpectedButReceived(
                    span,
                    "condition (eq, ne, ge, lt)".to_string(),
                    token,
                ));
                None
            }
            Err(err) => {
                self.recovery_mode = true;
                errors.push(ParserError::SyntaxError(err));
                None
            }
        }
    }

    fn expect_identifier(
        &mut self,
        offset: usize,
        errors: &mut Vec<ParserError>,
    ) -> Option<String> {
        match self.peek(offset) {
            Ok(TokenSpan {
                token: Token::Identifier(id),
                ..
            }) => Some(id),
            Ok(TokenSpan { token, span }) => {
                self.recovery_mode = true;
                errors.push(ParserError::ExpectedButReceived(
                    span,
                    "identifier".to_string(),
                    token,
                ));
                None
            }
            Err(err) => {
                self.recovery_mode = true;
                errors.push(ParserError::SyntaxError(err));
                None
            }
        }
    }

    fn expect_address(&mut self, offset: usize, errors: &mut Vec<ParserError>) -> Option<Address> {
        match self.peek(offset) {
            Ok(TokenSpan {
                token: Token::Number(n),
                ..
            }) => Some(Address::Value(n as i128)),
            Ok(TokenSpan {
                token: Token::Label(l),
                ..
            }) => Some(Address::Label(l)),
            Ok(TokenSpan {
                token: Token::Identifier(id),
                ..
            }) => Some(Address::Define(id)),
            Ok(TokenSpan { token, span }) => {
                self.recovery_mode = true;
                errors.push(ParserError::ExpectedButReceived(
                    span,
                    "address".to_string(),
                    token,
                ));
                None
            }
            Err(err) => {
                self.recovery_mode = true;
                errors.push(ParserError::SyntaxError(err));
                None
            }
        }
    }

    fn expect_register(
        &mut self,
        offset: usize,
        errors: &mut Vec<ParserError>,
    ) -> Option<Register> {
        match self.peek(offset) {
            Ok(TokenSpan {
                token: Token::Register(reg),
                ..
            }) => Some(reg),
            Ok(TokenSpan { token, span }) => {
                self.recovery_mode = true;
                errors.push(ParserError::ExpectedButReceived(
                    span,
                    "register (r0-r15)".to_string(),
                    token,
                ));
                None
            }
            Err(err) => {
                self.recovery_mode = true;
                errors.push(ParserError::SyntaxError(err));
                None
            }
        }
    }

    fn expect_immediate(
        &mut self,
        offset: usize,
        errors: &mut Vec<ParserError>,
    ) -> Option<Immediate> {
        match self.peek(offset) {
            Ok(TokenSpan {
                token: Token::Number(n),
                ..
            }) => Some(Immediate::Value(n as i128)),
            Ok(TokenSpan {
                token: Token::Identifier(id),
                ..
            }) => Some(Immediate::Define(id)),
            Ok(TokenSpan { token, span }) => {
                self.recovery_mode = true;
                errors.push(ParserError::ExpectedButReceived(
                    span,
                    "immediate (number or define)".to_string(),
                    token,
                ));
                None
            }
            Err(err) => {
                self.recovery_mode = true;
                errors.push(ParserError::SyntaxError(err));
                None
            }
        }
    }

    fn expect_offset(&mut self, offset: usize, errors: &mut Vec<ParserError>) -> Option<Offset> {
        match self.peek(offset) {
            Ok(TokenSpan {
                token: Token::Number(n),
                ..
            }) => Some(Offset::Value(n as i128)),
            Ok(TokenSpan {
                token: Token::Identifier(id),
                ..
            }) => Some(Offset::Define(id)),
            Ok(_) => None,
            Err(err) => {
                self.recovery_mode = true;
                errors.push(ParserError::SyntaxError(err));
                None
            }
        }
    }

    pub fn parse(&mut self) -> ParserResult {
        let mut defines = DefineMap::new();
        let mut labels = LabelMap::new();
        let mut operations = Vec::new();
        let mut errors = Vec::new();

        let mut instruction = 0;

        loop {
            instruction += 1;

            let token = self.advance();
            if self.recovery_mode && !is_sync_token(&token) {
                continue;
            } else if self.recovery_mode {
                self.recovery_mode = false;
            }

            match token {
                Ok(TokenSpan {
                    token: Token::Eof,
                    span: _,
                }) => break,
                Ok(TokenSpan {
                    token: Token::Keyword(Keyword::Define),
                    span,
                }) => {
                    instruction -= 1;

                    let identifier = self.expect_identifier(0, &mut errors);
                    let number = self.expect_number(1, &mut errors);

                    let (identifier, number) = match (identifier, number) {
                        (Some(identifier), Some(number)) => (identifier, number),
                        _ => continue,
                    };

                    let _ = self.advance();
                    let _ = self.advance();

                    if defines.contains_key(&identifier) {
                        errors.push(ParserError::DuplicateDefine(span, identifier));
                    } else {
                        defines.insert(identifier, number);
                    }
                }
                Ok(TokenSpan {
                    token: Token::Label(label),
                    span,
                }) => {
                    instruction -= 1;
                    if labels.contains_key(&label) {
                        self.recovery_mode = true;
                        errors.push(ParserError::DuplicateLabel(span, label));
                    } else {
                        labels.insert(label, instruction);
                    }
                }
                Ok(TokenSpan {
                    token:
                        Token::Keyword(Keyword::Operation(op @ (Operation::Jmp | Operation::Cal))),
                    span,
                }) => {
                    let address = self.expect_address(0, &mut errors);

                    let address = match address {
                        Some(address) => address,
                        _ => continue,
                    };

                    let _ = self.advance();

                    let span = Span::new(span.start(), self.last_span.end());

                    match op {
                        Operation::Jmp => operations
                            .push(SpannedOperation::new(OperationWithArgs::Jmp(address), span)),
                        Operation::Cal => operations
                            .push(SpannedOperation::new(OperationWithArgs::Cal(address), span)),
                        _ => unreachable!(),
                    }
                }
                Ok(TokenSpan {
                    token: Token::Keyword(Keyword::Operation(Operation::Brh)),
                    span,
                }) => {
                    let condition = self.expect_condition(0, &mut errors);
                    let address = self.expect_address(1, &mut errors);

                    let (condition, address) = match (condition, address) {
                        (Some(condition), Some(address)) => (condition, address),
                        _ => continue,
                    };

                    let _ = self.advance();
                    let _ = self.advance();

                    let span = Span::new(span.start(), self.last_span.end());

                    operations.push(SpannedOperation::new(
                        OperationWithArgs::Brh(condition, address),
                        span,
                    ));
                }
                Ok(TokenSpan {
                    token:
                        Token::Keyword(Keyword::Operation(
                            op @ (Operation::Nop | Operation::Hlt | Operation::Ret),
                        )),
                    span,
                }) => match op {
                    Operation::Nop => {
                        operations.push(SpannedOperation::new(OperationWithArgs::Nop, span))
                    }
                    Operation::Hlt => {
                        operations.push(SpannedOperation::new(OperationWithArgs::Hlt, span))
                    }
                    Operation::Ret => {
                        operations.push(SpannedOperation::new(OperationWithArgs::Ret, span))
                    }
                    _ => unreachable!(),
                },
                Ok(TokenSpan {
                    token:
                        Token::Keyword(Keyword::Operation(
                            op @ (Operation::Add
                            | Operation::Sub
                            | Operation::Nor
                            | Operation::And
                            | Operation::Xor),
                        )),
                    span,
                }) => {
                    let r1 = self.expect_register(0, &mut errors);
                    let r2 = self.expect_register(1, &mut errors);
                    let r3 = self.expect_register(2, &mut errors);

                    let (r1, r2, r3) = match (r1, r2, r3) {
                        (Some(r1), Some(r2), Some(r3)) => (r1, r2, r3),
                        _ => continue,
                    };

                    let _ = self.advance();
                    let _ = self.advance();
                    let _ = self.advance();

                    let span = Span::new(span.start(), self.last_span.end());

                    match op {
                        Operation::Add => operations.push(SpannedOperation::new(
                            OperationWithArgs::Add3(r1, r2, r3),
                            span,
                        )),
                        Operation::Sub => operations.push(SpannedOperation::new(
                            OperationWithArgs::Sub3(r1, r2, r3),
                            span,
                        )),
                        Operation::Nor => operations.push(SpannedOperation::new(
                            OperationWithArgs::Nor3(r1, r2, r3),
                            span,
                        )),
                        Operation::And => operations.push(SpannedOperation::new(
                            OperationWithArgs::And3(r1, r2, r3),
                            span,
                        )),
                        Operation::Xor => operations.push(SpannedOperation::new(
                            OperationWithArgs::Xor3(r1, r2, r3),
                            span,
                        )),
                        _ => unreachable!(),
                    }
                }
                Ok(TokenSpan {
                    token: Token::Keyword(Keyword::Operation(Operation::Rsh)),
                    span,
                }) => {
                    let r1 = self.expect_register(0, &mut errors);
                    let r2 = self.expect_register(1, &mut errors);

                    let (r1, r2) = match (r1, r2) {
                        (Some(r1), Some(r2)) => (r1, r2),
                        _ => continue,
                    };

                    let _ = self.advance();
                    let _ = self.advance();

                    let span = Span::new(span.start(), self.last_span.end());

                    operations.push(SpannedOperation::new(OperationWithArgs::Rsh2(r1, r2), span));
                }
                Ok(TokenSpan {
                    token:
                        Token::Keyword(Keyword::Operation(op @ (Operation::Ldi | Operation::Adi))),
                    span,
                }) => {
                    let register = self.expect_register(0, &mut errors);
                    let immediate = self.expect_immediate(1, &mut errors);

                    let (register, immediate) = match (register, immediate) {
                        (Some(register), Some(immediate)) => (register, immediate),
                        _ => continue,
                    };

                    let _ = self.advance();
                    let _ = self.advance();

                    let span = Span::new(span.start(), self.last_span.end());

                    match op {
                        Operation::Ldi => {
                            operations.push(SpannedOperation::new(
                                OperationWithArgs::Ldi2(register, immediate),
                                span,
                            ));
                        }
                        Operation::Adi => {
                            operations.push(SpannedOperation::new(
                                OperationWithArgs::Adi2(register, immediate),
                                span,
                            ));
                        }
                        _ => unreachable!(),
                    }
                }
                Ok(TokenSpan {
                    token:
                        Token::Keyword(Keyword::Operation(op @ (Operation::Lod | Operation::Str))),
                    span,
                }) => {
                    let r1 = self.expect_register(0, &mut errors);
                    let r2 = self.expect_register(1, &mut errors);
                    let offset = self.expect_offset(2, &mut errors);

                    let (r1, r2) = match (r1, r2) {
                        (Some(r1), Some(r2)) => (r1, r2),
                        _ => continue,
                    };

                    let _ = self.advance();
                    let _ = self.advance();

                    if let Some(_) = offset {
                        let _ = self.advance();
                    }

                    let span = Span::new(span.start(), self.last_span.end());

                    match op {
                        Operation::Lod => {
                            operations.push(SpannedOperation::new(
                                OperationWithArgs::Lod(r1, r2, offset),
                                span,
                            ));
                        }
                        Operation::Str => {
                            operations.push(SpannedOperation::new(
                                OperationWithArgs::Str(r1, r2, offset),
                                span,
                            ));
                        }
                        _ => unreachable!(),
                    }
                }
                Ok(TokenSpan {
                    token:
                        Token::Keyword(Keyword::Operation(
                            op @ (Operation::Cmp
                            | Operation::Mov
                            | Operation::Lsh
                            | Operation::Not
                            | Operation::Neg),
                        )),
                    span,
                }) => {
                    let r1 = self.expect_register(0, &mut errors);
                    let r2 = self.expect_register(1, &mut errors);

                    let (r1, r2) = match (r1, r2) {
                        (Some(r1), Some(r2)) => (r1, r2),
                        _ => continue,
                    };

                    let _ = self.advance();
                    let _ = self.advance();

                    let span = Span::new(span.start(), self.last_span.end());

                    let r0 = Register(0);

                    match op {
                        Operation::Cmp => operations.push(SpannedOperation::new(
                            OperationWithArgs::Sub3(r1, r2, r0),
                            span,
                        )),
                        Operation::Mov => operations.push(SpannedOperation::new(
                            OperationWithArgs::Add3(r1, r0, r2),
                            span,
                        )),
                        Operation::Lsh => operations.push(SpannedOperation::new(
                            OperationWithArgs::Add3(r1, r1, r2),
                            span,
                        )),
                        Operation::Not => operations.push(SpannedOperation::new(
                            OperationWithArgs::Nor3(r1, r0, r2),
                            span,
                        )),
                        Operation::Neg => operations.push(SpannedOperation::new(
                            OperationWithArgs::Sub3(r0, r1, r2),
                            span,
                        )),
                        _ => unreachable!(),
                    }
                }
                Ok(TokenSpan {
                    token:
                        Token::Keyword(Keyword::Operation(op @ (Operation::Inc | Operation::Dec))),
                    span,
                }) => {
                    let register = self.expect_register(0, &mut errors);

                    let register = match register {
                        Some(register) => register,
                        _ => continue,
                    };

                    let _ = self.advance();

                    let span = Span::new(span.start(), self.last_span.end());

                    match op {
                        Operation::Inc => operations.push(SpannedOperation::new(
                            OperationWithArgs::Adi2(register, Immediate::Value(1)),
                            span,
                        )),
                        Operation::Dec => operations.push(SpannedOperation::new(
                            OperationWithArgs::Adi2(register, Immediate::Value(-1)),
                            span,
                        )),
                        _ => unreachable!(),
                    }
                }
                Ok(token) => {
                    self.recovery_mode = true;
                    errors.push(ParserError::ExpectedButReceived(
                        token.span,
                        "operation".to_string(),
                        token.token,
                    ));
                }
                Err(err) => {
                    self.recovery_mode = true;
                    errors.push(ParserError::SyntaxError(err));
                }
            }
        }

        ParserResult {
            defines: defines,
            labels: labels,
            operations: operations,
            errors: errors,
        }
    }
}

fn is_sync_token(token: &Result<TokenSpan, LexerError>) -> bool {
    matches!(
        token,
        Ok(TokenSpan {
            token: Token::Keyword(Keyword::Operation(_))
                | Token::Keyword(Keyword::Define)
                | Token::Label(_)
                | Token::Eof,
            span: _,
        })
    )
}
