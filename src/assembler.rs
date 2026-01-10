use std::collections::HashMap;

use arbitrary_int::u4;
use thiserror::Error;

use crate::lexer::token::{
    AddressOrSymbol, Condition, ImmediateOrSymbol, OffsetOrSymbol, Span, Token, TokenSpan,
};

pub struct Assembler {
    tokens: Vec<TokenSpan>,
}

#[derive(Error, Debug)]
pub enum AssemblerError {
    #[error("Duplicate definition for {1}")]
    DuplicateDefinition(Span, String),

    #[error("Undefined symbol {1}")]
    UndefinedSymbol(Span, String),
}

impl Assembler {
    pub fn new(tokens: Vec<TokenSpan>) -> Self {
        Assembler { tokens }
    }

    pub fn assemble(self) -> Result<Vec<u16>, AssemblerError> {
        let mut bytes = Vec::new();

        let (defines, tokens): (Vec<_>, Vec<_>) = self.tokens.into_iter().partition(|token| {
            matches!(
                token,
                TokenSpan {
                    token: Token::Define(_, _),
                    span: _
                }
            )
        });

        let mut symbols_map = HashMap::new();
        for define in defines {
            if let TokenSpan {
                token: Token::Define(name, value),
                span,
            } = define
            {
                if symbols_map.contains_key(&name) {
                    return Err(AssemblerError::DuplicateDefinition(span, name));
                }
                symbols_map.insert(name, value);
            }
        }

        let r0 = u4::from_u8(0);
        let lowered_tokens: Vec<TokenSpan> = tokens
            .into_iter()
            .filter_map(|token| match token {
                TokenSpan {
                    token: Token::Cmp(r1, r2),
                    span,
                } => Some(TokenSpan::new(Token::Sub(r1, r2, r0), span)),
                TokenSpan {
                    token: Token::Mov(r1, r2),
                    span,
                } => Some(TokenSpan::new(Token::Add(r1, r0, r2), span)),
                TokenSpan {
                    token: Token::Lsh(r1, r2),
                    span,
                } => Some(TokenSpan::new(Token::Add(r1, r1, r2), span)),
                TokenSpan {
                    token: Token::Inc(r1),
                    span,
                } => Some(TokenSpan::new(
                    Token::Adi(r1, ImmediateOrSymbol::Immediate(1)),
                    span,
                )),
                TokenSpan {
                    token: Token::Dec(r1),
                    span,
                } => Some(TokenSpan::new(
                    Token::Adi(r1, ImmediateOrSymbol::Immediate(-1)),
                    span,
                )),
                TokenSpan {
                    token: Token::Not(r1, r2),
                    span,
                } => Some(TokenSpan::new(Token::Nor(r1, r0, r2), span)),
                TokenSpan {
                    token: Token::Neg(r1, r2),
                    span,
                } => Some(TokenSpan::new(Token::Sub(r0, r1, r2), span)),
                TokenSpan {
                    token: Token::Define(_, _) | Token::Eof,
                    span: _,
                } => None,
                t => Some(t),
            })
            .collect();

        let mut i = 0.0;
        for token in &lowered_tokens {
            if let TokenSpan {
                token: Token::Label(label),
                span,
            } = token
            {
                if symbols_map.contains_key(label) {
                    return Err(AssemblerError::DuplicateDefinition(
                        span.clone(),
                        label.to_string(),
                    ));
                }
                symbols_map.insert(label.to_string(), i);
            } else {
                i += 1.0;
            }
        }

        for token in lowered_tokens {
            let opcode = match token {
                TokenSpan {
                    token: Token::Nop,
                    span: _,
                } => u4::from_u8(0b0000),
                TokenSpan {
                    token: Token::Hlt,
                    span: _,
                } => u4::from_u8(0b0001),
                TokenSpan {
                    token: Token::Add(_, _, _),
                    span: _,
                } => u4::from_u8(0b0010),
                TokenSpan {
                    token: Token::Sub(_, _, _),
                    span: _,
                } => u4::from_u8(0b0011),
                TokenSpan {
                    token: Token::Nor(_, _, _),
                    span: _,
                } => u4::from_u8(0b0100),
                TokenSpan {
                    token: Token::And(_, _, _),
                    span: _,
                } => u4::from_u8(0b0101),
                TokenSpan {
                    token: Token::Xor(_, _, _),
                    span: _,
                } => u4::from_u8(0b0110),
                TokenSpan {
                    token: Token::Rsh(_, _),
                    span: _,
                } => u4::from_u8(0b0111),
                TokenSpan {
                    token: Token::Ldi(_, _),
                    span: _,
                } => u4::from_u8(0b1000),
                TokenSpan {
                    token: Token::Adi(_, _),
                    span: _,
                } => u4::from_u8(0b1001),
                TokenSpan {
                    token: Token::Jmp(_),
                    span: _,
                } => u4::from_u8(0b1010),
                TokenSpan {
                    token: Token::Brh(_, _),
                    span: _,
                } => u4::from_u8(0b1011),
                TokenSpan {
                    token: Token::Cal(_),
                    span: _,
                } => u4::from_u8(0b1100),
                TokenSpan {
                    token: Token::Ret,
                    span: _,
                } => u4::from_u8(0b1101),
                TokenSpan {
                    token: Token::Lod(_, _, _),
                    span: _,
                } => u4::from_u8(0b1110),
                TokenSpan {
                    token: Token::Str(_, _, _),
                    span: _,
                } => u4::from_u8(0b1111),
                _ => continue,
            };

            match token {
                TokenSpan {
                    token: Token::Nop | Token::Hlt | Token::Ret,
                    span: _,
                } => {
                    let val = (opcode.value() as u16) << 12;
                    bytes.push(val);
                }
                TokenSpan {
                    token:
                        Token::Add(r1, r2, r3)
                        | Token::Sub(r1, r2, r3)
                        | Token::Nor(r1, r2, r3)
                        | Token::And(r1, r2, r3)
                        | Token::Xor(r1, r2, r3),
                    span: _,
                } => {
                    let val = ((opcode.value() as u16) << 12)
                        | (((r1.value() & 0xF) as u16) << 8)
                        | (((r2.value() & 0xF) as u16) << 4)
                        | ((r3.value() & 0xF) as u16);

                    bytes.push(val);
                }
                TokenSpan {
                    token: Token::Rsh(r1, r2),
                    span: _,
                } => {
                    let val = ((opcode.value() as u16) << 12)
                        | (((r1.value() & 0xF) as u16) << 8)
                        | ((r2.value() & 0xF) as u16);

                    bytes.push(val);
                }
                TokenSpan {
                    token: Token::Ldi(r1, imm1) | Token::Adi(r1, imm1),
                    span,
                } => {
                    let imm1 = match imm1 {
                        ImmediateOrSymbol::Immediate(imm) => imm as u8,
                        ImmediateOrSymbol::Symbol(symbol) => match symbols_map.get(&symbol) {
                            Some(value) => *value as u8,
                            None => return Err(AssemblerError::UndefinedSymbol(span, symbol)),
                        },
                    };

                    let val = ((opcode.value() as u16) << 12)
                        | (((r1.value() & 0xF) as u16) << 8)
                        | ((imm1 & 0xFF) as u16);

                    bytes.push(val);
                }

                TokenSpan {
                    token: Token::Jmp(addr) | Token::Cal(addr),
                    span,
                } => {
                    let addr = match addr {
                        AddressOrSymbol::Address(addr) => addr.value(),
                        AddressOrSymbol::Symbol(symbol) => match symbols_map.get(&symbol) {
                            Some(value) => *value as u16,
                            None => return Err(AssemblerError::UndefinedSymbol(span, symbol)),
                        },
                    };

                    let val = ((opcode.value() as u16) << 12)
                        | (0 << 10)
                        | ((addr & 0b0011_1111_1111) as u16);

                    bytes.push(val);
                }

                TokenSpan {
                    token: Token::Brh(cond, addr),
                    span,
                } => {
                    let addr = match addr {
                        AddressOrSymbol::Address(addr) => addr.value(),
                        AddressOrSymbol::Symbol(symbol) => match symbols_map.get(&symbol) {
                            Some(value) => *value as u16,
                            None => return Err(AssemblerError::UndefinedSymbol(span, symbol)),
                        },
                    };

                    let cond = match cond {
                        Condition::Equal => 0b00,
                        Condition::NotEqual => 0b01,
                        Condition::GreaterEqual => 0b10,
                        Condition::Less => 0b11,
                    };

                    let val = ((opcode.value() as u16) << 12)
                        | ((cond & 0b11) as u16) << 10
                        | ((addr & 0b0011_1111_1111) as u16);

                    bytes.push(val);
                }

                TokenSpan {
                    token: Token::Lod(r1, r2, offset) | Token::Str(r1, r2, offset),
                    span,
                } => {
                    let offset = match offset {
                        OffsetOrSymbol::Offset(val) => val.value(),
                        OffsetOrSymbol::Symbol(symbol) => match symbols_map.get(&symbol) {
                            Some(value) => *value as i8,
                            None => return Err(AssemblerError::UndefinedSymbol(span, symbol)),
                        },
                        OffsetOrSymbol::NotPresent => 0,
                    };

                    let val = ((opcode.value() as u16) << 12)
                        | (((r1.value() & 0xF) as u16) << 8)
                        | (((r2.value() & 0xF) as u16) << 4)
                        | ((offset as u16) & 0xF);

                    bytes.push(val);
                }
                _ => continue,
            }
        }

        Ok(bytes)
    }
}
