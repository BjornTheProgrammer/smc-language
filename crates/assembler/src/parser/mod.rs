use std::collections::{HashMap, HashSet};

use thiserror::Error;

use crate::{
    lexer::{
        LexerError,
        token::{Condition, Keyword, Operation, Register, Span, Token, TokenSpan},
    },
    parser::operations::SkipFlag,
};

pub mod operations;

use operations::{Address, Immediate, Offset, OperationWithArgs, SpannedOperation};

pub type DefineMap = HashMap<String, f64>;

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

    #[error("Semantic Error: Invalid skip `{1}`")]
    InvalidSkip(Span, String),
}

#[derive(Debug, Clone)]
pub enum ParsedItem {
    Label(String, Span),
    Operation(SpannedOperation),
}

#[derive(Debug, Clone)]
pub struct ParserResult {
    pub defines: DefineMap,
    pub items: Vec<ParsedItem>,
    pub errors: Vec<ParserError>,
}

#[derive(Debug, Clone)]
pub struct Parser {
    tokens: Vec<Result<TokenSpan, LexerError>>,
    pos: usize,
    last_span: Span,
    recovery_mode: bool,
}

/// Instruction format types for cleaner parsing
#[derive(Debug, Clone, Copy)]
enum InstructionFormat {
    /// No operands: NOP, HLT, RET
    NoOperand,
    /// Single register: INC r1, DEC r1
    Reg1,
    /// Two registers: RSH r1 r2, CMP r1 r2, MOV r1 r2
    Reg2,
    /// Three registers: ADD r1 r2 r3
    Reg3,
    /// Register + immediate: LDI r1 imm, ADI r1 imm
    RegImm,
    /// Address only: JMP addr, CAL addr
    Addr,
    /// Condition + address: BRH cond addr
    CondAddr,
    /// Two registers + optional offset: LOD r1 r2 [offset], STR r1 r2 [offset]
    Reg2Offset,
    /// Skip enum
    Skip,
}

impl Parser {
    pub fn new(tokens: Vec<Result<TokenSpan, LexerError>>) -> Self {
        Parser {
            tokens,
            pos: 0,
            last_span: Span::new(0, 0),
            recovery_mode: false,
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

    fn is_sync_token(token: &Result<TokenSpan, LexerError>) -> bool {
        matches!(
            token,
            Ok(TokenSpan {
                token: Token::Keyword(Keyword::Operation(_))
                    | Token::Keyword(Keyword::Define)
                    | Token::Label(_)
                    | Token::Eof,
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

    fn expect_register(&mut self, offset: usize) -> Result<Register, ParserError> {
        self.expect(offset, "register (r0-r15)", |token| match token {
            Token::Register(r) => Some(*r),
            _ => None,
        })
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

    fn expect_condition(&mut self, offset: usize) -> Result<Condition, ParserError> {
        self.expect(offset, "condition (eq, ne, ge, lt)", |token| match token {
            Token::Keyword(Keyword::Condition(c)) => Some(c.clone()),
            _ => None,
        })
    }

    fn expect_address(&mut self, offset: usize) -> Result<Address, ParserError> {
        self.expect(
            offset,
            "address (number, label, or define)",
            |token| match token {
                Token::Number(n) => Some(Address::Value(*n as i128)),
                Token::Label(l) => Some(Address::Label(l.clone())),
                Token::Identifier(id) => Some(Address::Define(id.clone())),
                _ => None,
            },
        )
    }

    fn expect_immediate(&mut self, offset: usize) -> Result<Immediate, ParserError> {
        self.expect(
            offset,
            "immediate (number or define)",
            |token| match token {
                Token::Number(n) => Some(Immediate::Value(*n as i128)),
                Token::Identifier(id) => Some(Immediate::Define(id.clone())),
                _ => None,
            },
        )
    }

    fn try_skip(&mut self, offset: usize) -> Result<Option<SkipFlag>, ParserError> {
        match self.peek(offset) {
            Ok(TokenSpan {
                token: Token::Keyword(Keyword::Condition(n)),
                span,
            }) => match n {
                Condition::Not => Ok(Some(SkipFlag::Never)),
                Condition::NotZero => Ok(Some(SkipFlag::IfNotZero)),
                Condition::Negative => Ok(Some(SkipFlag::IfNegative)),
                Condition::NotNegative => Ok(Some(SkipFlag::IfNotNegative)),
                _ => Err(ParserError::InvalidSkip(span, format!("{:?}", n))),
            },
            Ok(TokenSpan {
                token: Token::Number(0.0),
                ..
            }) => Ok(Some(SkipFlag::IfZero)),
            Ok(_) => Ok(None),
            Err(e) => Err(ParserError::SyntaxError(e)),
        }
    }

    fn try_offset(&mut self, offset: usize) -> Result<Option<Offset>, ParserError> {
        match self.peek(offset) {
            Ok(TokenSpan {
                token: Token::Number(n),
                ..
            }) => Ok(Some(Offset::Value(n as i128))),
            Ok(TokenSpan {
                token: Token::Identifier(id),
                ..
            }) => Ok(Some(Offset::Define(id))),
            Ok(_) => Ok(None),
            Err(e) => Err(ParserError::SyntaxError(e)),
        }
    }

    fn count_operands(&mut self) -> usize {
        let mut count = 0;
        while !Self::is_sync_token(&self.peek(count)) {
            count += 1;
        }

        count
    }

    /// Parse operands based on instruction format and construct the operation
    fn parse_instruction(
        &mut self,
        format: InstructionFormat,
        start_span: &Span,
        constructor: impl FnOnce(ParsedOperands) -> OperationWithArgs,
    ) -> Result<SpannedOperation, ParserError> {
        let operands = self.parse_operands(format)?;
        let span = self.make_span(start_span);
        Ok(SpannedOperation::new(constructor(operands), span))
    }

    fn parse_operands(&mut self, format: InstructionFormat) -> Result<ParsedOperands, ParserError> {
        match format {
            InstructionFormat::NoOperand => Ok(ParsedOperands::None),

            InstructionFormat::Reg1 => {
                let r1 = self.expect_register(0)?;
                self.advance()?;
                Ok(ParsedOperands::Reg1(r1))
            }

            InstructionFormat::Reg2 => {
                let r1 = self.expect_register(0)?;
                let r2 = self.expect_register(1)?;
                self.advance()?;
                self.advance()?;
                Ok(ParsedOperands::Reg2(r1, r2))
            }

            InstructionFormat::Reg3 => {
                let r1 = self.expect_register(0)?;
                let r2 = self.expect_register(1)?;
                let r3 = self.expect_register(2)?;
                self.advance()?;
                self.advance()?;
                self.advance()?;
                Ok(ParsedOperands::Reg3(r1, r2, r3))
            }

            InstructionFormat::RegImm => {
                let r1 = self.expect_register(0)?;
                let imm = self.expect_immediate(1)?;
                self.advance()?;
                self.advance()?;
                Ok(ParsedOperands::RegImm(r1, imm))
            }

            InstructionFormat::Addr => {
                let addr = self.expect_address(0)?;
                self.advance()?;
                Ok(ParsedOperands::Addr(addr))
            }

            InstructionFormat::CondAddr => {
                let cond = self.expect_condition(0)?;
                let addr = self.expect_address(1)?;
                self.advance()?;
                self.advance()?;
                Ok(ParsedOperands::CondAddr(cond, addr))
            }

            InstructionFormat::Reg2Offset => {
                let r1 = self.expect_register(0)?;
                let r2 = self.expect_register(1)?;
                let offset = self.try_offset(2)?;
                self.advance()?;
                self.advance()?;
                if offset.is_some() {
                    self.advance()?;
                }
                Ok(ParsedOperands::Reg2Offset(r1, r2, offset))
            }
            InstructionFormat::Skip => {
                let skip = self.try_skip(0)?;
                let skip = match skip {
                    Some(skip) => skip,
                    None => SkipFlag::Never,
                };
                self.advance()?;
                Ok(ParsedOperands::Skip(skip))
            }
        }
    }

    pub fn parse(mut self) -> ParserResult {
        let mut defines = DefineMap::new();
        let mut labels: HashSet<String> = HashSet::new();
        let mut items = Vec::new();
        let mut errors = Vec::new();

        loop {
            let token = self.advance();

            // Skip tokens during error recovery until we hit a sync point
            if self.recovery_mode {
                if Self::is_sync_token(&token) {
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
                    token: Token::Label(name),
                    span,
                }) => {
                    if !labels.insert(name.clone()) {
                        self.enter_recovery(
                            ParserError::DuplicateLabel(span.clone(), name.clone()),
                            &mut errors,
                        );
                    } else {
                        items.push(ParsedItem::Label(name, span));
                    }
                }

                Ok(TokenSpan {
                    token: Token::Keyword(Keyword::Define),
                    span,
                }) => match self.parse_define() {
                    Ok((name, value)) => {
                        if defines.contains_key(&name) {
                            errors.push(ParserError::DuplicateDefine(span, name));
                        } else {
                            defines.insert(name, value);
                        }
                    }
                    Err(e) => self.enter_recovery(e, &mut errors),
                },

                Ok(TokenSpan {
                    token: Token::Keyword(Keyword::Operation(op)),
                    span,
                }) => match self.parse_operation(op, &span) {
                    Ok(spanned_op) => {
                        items.push(ParsedItem::Operation(spanned_op));
                    }
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
        }
    }

    fn parse_define(&mut self) -> Result<(String, f64), ParserError> {
        let name = self.expect_identifier(0)?;
        let value = self.expect_number(1)?;
        self.advance()?;
        self.advance()?;
        Ok((name, value))
    }

    fn parse_operation(
        &mut self,
        op: Operation,
        span: &Span,
    ) -> Result<SpannedOperation, ParserError> {
        use InstructionFormat::*;
        use OperationWithArgs::*;

        match op {
            // No-operand instructions
            Operation::Nop => self.parse_instruction(NoOperand, span, |_| Nop),
            Operation::Hlt => self.parse_instruction(NoOperand, span, |_| Hlt),
            Operation::Ret => self.parse_instruction(NoOperand, span, |_| Ret),
            Operation::Bkl => self.parse_instruction(NoOperand, span, |_| Bkl),
            Operation::Bkr => self.parse_instruction(NoOperand, span, |_| Bkr),

            // Single-register instructions
            Operation::Inv => self.parse_instruction(Reg1, span, |o| o.into_reg1(Inv1)),
            Operation::Inc => self.parse_instruction(Reg1, span, |o| o.into_reg1(Inc1)),
            Operation::Dec => self.parse_instruction(Reg1, span, |o| o.into_reg1(Dec1)),
            Operation::Clr => self.parse_instruction(Reg1, span, |o| o.into_reg1(Clr1)),

            // Two-register instructions
            Operation::Rsh => match self.count_operands() {
                0..=1 => self.parse_instruction(Reg1, span, |o| o.into_reg1(Rsh1)),
                _ => self.parse_instruction(Reg2, span, |o| o.into_reg2(Rsh2)),
            },
            Operation::Cmp => self.parse_instruction(Reg2, span, |o| o.into_reg2(Cmp2)),
            Operation::Mov => self.parse_instruction(Reg2, span, |o| o.into_reg2(Mov2)),
            Operation::Lsh => self.parse_instruction(Reg2, span, |o| o.into_reg2(Lsh2)),
            Operation::Not => self.parse_instruction(Reg2, span, |o| o.into_reg2(Not2)),
            Operation::Neg => self.parse_instruction(Reg2, span, |o| o.into_reg2(Neg2)),
            Operation::Or => self.parse_instruction(Reg2, span, |o| o.into_reg2(Or2)),
            Operation::Cpy => self.parse_instruction(Reg2, span, |o| o.into_reg2(Cpy2)),
            Operation::Adc => self.parse_instruction(Reg2, span, |o| o.into_reg2(Adc2)),
            Operation::Mld => self.parse_instruction(Reg2, span, |o| o.into_reg2(Mld2)),
            Operation::Mst => self.parse_instruction(Reg2, span, |o| o.into_reg2(Mst2)),
            Operation::Pld => self.parse_instruction(Reg2, span, |o| o.into_reg2(Pld2)),
            Operation::Pst => self.parse_instruction(Reg2, span, |o| o.into_reg2(Pst2)),

            // Three-register instructions
            Operation::Add => match self.count_operands() {
                0..=2 => self.parse_instruction(Reg2, span, |o| o.into_reg2(Add2)),
                _ => self.parse_instruction(Reg3, span, |o| o.into_reg3(Add3)),
            },
            Operation::Sub => match self.count_operands() {
                0..=2 => self.parse_instruction(Reg2, span, |o| o.into_reg2(Sub2)),
                _ => self.parse_instruction(Reg3, span, |o| o.into_reg3(Sub3)),
            },
            Operation::Nor => self.parse_instruction(Reg3, span, |o| o.into_reg3(Nor3)),
            Operation::And => match self.count_operands() {
                0..=2 => self.parse_instruction(Reg2, span, |o| o.into_reg2(And2)),
                _ => self.parse_instruction(Reg3, span, |o| o.into_reg3(And3)),
            },
            Operation::Xor => match self.count_operands() {
                0..=2 => self.parse_instruction(Reg2, span, |o| o.into_reg2(Xor2)),
                _ => self.parse_instruction(Reg3, span, |o| o.into_reg3(Xor3)),
            },

            // Register + immediate instructions
            Operation::Ldi => self.parse_instruction(RegImm, span, |o| o.into_reg_imm(Ldi2)),
            Operation::Adi => self.parse_instruction(RegImm, span, |o| o.into_reg_imm(Adi2)),
            Operation::Cpi => self.parse_instruction(RegImm, span, |o| o.into_reg_imm(Cpi2)),
            Operation::Ani => self.parse_instruction(RegImm, span, |o| o.into_reg_imm(Ani2)),

            // Address-only instructions
            Operation::Jmp => self.parse_instruction(Addr, span, |o| o.into_addr(Jmp)),
            Operation::Cal => self.parse_instruction(Addr, span, |o| o.into_addr(Cal)),

            // Condition + address instructions
            Operation::Brh => self.parse_instruction(CondAddr, span, |o| o.into_cond_addr(Brh)),

            // Skip
            Operation::Skp => self.parse_instruction(Skip, span, |o| o.into_skip(Skp)),

            // Memory instructions with optional offset
            Operation::Lod => self.parse_instruction(Reg2Offset, span, |o| o.into_reg2_offset(Lod)),
            Operation::Str => self.parse_instruction(Reg2Offset, span, |o| o.into_reg2_offset(Str)),
        }
    }
}

/// Intermediate representation of parsed operands
enum ParsedOperands {
    None,
    Reg1(Register),
    Reg2(Register, Register),
    Reg3(Register, Register, Register),
    RegImm(Register, Immediate),
    Addr(Address),
    CondAddr(Condition, Address),
    Reg2Offset(Register, Register, Option<Offset>),
    Skip(SkipFlag),
}

impl ParsedOperands {
    fn into_reg1(self, f: fn(Register) -> OperationWithArgs) -> OperationWithArgs {
        match self {
            ParsedOperands::Reg1(r) => f(r),
            _ => unreachable!("Expected Reg1 operands"),
        }
    }

    fn into_reg2(self, f: fn(Register, Register) -> OperationWithArgs) -> OperationWithArgs {
        match self {
            ParsedOperands::Reg2(r1, r2) => f(r1, r2),
            _ => unreachable!("Expected Reg2 operands"),
        }
    }

    fn into_reg3(
        self,
        f: fn(Register, Register, Register) -> OperationWithArgs,
    ) -> OperationWithArgs {
        match self {
            ParsedOperands::Reg3(r1, r2, r3) => f(r1, r2, r3),
            _ => unreachable!("Expected Reg3 operands"),
        }
    }

    fn into_reg_imm(self, f: fn(Register, Immediate) -> OperationWithArgs) -> OperationWithArgs {
        match self {
            ParsedOperands::RegImm(r, imm) => f(r, imm),
            _ => unreachable!("Expected RegImm operands"),
        }
    }

    fn into_addr(self, f: fn(Address) -> OperationWithArgs) -> OperationWithArgs {
        match self {
            ParsedOperands::Addr(addr) => f(addr),
            _ => unreachable!("Expected Addr operands"),
        }
    }

    fn into_cond_addr(self, f: fn(Condition, Address) -> OperationWithArgs) -> OperationWithArgs {
        match self {
            ParsedOperands::CondAddr(cond, addr) => f(cond, addr),
            _ => unreachable!("Expected CondAddr operands"),
        }
    }

    fn into_skip(self, f: fn(SkipFlag) -> OperationWithArgs) -> OperationWithArgs {
        match self {
            ParsedOperands::Skip(skip) => f(skip),
            _ => unreachable!("Expected CondAddr operands"),
        }
    }

    fn into_reg2_offset(
        self,
        f: fn(Register, Register, Option<Offset>) -> OperationWithArgs,
    ) -> OperationWithArgs {
        match self {
            ParsedOperands::Reg2Offset(r1, r2, offset) => f(r1, r2, offset),
            _ => unreachable!("Expected Reg2Offset operands"),
        }
    }
}
