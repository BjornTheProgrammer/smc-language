use thiserror::Error;

use crate::{
    assembler::backends::Backend,
    lexer::token::Span,
    parser::{
        DefineMap, LabelMap, ParserError, ParserResult,
        operations::{Address, Immediate, Offset, OperationWithArgs, SpannedOperation},
    },
};

pub mod backends;

pub struct Assembler {
    parser_results: ParserResult,
    target: Backend,
}

#[derive(Error, Debug)]
pub enum AssemblerError {
    #[error("AssemblerError: Failed to get define value for {1}")]
    DefineNotFound(Span, String),

    #[error("AssemblerError: Failed to get label value for {1}")]
    LabelNotFound(Span, String),

    #[error("{0}")]
    ParserError(#[from] ParserError),

    #[error("AssemblerError: Unsupported operation {1:?}")]
    UnsupportedOperation(Span, OperationWithArgs),

    #[error("AssemblerError: Invalid register `{1}`")]
    InvalidRegister(Span, u8),

    #[error("AssemblerError: Address out of range {1}")]
    AddressOutOfRange(Span, i128),

    #[error("AssemblerError: Offset out of range {1}")]
    OffsetOutOfRange(Span, i128),
}

impl Assembler {
    pub fn new(target: Backend, parser_results: ParserResult) -> Self {
        Assembler {
            parser_results,
            target,
        }
    }

    pub fn assemble(mut self) -> Result<Vec<u16>, Vec<AssemblerError>> {
        let mut bytes = Vec::new();
        let mut errors = Vec::new();

        let original_errors = std::mem::take(&mut self.parser_results.errors);
        for error in original_errors {
            errors.push(AssemblerError::ParserError(error));
        }

        let operations = std::mem::take(&mut self.parser_results.operations);

        if let Err(e) = self.target.insert_before(
            &mut self.parser_results.defines,
            &mut self.parser_results.labels,
        ) {
            errors.push(e);
        }

        for SpannedOperation { op, span } in operations {
            match self.target.assemble_operation(
                &self.parser_results.defines,
                &self.parser_results.labels,
                op,
                span,
            ) {
                Ok(word) => bytes.push(word),
                Err(e) => errors.push(e),
            }
        }

        if errors.is_empty() {
            Ok(bytes)
        } else {
            Err(errors)
        }
    }
}

pub fn get_offset_value(
    span: &Span,
    defines: &DefineMap,
    offset: Offset,
) -> Result<i128, AssemblerError> {
    Ok(match offset {
        Offset::Value(val) => val,
        Offset::Define(identifier) => *defines
            .get(&identifier)
            .ok_or(AssemblerError::DefineNotFound(span.clone(), identifier))?
            as i128,
    })
}

pub fn get_address_value(
    span: &Span,
    defines: &DefineMap,
    labels: &LabelMap,
    address: Address,
) -> Result<i128, AssemblerError> {
    Ok(match address {
        Address::Value(val) => val,
        Address::Define(identifier) => *defines
            .get(&identifier)
            .ok_or(AssemblerError::DefineNotFound(span.clone(), identifier))?
            as i128,
        Address::Label(identifier) => *labels
            .get(&identifier)
            .ok_or(AssemblerError::LabelNotFound(span.clone(), identifier))?
            as i128,
    })
}

pub fn get_immediate_value(
    span: Span,
    defines: &DefineMap,
    immediate: Immediate,
) -> Result<i128, AssemblerError> {
    Ok(match immediate {
        Immediate::Value(val) => val,
        Immediate::Define(identifier) => *defines
            .get(&identifier)
            .ok_or(AssemblerError::DefineNotFound(span, identifier))?
            as i128,
    })
}
