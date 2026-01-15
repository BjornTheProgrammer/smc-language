use arbitrary_int::{u4, u10};
use thiserror::Error;

use crate::{
    assembler::backends::Backend,
    lexer::token::{Condition, Span},
    parser::{
        DefineMap, LabelMap, ParserError, ParserResult,
        operations::{Address, Immediate, Offset, OperationWithArgs, SpannedOperation},
    },
};

pub mod backends;

pub struct Assembler {
    parser_results: ParserResult,
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
}

impl Assembler {
    pub fn new(target: Backend, parser_results: ParserResult) -> Self {
        Assembler { parser_results }
    }

    pub fn assemble(mut self) -> Result<Vec<u16>, Vec<AssemblerError>> {
        let mut bytes = Vec::new();
        let mut errors = Vec::new();

        let original_errors = std::mem::take(&mut self.parser_results.errors);
        for error in original_errors {
            errors.push(AssemblerError::ParserError(error));
        }

        let operations = std::mem::take(&mut self.parser_results.operations);

        for SpannedOperation { op, span } in operations {
            match self.assemble_operation(op, span) {
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

    fn assemble_operation(
        &self,
        operation: OperationWithArgs,
        span: Span,
    ) -> Result<u16, AssemblerError> {
        match operation {
            OperationWithArgs::Nop => Ok(0b0000 << 12),
            OperationWithArgs::Hlt => Ok(0b0001 << 12),
            OperationWithArgs::Add3(r1, r2, r3) => Ok(0b0010 << 12
                | (((r1.value() & 0xF) as u16) << 8)
                | (((r2.value() & 0xF) as u16) << 4)
                | ((r3.value() & 0xF) as u16)),
            OperationWithArgs::Sub3(r1, r2, r3) => Ok(0b0011 << 12
                | (((r1.value() & 0xF) as u16) << 8)
                | (((r2.value() & 0xF) as u16) << 4)
                | ((r3.value() & 0xF) as u16)),
            OperationWithArgs::Nor3(r1, r2, r3) => Ok(0b0100 << 12
                | (((r1.value() & 0xF) as u16) << 8)
                | (((r2.value() & 0xF) as u16) << 4)
                | ((r3.value() & 0xF) as u16)),
            OperationWithArgs::And3(r1, r2, r3) => Ok(0b0101 << 12
                | (((r1.value() & 0xF) as u16) << 8)
                | (((r2.value() & 0xF) as u16) << 4)
                | ((r3.value() & 0xF) as u16)),
            OperationWithArgs::Xor3(r1, r2, r3) => Ok(0b0110 << 12
                | (((r1.value() & 0xF) as u16) << 8)
                | (((r2.value() & 0xF) as u16) << 4)
                | ((r3.value() & 0xF) as u16)),
            OperationWithArgs::Rsh2(r1, r2) => {
                Ok(0b0111 << 12 | (((r1.value() & 0xF) as u16) << 8) | ((r2.value() & 0xF) as u16))
            }
            OperationWithArgs::Ldi2(r1, immediate) => Ok(0b1000 << 12
                | (((r1.value() & 0xF) as u16) << 8)
                | (get_immediate_value(span, &self.parser_results.defines, immediate)? as u8
                    as u16)),
            OperationWithArgs::Adi2(r1, immediate) => Ok(0b1001 << 12
                | (((r1.value() & 0xF) as u16) << 8)
                | (get_immediate_value(span, &self.parser_results.defines, immediate)? as u8
                    as u16)),
            OperationWithArgs::Jmp(address) => Ok(0b1010 << 12
                | (get_address_value(
                    span,
                    &self.parser_results.defines,
                    &self.parser_results.labels,
                    address,
                )?
                .value()
                    & 0b0000_0011_1111_1111)),
            OperationWithArgs::Brh(condition, address) => {
                let condition = match condition {
                    Condition::Equal => 0b00,
                    Condition::NotEqual => 0b01,
                    Condition::GreaterEqual => 0b10,
                    Condition::Less => 0b11,
                };

                Ok(0b1011 << 12
                    | (condition << 10)
                    | (get_address_value(
                        span,
                        &self.parser_results.defines,
                        &self.parser_results.labels,
                        address,
                    )?
                    .value()
                        & 0b0000_0011_1111_1111))
            }
            OperationWithArgs::Cal(address) => Ok(0b1100 << 12
                | (get_address_value(
                    span,
                    &self.parser_results.defines,
                    &self.parser_results.labels,
                    address,
                )?
                .value()
                    & 0b0000_0011_1111_1111)),
            OperationWithArgs::Ret => Ok(0b1101_0000_0000_0000),
            OperationWithArgs::Lod(r1, r2, offset) => {
                let offset = match offset {
                    Some(offset) => get_offset_value(span, &self.parser_results.defines, offset)?,
                    None => u4::from_u8(0b0000u8),
                };

                Ok(0b1110 << 12
                    | (((r1.value() & 0xF) as u16) << 8)
                    | (((r2.value() & 0xF) as u16) << 4)
                    | ((offset.value() & 0xF) as u16))
            }
            OperationWithArgs::Str(r1, r2, offset) => {
                let offset = match offset {
                    Some(offset) => get_offset_value(span, &self.parser_results.defines, offset)?,
                    None => u4::from_u8(0b0000u8),
                };

                Ok(0b1111 << 12
                    | (((r1.value() & 0xF) as u16) << 8)
                    | (((r2.value() & 0xF) as u16) << 4)
                    | ((offset.value() & 0xF) as u16))
            }
            _ => Err(AssemblerError::UnsupportedOperation(span, operation)),
        }
    }
}

fn get_offset_value(span: Span, defines: &DefineMap, offset: Offset) -> Result<u4, AssemblerError> {
    Ok(match offset {
        Offset::Value(val) => val,
        Offset::Define(identifier) => u4::from_u8(
            *defines
                .get(&identifier)
                .ok_or(AssemblerError::DefineNotFound(span, identifier))? as i8 as u8
                & 0x0F,
        ),
    })
}

fn get_address_value(
    span: Span,
    defines: &DefineMap,
    labels: &LabelMap,
    address: Address,
) -> Result<u10, AssemblerError> {
    Ok(match address {
        Address::Value(val) => val,
        Address::Define(identifier) => u10::from_u16(
            *defines
                .get(&identifier)
                .ok_or(AssemblerError::DefineNotFound(span, identifier))? as i32 as u16,
        ),
        Address::Label(identifier) => u10::from_u16(
            *labels
                .get(&identifier)
                .ok_or(AssemblerError::LabelNotFound(span, identifier))? as u16,
        ),
    })
}

fn get_immediate_value(
    span: Span,
    defines: &DefineMap,
    immediate: Immediate,
) -> Result<i8, AssemblerError> {
    Ok(match immediate {
        Immediate::Value(val) => val,
        Immediate::Define(identifier) => *defines
            .get(&identifier)
            .ok_or(AssemblerError::DefineNotFound(span, identifier))?
            as i32 as i8,
    })
}
