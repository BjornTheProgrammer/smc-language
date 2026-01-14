use arbitrary_int::{u4, u10};
use thiserror::Error;

use crate::{
    lexer::token::{Condition, Span},
    parser::{
        DefineMap, LabelMap, ParserError, ParserResult,
        operations::{
            Address, Immediate, LoweredOperationWithArgs, Offset, OperationWithArgs,
            PseudoOperationWithArgs,
        },
    },
};

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
}

impl Assembler {
    pub fn new(parser_results: ParserResult) -> Self {
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

        let operations = operations
            .into_iter()
            .map(|operation| match operation {
                OperationWithArgs::Lowered(lowered_operation_with_args, span) => {
                    (lowered_operation_with_args, span)
                }
                OperationWithArgs::Pseudo(pseudo_operation_with_args, span) => {
                    let r0 = u4::from_u8(0u8);
                    let op = match pseudo_operation_with_args {
                        PseudoOperationWithArgs::Cmp(a, b) => {
                            LoweredOperationWithArgs::Sub(a, b, r0)
                        }
                        PseudoOperationWithArgs::Mov(a, c) => {
                            LoweredOperationWithArgs::Add(a, r0, c)
                        }
                        PseudoOperationWithArgs::Lsh(a, c) => {
                            LoweredOperationWithArgs::Add(a, a, c)
                        }
                        PseudoOperationWithArgs::Inc(a) => {
                            LoweredOperationWithArgs::Adi(a, Immediate::Value(1))
                        }
                        PseudoOperationWithArgs::Dec(a) => {
                            LoweredOperationWithArgs::Adi(a, Immediate::Value(-1))
                        }
                        PseudoOperationWithArgs::Not(a, c) => {
                            LoweredOperationWithArgs::Nor(a, r0, c)
                        }
                        PseudoOperationWithArgs::Neg(a, c) => {
                            LoweredOperationWithArgs::Sub(r0, a, c)
                        }
                    };

                    (op, span)
                }
            })
            .collect::<Vec<_>>();

        for (operation, span) in operations {
            match self.assemble_operation(operation, span) {
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
        operation: LoweredOperationWithArgs,
        span: Span,
    ) -> Result<u16, AssemblerError> {
        match operation {
            LoweredOperationWithArgs::Nop => Ok(0b0000 << 12),
            LoweredOperationWithArgs::Hlt => Ok(0b0001 << 12),
            LoweredOperationWithArgs::Add(r1, r2, r3) => Ok(0b0010 << 12
                | (((r1.value() & 0xF) as u16) << 8)
                | (((r2.value() & 0xF) as u16) << 4)
                | ((r3.value() & 0xF) as u16)),
            LoweredOperationWithArgs::Sub(r1, r2, r3) => Ok(0b0011 << 12
                | (((r1.value() & 0xF) as u16) << 8)
                | (((r2.value() & 0xF) as u16) << 4)
                | ((r3.value() & 0xF) as u16)),
            LoweredOperationWithArgs::Nor(r1, r2, r3) => Ok(0b0100 << 12
                | (((r1.value() & 0xF) as u16) << 8)
                | (((r2.value() & 0xF) as u16) << 4)
                | ((r3.value() & 0xF) as u16)),
            LoweredOperationWithArgs::And(r1, r2, r3) => Ok(0b0101 << 12
                | (((r1.value() & 0xF) as u16) << 8)
                | (((r2.value() & 0xF) as u16) << 4)
                | ((r3.value() & 0xF) as u16)),
            LoweredOperationWithArgs::Xor(r1, r2, r3) => Ok(0b0110 << 12
                | (((r1.value() & 0xF) as u16) << 8)
                | (((r2.value() & 0xF) as u16) << 4)
                | ((r3.value() & 0xF) as u16)),
            LoweredOperationWithArgs::Rsh(r1, r2) => {
                Ok(0b0111 << 12 | (((r1.value() & 0xF) as u16) << 8) | ((r2.value() & 0xF) as u16))
            }
            LoweredOperationWithArgs::Ldi(r1, immediate) => Ok(0b1000 << 12
                | (((r1.value() & 0xF) as u16) << 8)
                | (get_immediate_value(span, &self.parser_results.defines, immediate)? as u8
                    as u16)),
            LoweredOperationWithArgs::Adi(r1, immediate) => Ok(0b1001 << 12
                | (((r1.value() & 0xF) as u16) << 8)
                | (get_immediate_value(span, &self.parser_results.defines, immediate)? as u8
                    as u16)),
            LoweredOperationWithArgs::Jmp(address) => Ok(0b1010 << 12
                | (get_address_value(
                    span,
                    &self.parser_results.defines,
                    &self.parser_results.labels,
                    address,
                )?
                .value()
                    & 0b0000_0011_1111_1111)),
            LoweredOperationWithArgs::Brh(condition, address) => {
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
            LoweredOperationWithArgs::Cal(address) => Ok(0b1100 << 12
                | (get_address_value(
                    span,
                    &self.parser_results.defines,
                    &self.parser_results.labels,
                    address,
                )?
                .value()
                    & 0b0000_0011_1111_1111)),
            LoweredOperationWithArgs::Ret => Ok(0b1101_0000_0000_0000),
            LoweredOperationWithArgs::Lod(r1, r2, offset) => {
                let offset = match offset {
                    Some(offset) => get_offset_value(span, &self.parser_results.defines, offset)?,
                    None => u4::from_u8(0b0000u8),
                };

                Ok(0b1110 << 12
                    | (((r1.value() & 0xF) as u16) << 8)
                    | (((r2.value() & 0xF) as u16) << 4)
                    | ((offset.value() & 0xF) as u16))
            }
            LoweredOperationWithArgs::Str(r1, r2, offset) => {
                let offset = match offset {
                    Some(offset) => get_offset_value(span, &self.parser_results.defines, offset)?,
                    None => u4::from_u8(0b0000u8),
                };

                Ok(0b1111 << 12
                    | (((r1.value() & 0xF) as u16) << 8)
                    | (((r2.value() & 0xF) as u16) << 4)
                    | ((offset.value() & 0xF) as u16))
            }
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
