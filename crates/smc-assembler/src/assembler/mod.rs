use std::collections::HashMap;

use thiserror::Error;

use crate::{
    assembler::backends::{Lowered, Machine, Resolver, Target},
    lexer::token::{Condition, Span},
    parser::{
        DefineMap, DefineSpanMap, LabelSpanMap, ParserError, ParserResult, ReferenceMap,
        operations::{Address, Immediate, Offset},
    },
};

pub mod backends;

pub type LabelMap = HashMap<String, usize>;

#[derive(Debug)]
pub struct Assembler {
    parser_results: ParserResult,
    target: &'static dyn Target,
}

#[derive(Error, Debug)]
pub enum AssemblerError {
    #[error("AssemblerError: Failed to get define value for {1}")]
    DefineNotFound(Span, String),

    #[error("AssemblerError: Failed to get label value for {1}")]
    LabelNotFound(Span, String),

    #[error("{0}")]
    ParserError(#[from] ParserError),

    #[error("AssemblerError: Unknown instruction `{1}` for this target")]
    UnsupportedOperation(Span, String),

    #[error("AssemblerError: Invalid register `{1}`")]
    InvalidRegister(Span, u8),

    #[error("AssemblerError: Address out of range {1}")]
    AddressOutOfRange(Span, i128),

    #[error("AssemblerError: Offset out of range {1}")]
    OffsetOutOfRange(Span, i128),

    #[error("AssemblerError: Invalid Condition for target {1:?}")]
    InvalidCondition(Span, Condition),

    #[error("AssemblerError: Immediate out of range {1}")]
    ImmediateOutOfRange(Span, i128),

    #[error("AssemblerError: {1}")]
    InvalidOperands(Span, String),
}

#[derive(Debug)]
pub struct AssemblerResult {
    pub result: Result<Vec<u8>, Vec<AssemblerError>>,
    pub defines: DefineMap,
    pub labels: LabelMap,
    pub define_spans: DefineSpanMap,
    pub label_spans: LabelSpanMap,
    pub define_references: ReferenceMap,
    pub label_references: ReferenceMap,
}

impl Assembler {
    pub fn new(target: &'static dyn Target, parser_results: ParserResult) -> Self {
        Assembler {
            parser_results,
            target,
        }
    }

    pub fn assemble(mut self) -> AssemblerResult {
        let mut bytes = Vec::new();
        let mut errors = Vec::new();

        let original_errors = std::mem::take(&mut self.parser_results.errors);
        for error in original_errors {
            errors.push(AssemblerError::ParserError(error));
        }

        let items = std::mem::take(&mut self.parser_results.items);

        if let Err(e) = self.target.insert_before(&mut self.parser_results.defines) {
            errors.push(e);
        }

        let (lowered, lower_errors) = self.target.lower(&items);
        errors.extend(lower_errors);

        // Lay out the program, giving every label its address
        let mut labels: LabelMap = HashMap::new();
        let mut address = 0;
        for item in &lowered {
            match item {
                Lowered::Label(name) => {
                    labels.insert(name.clone(), address / self.target.bytes_per_address());
                }
                Lowered::Operation { machines, .. } => {
                    address += machines.iter().map(Machine::size).sum::<usize>();
                }
            }
        }

        for item in lowered {
            let Lowered::Operation { span, machines } = item else {
                continue;
            };
            let resolver = Resolver {
                defines: &self.parser_results.defines,
                labels: &labels,
                span: &span,
            };
            for machine in machines {
                match machine.encode(&resolver) {
                    Ok(mut machine_bytes) => bytes.append(&mut machine_bytes),
                    Err(e) => errors.push(e),
                }
            }
        }

        if errors.is_empty() {
            AssemblerResult {
                result: Ok(bytes),
                defines: self.parser_results.defines,
                labels,
                define_spans: self.parser_results.define_spans,
                label_spans: self.parser_results.label_spans,
                define_references: self.parser_results.define_references,
                label_references: self.parser_results.label_references,
            }
        } else {
            AssemblerResult {
                result: Err(errors),
                defines: self.parser_results.defines,
                labels,
                define_spans: self.parser_results.define_spans,
                label_spans: self.parser_results.label_spans,
                define_references: self.parser_results.define_references,
                label_references: self.parser_results.label_references,
            }
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
            .get(identifier.as_str())
            .ok_or(AssemblerError::LabelNotFound(span.clone(), identifier))?
            as i128,
    })
}

pub fn get_immediate_value(
    span: &Span,
    defines: &DefineMap,
    immediate: Immediate,
) -> Result<i128, AssemblerError> {
    Ok(match immediate {
        Immediate::Value(val) => val,
        Immediate::Define(identifier) => *defines
            .get(&identifier)
            .ok_or(AssemblerError::DefineNotFound(span.clone(), identifier))?
            as i128,
    })
}
