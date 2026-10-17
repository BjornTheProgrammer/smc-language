use clap::{ValueEnum, builder::PossibleValue};
use strum::VariantArray;
use strum_macros::VariantArray;

use crate::{
    assembler::{AssemblerError, LabelMap},
    lexer::token::{Register, Span},
    parser::{DefineMap, ParsedItem, operations::OperationWithArgs},
};

pub mod batpu2_mattbatwings_none;
pub mod lyminal_uni_none;
pub mod tau_analyzers_none;

#[derive(Debug, Clone, PartialEq, VariantArray)]
pub enum Backend {
    BatPU2,
    TauAnalyzersNone,
    LyminalUniNone,
}

impl Backend {
    pub fn insert_before(&self, defines: &mut DefineMap) -> Result<(), AssemblerError> {
        match self {
            Backend::BatPU2 => batpu2_mattbatwings_none::insert_before(defines),
            Backend::TauAnalyzersNone => Ok(()),
            Backend::LyminalUniNone => Ok(()),
        }
    }

    /// Checks rules that span several instructions, before any are assembled
    pub fn validate(&self, items: &[ParsedItem]) -> Vec<AssemblerError> {
        match self {
            Backend::BatPU2 | Backend::TauAnalyzersNone => Vec::new(),
            Backend::LyminalUniNone => lyminal_uni_none::validate(items),
        }
    }

    /// `previous` is the operation directly before `op`, or `None` if a label separates them
    pub fn assemble_operation(
        &self,
        defines: &DefineMap,
        labels: &LabelMap,
        op: OperationWithArgs,
        previous: Option<&OperationWithArgs>,
        span: Span,
    ) -> Result<Vec<u8>, AssemblerError> {
        match self {
            Backend::BatPU2 => Ok(batpu2_mattbatwings_none::assemble_operation(
                defines, labels, op, span,
            )?
            .to_be_bytes()
            .to_vec()),
            Backend::TauAnalyzersNone => {
                tau_analyzers_none::assemble_operation(defines, labels, op, span)
            }
            Backend::LyminalUniNone => {
                lyminal_uni_none::assemble_operation(defines, labels, op, previous, span)
            }
        }
    }

    pub fn instruction_byte_size(
        &self,
        op: &OperationWithArgs,
        previous: Option<&OperationWithArgs>,
    ) -> usize {
        match self {
            Backend::BatPU2 => batpu2_mattbatwings_none::instruction_byte_size(op),
            Backend::TauAnalyzersNone => tau_analyzers_none::instruction_byte_size(op),
            Backend::LyminalUniNone => lyminal_uni_none::instruction_byte_size(op, previous),
        }
    }
}

impl ValueEnum for Backend {
    fn value_variants<'a>() -> &'a [Self] {
        Backend::VARIANTS
    }

    fn to_possible_value(&self) -> Option<PossibleValue> {
        Some(PossibleValue::new(self.to_str()))
    }
}

impl Backend {
    pub fn to_str(&self) -> &'static str {
        match self {
            Backend::BatPU2 => "batpu2-mattbatwings-none",
            Backend::TauAnalyzersNone => "tau-analyzers-none",
            Backend::LyminalUniNone => "lyminal-uni-none",
        }
    }
}

impl Register {
    fn check(&self, backend: &Backend, span: &Span) -> Result<u8, AssemblerError> {
        match backend {
            Backend::BatPU2 => match self {
                Register(0..=15) => Ok(self.0),
                Register(_) => Err(AssemblerError::InvalidRegister(span.clone(), self.0)),
            },
            Backend::TauAnalyzersNone => match self {
                Register(0..=3) => Ok(self.0),
                Register(_) => Err(AssemblerError::InvalidRegister(span.clone(), self.0)),
            },
            Backend::LyminalUniNone => match self {
                Register(0..=3) => Ok(self.0),
                Register(_) => Err(AssemblerError::InvalidRegister(span.clone(), self.0)),
            },
        }
    }
}
