use std::fmt;

use crate::{
    assembler::{AssemblerError, LabelMap},
    lexer::token::{Register, Span},
    parser::{DefineMap, ParsedItem, operations::OperationWithArgs},
};

pub mod batpu2_mattbatwings_none;
pub mod lyminal_uni_none;
pub mod tau_analyzers_none;

pub use batpu2_mattbatwings_none::BatPu2;
pub use lyminal_uni_none::LyminalUniNone;
pub use tau_analyzers_none::TauAnalyzersNone;

/// Every target the assembler supports. The CLI and LSP discover targets from here.
pub static TARGETS: &[&dyn Target] = &[&BatPu2, &TauAnalyzersNone, &LyminalUniNone];

/// An instruction set the assembler can produce machine code for
pub trait Target: Sync {
    /// Name used to select the target on the command line
    fn name(&self) -> &'static str;

    /// Source file extensions the LSP assembles with this target
    fn extensions(&self) -> &'static [&'static str];

    /// Adds target provided defines, such as port names
    fn insert_before(&self, _defines: &mut DefineMap) -> Result<(), AssemblerError> {
        Ok(())
    }

    /// Checks rules that span several instructions, before any are assembled
    fn validate(&self, _items: &[ParsedItem]) -> Vec<AssemblerError> {
        Vec::new()
    }

    /// Size of `op` in bytes. `previous` is the operation directly before it, or `None`
    /// if a label separates them.
    fn instruction_byte_size(
        &self,
        op: &OperationWithArgs,
        previous: Option<&OperationWithArgs>,
    ) -> usize;

    /// `previous` is the operation directly before `op`, or `None` if a label separates them
    fn assemble_operation(
        &self,
        defines: &DefineMap,
        labels: &LabelMap,
        op: OperationWithArgs,
        previous: Option<&OperationWithArgs>,
        span: Span,
    ) -> Result<Vec<u8>, AssemblerError>;
}

impl fmt::Debug for dyn Target {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.name())
    }
}

pub fn find_by_name(name: &str) -> Option<&'static dyn Target> {
    TARGETS.iter().copied().find(|target| target.name() == name)
}

pub fn find_by_extension(extension: &str) -> Option<&'static dyn Target> {
    TARGETS.iter().copied().find(|target| {
        target
            .extensions()
            .iter()
            .any(|ext| ext.eq_ignore_ascii_case(extension))
    })
}

impl Register {
    /// Returns the register number if the target has that many registers
    fn check(&self, register_count: u8, span: &Span) -> Result<u8, AssemblerError> {
        if self.0 < register_count {
            Ok(self.0)
        } else {
            Err(AssemblerError::InvalidRegister(span.clone(), self.0))
        }
    }
}
