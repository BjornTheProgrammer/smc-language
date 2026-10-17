use std::fmt;

use crate::{
    assembler::{
        AssemblerError, LabelMap, get_address_value, get_immediate_value, get_offset_value,
    },
    lexer::token::{Register, Span},
    parser::{
        DefineMap, ParsedItem,
        operations::{Address, Immediate, Offset, OperationWithArgs},
    },
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

    /// Bytes per memory address, which label addresses are counted in
    fn bytes_per_address(&self) -> usize {
        1
    }

    /// Adds target provided defines, such as port names
    fn insert_before(&self, _defines: &mut DefineMap) -> Result<(), AssemblerError> {
        Ok(())
    }

    /// Lowers the whole program to machine instructions, keeping labels in place so the
    /// assembler can lay it out. Seeing the whole program lets a target enforce rules
    /// that span several instructions.
    fn lower(&self, items: &[ParsedItem]) -> (Vec<Lowered>, Vec<AssemblerError>);
}

/// A program after lowering
pub enum Lowered {
    Label(String),
    /// The machine instructions one source operation lowered to
    Operation {
        span: Span,
        machines: Vec<Machine>,
    },
}

type Encoder = Box<dyn FnOnce(&Resolver) -> Result<Vec<u8>, AssemblerError> + Send>;

/// A machine instruction whose size is known before labels are, so the program can be
/// laid out first and encoded once every label has an address
pub struct Machine {
    size: usize,
    encode: Encoder,
}

impl Machine {
    /// An instruction that needs defines or labels to encode. `encode` must produce
    /// exactly `size` bytes.
    pub fn new(
        size: usize,
        encode: impl FnOnce(&Resolver) -> Result<Vec<u8>, AssemblerError> + Send + 'static,
    ) -> Self {
        Self {
            size,
            encode: Box::new(encode),
        }
    }

    /// An instruction that is already fully encoded
    pub fn bytes(bytes: Vec<u8>) -> Self {
        Self::new(bytes.len(), move |_| Ok(bytes))
    }

    pub fn size(&self) -> usize {
        self.size
    }

    pub fn encode(self, resolver: &Resolver) -> Result<Vec<u8>, AssemblerError> {
        let bytes = (self.encode)(resolver)?;
        if bytes.len() != self.size {
            return Err(AssemblerError::InvalidOperands(
                resolver.span.clone(),
                format!(
                    "internal error: encoded {} bytes, but {} were laid out",
                    bytes.len(),
                    self.size
                ),
            ));
        }
        Ok(bytes)
    }
}

/// Looks up define and label values for an operation once the program is laid out
pub struct Resolver<'a> {
    pub defines: &'a DefineMap,
    pub labels: &'a LabelMap,
    pub span: &'a Span,
}

impl Resolver<'_> {
    pub fn immediate(&self, immediate: Immediate) -> Result<i128, AssemblerError> {
        get_immediate_value(self.span, self.defines, immediate)
    }

    pub fn address(&self, address: Address) -> Result<i128, AssemblerError> {
        get_address_value(self.span, self.defines, self.labels, address)
    }

    pub fn offset(&self, offset: Offset) -> Result<i128, AssemblerError> {
        get_offset_value(self.span, self.defines, offset)
    }
}

/// Lowers each operation on its own, for targets without rules spanning instructions
pub fn lower_each(
    items: &[ParsedItem],
    mut lower_operation: impl FnMut(&OperationWithArgs, &Span) -> Result<Vec<Machine>, AssemblerError>,
) -> (Vec<Lowered>, Vec<AssemblerError>) {
    let mut lowered = Vec::new();
    let mut errors = Vec::new();

    for item in items {
        match item {
            ParsedItem::Label(name, _) => lowered.push(Lowered::Label(name.clone())),
            ParsedItem::Operation(spanned) => match lower_operation(&spanned.op, &spanned.span) {
                Ok(machines) => lowered.push(Lowered::Operation {
                    span: spanned.span.clone(),
                    machines,
                }),
                Err(error) => errors.push(error),
            },
        }
    }

    (lowered, errors)
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
