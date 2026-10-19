use std::path::Path;

use smc_assembler::{
    Assembled, CompileError,
    assembler::{AssembledInstruction, backends::LyminalUniNone},
    compile_with_source_map,
};

use crate::emulators::{batpu_2::Batpu2Emulator, lyminal::Lyminal};

pub mod batpu_2;
pub mod lyminal;

pub enum Emulator {
    Batpu2(Batpu2Emulator),
    Lyminal(Box<Lyminal>),
}

impl Default for Emulator {
    /// An empty Lyminal, the only machine that runs programs so far
    fn default() -> Self {
        Self::Lyminal(Box::new(Lyminal::new()))
    }
}

impl Emulator {
    /// Loads a program, picking the machine from the file extension. Lyminal assembly
    /// (`.lasm`) is assembled first, and comes with its listing; `.lym` is one binary byte
    /// per line.
    pub fn load(path: &Path) -> Result<(Self, Option<Listing>), String> {
        let source = std::fs::read_to_string(path)
            .map_err(|e| format!("Cannot read {}: {e}", path.display()))?;

        let (program, listing) = match path.extension().and_then(|ext| ext.to_str()) {
            Some("lasm") => {
                let assembled = assemble_lyminal(&source)?;
                let listing = Listing::new(&source, &assembled.instructions);
                (assembled.bytes, Some(listing))
            }
            Some("lym") => (parse_binary_lines(&source)?, None),
            _ => return Err(format!("Unsupported program `{}`", path.display())),
        };

        let mut cpu = Box::new(Lyminal::new());
        cpu.load_program(&program);
        Ok((Self::Lyminal(cpu), listing))
    }
}

/// A program's source, line by line, with the ROM bytes each line assembled to
#[derive(Debug, Clone, Default)]
pub struct Listing {
    pub lines: Vec<ListingLine>,
}

#[derive(Debug, Clone)]
pub struct ListingLine {
    pub text: String,
    /// Where its instruction starts in ROM, if it assembled to any bytes
    pub address: Option<usize>,
    /// What it assembled to; ROM may have been edited since
    pub bytes: Vec<u8>,
}

impl Listing {
    fn new(source: &str, instructions: &[AssembledInstruction]) -> Self {
        let mut lines: Vec<_> = source
            .lines()
            .map(|text| ListingLine {
                text: text.trim_end().to_string(),
                address: None,
                bytes: Vec::new(),
            })
            .collect();

        // A line holds at most one instruction
        for instruction in instructions.iter().filter(|i| !i.bytes.is_empty()) {
            let start = instruction.span.start().min(source.len());
            let number = source[..start].matches('\n').count();
            if let Some(line) = lines.get_mut(number) {
                line.address = Some(instruction.address);
                line.bytes = instruction.bytes.clone();
            }
        }

        Self { lines }
    }

    /// The line whose instruction includes the ROM byte at `address`
    pub fn line_at(&self, address: usize) -> Option<usize> {
        self.lines.iter().position(|line| {
            line.address
                .is_some_and(|start| (start..start + line.bytes.len()).contains(&address))
        })
    }
}

fn assemble_lyminal(source: &str) -> Result<Assembled, String> {
    compile_with_source_map(source, &LyminalUniNone).map_err(|error| match error {
        CompileError::AssembleError(errors) => errors
            .iter()
            .map(|error| format!("Line {}: {error}", error_line(source, error)))
            .collect::<Vec<_>>()
            .join("\n"),
        error => error.to_string(),
    })
}

/// The 1-based line an assembler error points at
fn error_line(source: &str, error: &smc_assembler::assembler::AssemblerError) -> usize {
    use smc_assembler::{
        assembler::AssemblerError as E, lexer::LexerError as L, parser::ParserError as P,
    };

    let span = match error {
        E::DefineNotFound(span, _)
        | E::LabelNotFound(span, _)
        | E::UnsupportedOperation(span, _)
        | E::InvalidRegister(span, _)
        | E::AddressOutOfRange(span, _)
        | E::OffsetOutOfRange(span, _)
        | E::InvalidCondition(span, _)
        | E::ImmediateOutOfRange(span, _)
        | E::InvalidOperands(span, _) => span,
        E::ParserError(error) => match error {
            P::SyntaxError(error) => match error {
                L::InvalidNumber(span, _)
                | L::UnexpectedCharacter(span, _)
                | L::ExpectedCharacter(span, _)
                | L::InvalidOffset(span, _) => span,
            },
            P::DuplicateDefine(span, _)
            | P::DuplicateLabel(span, _)
            | P::ExpectedButReceived(span, _, _)
            | P::UnexpectedEof(span) => span,
        },
    };

    let offset = span.start().min(source.len());
    source[..offset].matches('\n').count() + 1
}

fn parse_binary_lines(source: &str) -> Result<Vec<u8>, String> {
    source
        .lines()
        .enumerate()
        .filter(|(_, line)| !line.trim().is_empty())
        .map(|(i, line)| {
            u8::from_str_radix(line.trim(), 2)
                .map_err(|_| format!("Line {}: `{}` is not a binary byte", i + 1, line.trim()))
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    const HELLO: &str = concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../smc-assembler/tests/lyminal"
    );

    fn rom(path: &str) -> Vec<u8> {
        match Emulator::load(Path::new(path))
            .expect("program should load")
            .0
        {
            Emulator::Lyminal(cpu) => cpu.rom().to_vec(),
            Emulator::Batpu2(_) => panic!("expected a Lyminal machine"),
        }
    }

    #[test]
    fn assembled_and_binary_programs_match() {
        assert_eq!(
            rom(&format!("{HELLO}/programs/hello-world.lasm")),
            rom(&format!("{HELLO}/expected/hello-world.lym"))
        );
    }

    #[test]
    fn reports_assembly_errors_by_line() {
        let error = assemble_lyminal("MOV X Y\nSWAP X [Y] 1").unwrap_err();
        assert!(error.starts_with("Line 2: "), "{error}");
    }
}
