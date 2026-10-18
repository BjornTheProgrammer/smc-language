use std::path::Path;

use smc_assembler::{CompileError, assembler::backends::LyminalUniNone, compile};

use crate::emulators::{batpu_2::Batpu2Emulator, lyminal::Lyminal};

pub mod batpu_2;
pub mod lyminal;

pub enum Emulator {
    Batpu2(Batpu2Emulator),
    Lyminal(Box<Lyminal>),
}

impl Default for Emulator {
    fn default() -> Self {
        Self::Batpu2(Batpu2Emulator::new())
    }
}

impl Emulator {
    /// Loads a program, picking the machine from the file extension. Lyminal assembly
    /// (`.lasm`) is assembled first; `.lym` is one binary byte per line.
    pub fn load(path: &Path) -> Result<Self, String> {
        let source = std::fs::read_to_string(path)
            .map_err(|e| format!("Cannot read {}: {e}", path.display()))?;

        let program = match path.extension().and_then(|ext| ext.to_str()) {
            Some("lasm") => assemble_lyminal(&source)?,
            Some("lym") => parse_binary_lines(&source)?,
            _ => return Err(format!("Unsupported program `{}`", path.display())),
        };

        let mut cpu = Box::new(Lyminal::new());
        cpu.load_program(&program);
        Ok(Self::Lyminal(cpu))
    }
}

fn assemble_lyminal(source: &str) -> Result<Vec<u8>, String> {
    compile(source, &LyminalUniNone, false).map_err(|error| match error {
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
        match Emulator::load(Path::new(path)).expect("program should load") {
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
