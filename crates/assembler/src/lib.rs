use std::{fs, path::Path};

use crate::{
    assembler::{Assembler, AssemblerError, backends::Backend},
    lexer::{Lexer, LexerError},
    parser::{Parser, ParserError},
    save::{memory::Format, save_file},
};

pub mod assembler;
pub mod lexer;
pub mod parser;
pub mod save;

#[derive(thiserror::Error, Debug)]
pub enum CompileError {
    #[error("Specified path does not exist!")]
    PathDoesNotExist,
    #[error("Failed to read file: {0}")]
    ReadFileError(std::io::Error),
    #[error("Failed to write file: {0}")]
    WriteFileError(std::io::Error),
    #[error("Failed to format: {0}")]
    FormatError(std::fmt::Error),
    #[error("Failed to assemble")]
    AssembleError(Vec<AssemblerError>, String),
    #[error("Compilation failed")]
    CompilationFailed,
    #[error("Schematic save failed")]
    SchematicSaveFailed(#[from] mc_schem::Error),
    #[error("Unsupported file type")]
    UnsupportedFileType,
    #[error(
        "Missing format when generating schematic, specify format with `--format <FORMAT NAME>`"
    )]
    MissingFormat,
}

pub fn compile_to_file<P1: AsRef<Path>, P2: AsRef<Path>>(
    input: P1,
    output: P2,
    target: Backend,
    debug_artifacts: bool,
    format: Option<Format>,
) -> Result<(), CompileError> {
    let input = input.as_ref();
    let output = output.as_ref();

    let result = compile(input, target, debug_artifacts);

    match result {
        Ok(result) => {
            save_file(output, result, format)?;

            println!("Output written to: {}", output.display());
            return Ok(());
        }
        Err(errors) => {
            let (errors, source) = match errors {
                CompileError::AssembleError(assembler_errors, source) => (assembler_errors, source),
                error @ _ => return Err(error),
            };

            eprintln!("Compilation failed with {} error(s):\n", errors.len());
            for err in &errors {
                let span = match err {
                    AssemblerError::DefineNotFound(span, _) => span,
                    AssemblerError::LabelNotFound(span, _) => span,
                    AssemblerError::ParserError(parser_error) => match parser_error {
                        ParserError::SyntaxError(lexer_error) => match lexer_error {
                            LexerError::InvalidNumber(span, _) => span,
                            LexerError::UnexpectedCharacter(span, _) => span,
                            LexerError::ExpectedCharacter(span, _) => span,
                            LexerError::UnknownCondition(span, _) => span,
                            LexerError::InvalidOffset(span, _) => span,
                            LexerError::InvalidIsaCode(span, _) => span,
                            LexerError::InvalidRegisterNumber(span, _) => span,
                        },
                        ParserError::DuplicateDefine(span, _) => span,
                        ParserError::DuplicateLabel(span, _) => span,
                        ParserError::ExpectedButReceived(span, _, _) => span,
                        ParserError::UnexpectedEof(span) => span,
                        ParserError::InvalidSkip(span, _) => span,
                    },
                    AssemblerError::UnsupportedOperation(span, _) => span,
                    AssemblerError::InvalidRegister(span, _) => span,
                    AssemblerError::AddressOutOfRange(span, _) => span,
                    AssemblerError::OffsetOutOfRange(span, _) => span,
                    AssemblerError::InvalidCondition(span, _) => span,
                    AssemblerError::ImmediateOutOfRange(span, _) => span,
                };

                eprintln!("{}", span.format_error(&input, &source, &err.to_string()));
            }
            return Err(CompileError::CompilationFailed);
        }
    }
}

pub fn compile<P: AsRef<Path>>(
    input: P,
    target: Backend,
    generate_debug_artifacts: bool,
) -> Result<Vec<u8>, CompileError> {
    let input = input.as_ref();
    if !input.exists() {
        return Err(CompileError::PathDoesNotExist.into());
    }

    let source = fs::read_to_string(&input).map_err(|err| CompileError::ReadFileError(err))?;

    let tokens: Vec<_> = Lexer::new(&source).into_iter().collect();

    if generate_debug_artifacts {
        fs::write(
            "tokens.txt",
            tokens
                .iter()
                .map(|token| format!("{:?}", token))
                .collect::<Vec<String>>()
                .join("\n"),
        )
        .map_err(|err| CompileError::WriteFileError(err))?;
    }

    let parsed = Parser::new(tokens).parse();

    if generate_debug_artifacts {
        fs::write(
            "items.txt",
            parsed
                .items
                .iter()
                .map(|parsed| format!("{:?}", parsed))
                .collect::<Vec<String>>()
                .join("\n"),
        )
        .map_err(|err| CompileError::WriteFileError(err))?;

        fs::write(
            "defines.txt",
            parsed
                .defines
                .iter()
                .map(|parsed| format!("{:?}", parsed))
                .collect::<Vec<String>>()
                .join("\n"),
        )
        .map_err(|err| CompileError::WriteFileError(err))?;
    }

    let assembler = Assembler::new(target, parsed);
    assembler
        .assemble()
        .map_err(|err| CompileError::AssembleError(err, source))
}
