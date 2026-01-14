use std::{
    fmt::Write as _,
    fs::{self, OpenOptions},
    io::Write,
    path::{Path, PathBuf},
};

use crate::{
    assembler::{Assembler, AssemblerError},
    lexer::{Lexer, LexerError},
    parser::{Parser, ParserError},
};

pub mod assembler;
pub mod lexer;
pub mod parser;

#[derive(thiserror::Error, Debug)]
pub enum CompileError {
    #[error("Specified path does not exist!")]
    PathDoesNotExist,
    #[error("Failed to read file: {0}")]
    ReadFileError(std::io::Error),
    #[error("Failed to write file: {0}")]
    WriteFileError(std::io::Error),
    #[error("Failed to assemble")]
    AssembleError(Vec<AssemblerError>, String),
    #[error("Compilation failed")]
    CompilationFailed,
}

pub fn compile_to_file(
    input: &str,
    output: &str,
    debug_artifacts: bool,
) -> Result<(), CompileError> {
    let result = compile(input, debug_artifacts);

    match result {
        Ok(result) => {
            let mut file = OpenOptions::new()
                .create(true)
                .truncate(true)
                .write(true)
                .open(&output)
                .map_err(|err| CompileError::WriteFileError(err))?;

            let output = PathBuf::from(output);
            let bytes = result
                .into_iter()
                .map(|word| word.to_be_bytes())
                .collect::<Vec<_>>();

            for byte in bytes {
                let byte1 = byte[0];
                let byte2 = byte[1];
                writeln!(file, "{:08b}{:08b}", byte1, byte2)
                    .map_err(|err| CompileError::WriteFileError(err))?;
            }

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
                        },
                        ParserError::DuplicateDefine(span, _) => span,
                        ParserError::DuplicateLabel(span, _) => span,
                        ParserError::ExpectedButReceived(span, _, _) => span,
                        ParserError::UnexpectedEof(span) => span,
                    },
                };

                eprintln!("{}", span.format_error(&input, &source, &err.to_string()));
            }
            return Err(CompileError::CompilationFailed);
        }
    }
}

pub fn convert_to_mc(input: Vec<u16>) -> Result<String, std::fmt::Error> {
    let bytes = input
        .into_iter()
        .map(|word| word.to_be_bytes())
        .collect::<Vec<_>>();

    let mut output = String::new();
    for byte in bytes {
        let byte1 = byte[0];
        let byte2 = byte[1];
        writeln!(output, "{:08b}{:08b}", byte1, byte2)?;
    }
    Ok(output)
}

pub fn compile<P: AsRef<Path>>(
    input: P,
    generate_debug_artifacts: bool,
) -> Result<Vec<u16>, CompileError> {
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

    let mut parsed = Parser::new(tokens).parse();

    if generate_debug_artifacts {
        fs::write(
            "operations.txt",
            parsed
                .operations
                .iter()
                .map(|parsed| format!("{:?}", parsed))
                .collect::<Vec<String>>()
                .join("\n"),
        )
        .map_err(|err| CompileError::WriteFileError(err))?;

        fs::write(
            "labels.txt",
            parsed
                .labels
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

    let ports = [
        "pixel_x",
        "pixel_y",
        "draw_pixel",
        "clear_pixel",
        "load_pixel",
        "buffer_screen",
        "clear_screen_buffer",
        "write_char",
        "buffer_chars",
        "clear_chars_buffer",
        "show_number",
        "clear_number",
        "signed_mode",
        "unsigned_mode",
        "rng",
        "controller_input",
    ];
    for (i, port) in ports.into_iter().enumerate() {
        parsed.defines.insert(port.to_string(), (i + 240) as f32);
    }

    let assembler = Assembler::new(parsed);
    assembler
        .assemble()
        .map_err(|err| CompileError::AssembleError(err, source))
}
