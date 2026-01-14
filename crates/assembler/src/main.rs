use std::{fs, path::PathBuf};

use anyhow::{Result, bail};
use clap::{Parser as ClapParser, Subcommand};
use tracing::instrument;

use crate::{
    assembler::{Assembler, AssemblerError},
    lexer::{Lexer, LexerError},
    parser::{Parser, ParserError},
};

pub mod assembler;
pub mod lexer;
pub mod parser;

#[derive(ClapParser)]
#[command(version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Compiles the given source file
    Compile {
        /// Path to the input file
        input: String,

        /// Path of the output file
        output: String,
    },
}

#[instrument]
fn main() -> Result<()> {
    let cli = Cli::parse();

    match &cli.command {
        Commands::Compile { input, output } => {
            let input = PathBuf::from(input);
            if !input.exists() {
                bail!("Specified path does not exist!");
            }

            let source = fs::read_to_string(&input)?;

            let tokens: Vec<_> = Lexer::new(&source).into_iter().collect();

            fs::write(
                "tokens.txt",
                tokens
                    .iter()
                    .map(|token| format!("{:?}", token))
                    .collect::<Vec<String>>()
                    .join("\n"),
            )?;

            let parsed = Parser::new(tokens).parse();

            fs::write(
                "operations.txt",
                parsed
                    .operations
                    .iter()
                    .map(|parsed| format!("{:?}", parsed))
                    .collect::<Vec<String>>()
                    .join("\n"),
            )?;

            fs::write(
                "labels.txt",
                parsed
                    .labels
                    .iter()
                    .map(|parsed| format!("{:?}", parsed))
                    .collect::<Vec<String>>()
                    .join("\n"),
            )?;

            fs::write(
                "defines.txt",
                parsed
                    .defines
                    .iter()
                    .map(|parsed| format!("{:?}", parsed))
                    .collect::<Vec<String>>()
                    .join("\n"),
            )?;

            let assembler = Assembler::new(parsed);
            let result = assembler.assemble();

            match result {
                Ok(result) => {
                    let output = PathBuf::from(output);
                    let bytes: Vec<u8> = result
                        .into_iter()
                        .flat_map(|word| word.to_le_bytes())
                        .collect();
                    fs::write(&output, bytes)?;
                    println!("Output written to: {}", output.display());
                }
                Err(errors) => {
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
                                },
                                ParserError::DuplicateDefine(span, _) => span,
                                ParserError::DuplicateLabel(span, _) => span,
                                ParserError::ExpectedButReceived(span, _, _) => span,
                                ParserError::UnexpectedEof(span) => span,
                            },
                        };

                        eprintln!("{}", span.format_error(&input, &source, &err.to_string()));
                    }
                    bail!("Compilation failed");
                }
            }
        }
    }

    Ok(())
}
