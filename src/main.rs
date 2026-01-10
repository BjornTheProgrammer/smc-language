use std::{fs, path::PathBuf};

use anyhow::{Result, bail};
use clap::{Parser, Subcommand};
use tracing::instrument;

use crate::{
    assembler::Assembler,
    lexer::{Lexer, LexerError},
};

pub mod assembler;
pub mod lexer;

#[derive(Parser)]
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

            let source = fs::read_to_string(input)?;

            let lexer = Lexer::new(&source).into_iter();

            let (tokens, errors): (Vec<_>, Vec<_>) = lexer
                .collect::<Vec<_>>()
                .into_iter()
                .partition(Result::is_ok);

            let tokens: Vec<_> = tokens.into_iter().map(Result::unwrap).collect();
            let errors: Vec<_> = errors.into_iter().map(Result::unwrap_err).collect();

            if !errors.is_empty() {
                eprintln!("Compilation failed with {} error(s):\n", errors.len());
                for err in &errors {
                    let span = match err {
                        LexerError::UnknownInstruction(span, _) => span,
                        LexerError::InvalidNumber(span, _) => span,
                        LexerError::InvalidRegister(span, _) => span,
                        LexerError::UnexpectedCharacter(span, _) => span,
                        LexerError::ExpectedCharacter(span, _) => span,
                        LexerError::UnknownCondition(span, _) => span,
                        LexerError::InvalidOffset(span, _) => span,
                    };

                    eprintln!("{}", span.format_error(&source, &err.to_string()));
                }
                bail!("Compilation failed");
            }

            println!("tokens: {:?}", tokens);

            let assembler = Assembler::new(tokens);
            let res = assembler.assemble()?;

            let output = PathBuf::from(output);
            let bytes: Vec<u8> = res
                .into_iter()
                .flat_map(|word| word.to_le_bytes())
                .collect();
            fs::write(&output, bytes)?;
            println!("Output written to: {}", output.display());
        }
    }

    Ok(())
}
