use anyhow::Result;
use clap::{Parser as ClapParser, Subcommand};
use smc_assembler::{assembler::backends::Backend, compile_to_file};
use tracing::instrument;

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

        /// Target backend
        #[arg(short, long)]
        target: Backend,

        /// Generate debug artifacts
        #[arg(long)]
        debug_artifacts: bool,
    },
}

#[instrument]
fn main() -> Result<()> {
    let cli = Cli::parse();

    match &cli.command {
        Commands::Compile {
            input,
            output,
            target,
            debug_artifacts,
        } => compile_to_file(input, output, target.clone(), *debug_artifacts)?,
    }

    Ok(())
}
