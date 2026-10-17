use anyhow::Result;
use clap::{
    Parser as ClapParser, Subcommand,
    builder::{PossibleValuesParser, TypedValueParser},
};
use smc_assembler::{
    assembler::backends::{self, TARGETS, Target},
    compile_to_file,
    save::memory::Format,
};
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
        #[arg(short, long, value_parser = target_parser())]
        target: &'static dyn Target,

        /// Instruction memory format for schematic file
        #[arg(short, long)]
        format: Option<Format>,

        /// Generate debug artifacts
        #[arg(long)]
        debug_artifacts: bool,
    },
}

fn target_parser() -> impl TypedValueParser<Value = &'static dyn Target> {
    PossibleValuesParser::new(TARGETS.iter().map(|target| target.name())).map(|name| {
        backends::find_by_name(&name).expect("possible values are registered target names")
    })
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
            format,
        } => compile_to_file(input, output, *target, *debug_artifacts, *format)?,
    }

    Ok(())
}
