use std::path::Path;
use std::thread;

use anyhow::Result;
use clap::{
    Parser as ClapParser, Subcommand,
    builder::{PossibleValuesParser, TypedValueParser},
};
use smc_assembler::{
    CompileError,
    assembler::backends::{self, TARGETS, Target},
    compile_to_file,
    save::memory::Format,
};
use tracing::instrument;

const UPDATE: smc_update::App = smc_update::App {
    name: "smc-assembler",
    version: env!("CARGO_PKG_VERSION"),
};

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

        /// Target backend, inferred from the input file extension if omitted
        #[arg(short, long, value_parser = target_parser())]
        target: Option<&'static dyn Target>,

        /// Instruction memory format for schematic file
        #[arg(short, long)]
        format: Option<Format>,

        /// Generate debug artifacts
        #[arg(long)]
        debug_artifacts: bool,
    },
    /// Updates to the newest release, for copies installed by the release installer
    Update,
}

fn target_parser() -> impl TypedValueParser<Value = &'static dyn Target> {
    PossibleValuesParser::new(TARGETS.iter().map(|target| target.name())).map(|name| {
        backends::find_by_name(&name).expect("possible values are registered target names")
    })
}

#[instrument]
fn main() -> Result<()> {
    let cli = Cli::parse();

    // Asks about newer releases while the command runs, at most once a day
    let notice = match cli.command {
        Commands::Update => None,
        _ => Some(thread::spawn(|| UPDATE.daily_notice())),
    };

    match &cli.command {
        Commands::Compile {
            input,
            output,
            target,
            debug_artifacts,
            format,
        } => {
            let target = target
                .or_else(|| {
                    let extension = Path::new(input).extension()?.to_str()?;
                    backends::find_by_extension(extension)
                })
                .ok_or(CompileError::MissingTarget)?;
            compile_to_file(input, output, target, *debug_artifacts, *format)?
        }
        Commands::Update => match UPDATE.install_blocking() {
            Ok(version) => println!("Updated smc-assembler to v{version}"),
            Err(error) => anyhow::bail!(
                "Cannot update: {error}. The newest release is at \
                 https://github.com/BjornTheProgrammer/smc-language/releases/latest"
            ),
        },
    }

    if let Some(Ok(Some(version))) = notice.map(thread::JoinHandle::join) {
        eprintln!(
            "smc-assembler v{version} is available. Run `smc-assembler update` to install it."
        );
    }

    Ok(())
}
