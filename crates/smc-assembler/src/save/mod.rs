use std::{fs, path::Path};

use mc_schem::{Schematic, schem::WorldEdit13SaveOption};

use crate::{
    CompileError,
    save::{
        convert::{convert_to_mc, convert_to_tau},
        memory::Format,
    },
};

pub mod convert;
pub mod memory;

pub fn save_file<P: AsRef<Path>>(
    output: P,
    data: Vec<u8>,
    format: Option<Format>,
) -> Result<(), CompileError> {
    let output = output.as_ref();
    let extension = output
        .extension()
        .unwrap_or_default()
        .to_str()
        .unwrap_or("");

    // Check if it's a schematic format
    if extension == "litematic" || extension == "nbt" || extension == "schem" {
        let format = match format {
            Some(format) => format,
            None => return Err(CompileError::MissingFormat),
        };

        let schematic = format.make_schematic(data)?;

        if extension == "schem" {
            save_sponge_schematic(&schematic, output)?;
        } else {
            schematic
                .save_to_file(&output.display().to_string())
                .map_err(|e| CompileError::SchematicSaveFailed(e))?;
        }

        return Ok(());
    }

    if extension == "mc" {
        fs::write(
            output,
            convert_to_mc(data).map_err(CompileError::FormatError)?,
        )
        .map_err(CompileError::WriteFileError)?;

        return Ok(());
    } else if extension == "tau" || extension == "lym" {
        // Tau and Lyminal both have 8 bit words, one per line
        fs::write(
            output,
            convert_to_tau(data).map_err(CompileError::FormatError)?,
        )
        .map_err(CompileError::WriteFileError)?;

        return Ok(());
    }

    Err(CompileError::UnsupportedFileType)
}

/// Saves a WorldEdit `.schem`. WorldEdit 7.2 (and tools built on it, like ORE's
/// Schemati) only recognise one whose root tag is named `Schematic`. mc_schem's
/// `save_to_file` leaves it unnamed, but its writer version names it.
fn save_sponge_schematic(schematic: &Schematic, output: &Path) -> Result<(), CompileError> {
    let mut file = fs::File::create(output).map_err(CompileError::WriteFileError)?;
    schematic.save_world_edit_13_writer(&mut file, &WorldEdit13SaveOption::default())?;
    Ok(())
}
