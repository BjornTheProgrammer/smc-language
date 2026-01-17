use std::{fs, path::Path};

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

        schematic
            .save_to_file(&output.display().to_string())
            .map_err(|e| CompileError::SchematicSaveFailed(e))?;

        return Ok(());
    }

    if extension == "mc" {
        fs::write(
            output,
            convert_to_mc(data).map_err(CompileError::FormatError)?,
        )
        .map_err(CompileError::WriteFileError)?;

        return Ok(());
    } else if extension == "tau" {
        fs::write(
            output,
            convert_to_tau(data).map_err(CompileError::FormatError)?,
        )
        .map_err(CompileError::WriteFileError)?;

        return Ok(());
    }

    Err(CompileError::UnsupportedFileType)
}
