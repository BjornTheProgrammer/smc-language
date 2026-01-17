use clap::ValueEnum;
use mc_schem::{Block, Schematic};

use crate::CompileError;
pub mod arc_memory_hexserial;
pub mod batpu2_instruction_memory;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
pub enum Format {
    ArcMemoryHexSerial,
    Batpu2InstructionMemory,
}

impl Format {
    pub fn make_schematic(&self, data: Vec<u8>) -> Result<Schematic, CompileError> {
        match self {
            Format::ArcMemoryHexSerial => todo!("Still need to implement"),
            Format::Batpu2InstructionMemory => batpu2_instruction_memory::make_schematic(data),
        }
    }
}

pub fn make_block(id: &str) -> Block {
    Block::from_id(id).unwrap_or_else(|_| Block::air())
}
