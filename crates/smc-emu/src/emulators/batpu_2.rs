use thiserror::Error;

#[derive(Error, Debug)]
pub enum Batpu2EmulationError {
    #[error("Invalid program size")]
    InvalidProgramSize,
}

#[derive(Debug)]
pub struct Batpu2Emulator {
    program: [u8; 1023],
    registers: [u8; 8],
    zero_flag: bool,
    carry_flag: bool,
}

impl Batpu2Emulator {
    pub fn new() -> Self {
        Self {
            program: [0u8; 1023],
            registers: [0u8; 8],
            zero_flag: false,
            carry_flag: false,
        }
    }

    pub fn load_program(&mut self, program: Vec<u8>) -> Result<(), Batpu2EmulationError> {
        if program.len() > 1023 {
            return Err(Batpu2EmulationError::InvalidProgramSize);
        }

        self.program.fill(0);
        self.program[..program.len()].copy_from_slice(&program);

        Ok(())
    }

    pub fn program(&self) -> &[u8] {
        &self.program
    }

    pub fn registers(&self) -> &[u8; 8] {
        &self.registers
    }

    pub fn zero_flag(&self) -> bool {
        self.zero_flag
    }

    pub fn carry_flag(&self) -> bool {
        self.carry_flag
    }
}
