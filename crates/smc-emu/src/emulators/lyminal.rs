//! Lyminal: a 1 byte, 3 operand CPU with four opcodes (A_D, SUB, MEM, BYT).
//!
//! Harvard architecture: program ROM, and a separate 256 address data space of 240 bytes of
//! RAM followed by 16 MMIO ports. Address 255 is the program counter.
//!
//! ROM holds whatever is loaded, but the 8 bit PC only reaches its first 256 bytes; the rest
//! is there for bank switching hardware to map in. The CPU halts when the PC ticks over 255.

use std::collections::VecDeque;

use smc_assembler::isa::lyminal::{A, Instruction, Opcode, join_immediate};

/// Bytes of ROM the 8 bit program counter can address
pub const ADDRESS_SPACE: usize = 256;
pub const RAM_SIZE: usize = 240;
pub const SCREEN_SIZE: usize = 32;

// MMIO ports. 240..=247 are left for user defined hardware.
pub const SCREEN_X: u8 = 248;
pub const SCREEN_Y: u8 = 249;
pub const SCREEN_PLOT: u8 = 250;
pub const SCREEN_CONTROL: u8 = 251;
pub const ASCII_IO: u8 = 252;
pub const GAMEPAD: u8 = 253;
pub const RESERVED: u8 = 254;
pub const PROGRAM_COUNTER: u8 = 255;

#[derive(Debug)]
pub struct Lyminal {
    /// At least `ADDRESS_SPACE` bytes, so every PC value reads something
    rom: Vec<u8>,
    ram: [u8; RAM_SIZE],
    /// X, Y and Z
    registers: [u8; 3],
    a: u8,
    /// Whether the previous instruction stored to A
    a_mode: bool,
    carry: bool,
    zero: bool,
    /// Set by a predicate whose condition was false; skips the next MEM
    predicate: bool,
    pc: u8,
    /// Set when fetching from 255 ticked the PC over, until the instruction ends
    pc_overflowed: bool,
    /// Stopped after the PC ticked over 255, until reset
    halted: bool,
    cycles: u64,
    pub io: Io,
}

impl Default for Lyminal {
    fn default() -> Self {
        Self::new()
    }
}

impl Lyminal {
    pub fn new() -> Self {
        Self {
            rom: vec![0; ADDRESS_SPACE],
            ram: [0; RAM_SIZE],
            registers: [0; 3],
            a: 0,
            a_mode: false,
            carry: false,
            zero: false,
            predicate: false,
            pc: 0,
            pc_overflowed: false,
            halted: false,
            cycles: 0,
            io: Io::default(),
        }
    }

    /// Loads a program into ROM, of any size, and resets the machine
    pub fn load_program(&mut self, program: &[u8]) {
        self.rom = program.to_vec();
        self.rom.resize(self.rom.len().max(ADDRESS_SPACE), 0);
        self.reset();
    }

    /// Zeroes memory and registers, keeping the program and gamepad state
    pub fn reset(&mut self) {
        let rom = std::mem::take(&mut self.rom);
        let gamepad = self.io.gamepad;
        *self = Self::new();
        self.rom = rom;
        self.io.gamepad = gamepad;
    }

    /// Overwrites a ROM byte, for editing programs in place
    pub fn set_rom(&mut self, index: usize, value: u8) {
        self.rom[index] = value;
    }

    pub fn set_ram(&mut self, index: usize, value: u8) {
        self.ram[index] = value;
    }

    /// Sets X, Y or Z (0 to 2), or A's stored value (3). A mode is unchanged.
    pub fn set_register(&mut self, index: usize, value: u8) {
        match index {
            3 => self.a = value,
            _ => self.registers[index] = value,
        }
    }

    pub fn rom(&self) -> &[u8] {
        &self.rom
    }

    pub fn ram(&self) -> &[u8; RAM_SIZE] {
        &self.ram
    }

    pub fn registers(&self) -> [u8; 3] {
        self.registers
    }

    /// A's stored value, which instructions only see in A mode
    pub fn a(&self) -> u8 {
        self.a
    }

    pub fn a_mode(&self) -> bool {
        self.a_mode
    }

    pub fn carry(&self) -> bool {
        self.carry
    }

    pub fn zero(&self) -> bool {
        self.zero
    }

    pub fn predicate(&self) -> bool {
        self.predicate
    }

    pub fn pc(&self) -> u8 {
        self.pc
    }

    pub fn cycles(&self) -> u64 {
        self.cycles
    }

    pub fn halted(&self) -> bool {
        self.halted
    }

    fn fetch(&mut self) -> u8 {
        let byte = self.rom[self.pc as usize];
        (self.pc, self.pc_overflowed) = self.pc.overflowing_add(1);
        byte
    }

    fn read_register(&self, register: u8) -> u8 {
        match register {
            A if self.a_mode => self.a,
            A => 0xFF,
            _ => self.registers[register as usize],
        }
    }

    /// Returns whether the write was to A
    fn write_register(&mut self, register: u8, value: u8) -> bool {
        match register {
            A => {
                self.a = value;
                true
            }
            _ => {
                self.registers[register as usize] = value;
                false
            }
        }
    }

    fn set_flags(&mut self, result: u8, carry: bool) {
        self.zero = result == 0;
        self.carry = carry;
    }

    /// Reads a data address. `consume` is false when the access only stores, so reading
    /// doesn't take input from devices.
    fn read_memory(&mut self, address: u8, consume: bool) -> u8 {
        match address {
            PROGRAM_COUNTER => self.pc,
            RESERVED => 0,
            port if port as usize >= RAM_SIZE => match consume {
                true => self.io.read(port),
                false => self.io.peek(port),
            },
            _ => self.ram[address as usize],
        }
    }

    fn write_memory(&mut self, address: u8, value: u8) {
        match address {
            // A jump sets the PC, so it doesn't tick over
            PROGRAM_COUNTER => {
                self.pc = value;
                self.pc_overflowed = false;
            }
            RESERVED => {}
            port if port as usize >= RAM_SIZE => self.io.write(port, value),
            _ => self.ram[address as usize] = value,
        }
    }

    /// Executes one instruction, unless halted. A BYT prefix and the instruction it modifies
    /// run together.
    pub fn step(&mut self) {
        if self.halted {
            return;
        }

        let mut byte = self.fetch();
        let mut immediate = None;
        while Opcode::decode(byte) == Opcode::Byt {
            // A prefix at 255 has no instruction left to modify
            if self.pc_overflowed {
                self.halted = true;
                return;
            }
            let prefix = byte;
            byte = self.fetch();
            immediate = Some(join_immediate(prefix, Instruction::decode(byte).z));
        }
        self.cycles += 1;

        let Instruction { opcode, x, y, z } = Instruction::decode(byte);
        let wrote_a = match opcode {
            Opcode::AddAnd => {
                let a = self.read_register(y);
                let (result, carry) = match immediate {
                    Some(imm) => (a & imm, false),
                    // A_D ands when the first source has the higher address
                    None if y > z => (a & self.read_register(z), false),
                    None => a.overflowing_add(self.read_register(z)),
                };
                self.set_flags(result, carry);
                self.write_register(x, result)
            }
            Opcode::Sub => {
                let (result, carry) = match immediate {
                    Some(imm) => subtract(self.read_register(y), imm),
                    None if y != z => subtract(self.read_register(y), self.read_register(z)),
                    // Orthogonality patches
                    None if !self.a_mode => (y.wrapping_sub(1), false),
                    None if y == A => (!self.a, false),
                    None => (self.a.wrapping_add(y).wrapping_sub(1), false),
                };
                self.set_flags(result, carry);
                self.write_register(x, result)
            }
            Opcode::Mem => self.memory(x, y, z, immediate),
            Opcode::Byt => unreachable!("BYT prefixes are consumed above"),
        };

        self.a_mode = wrote_a;
        self.halted = self.pc_overflowed;
    }

    /// MEM X [Y] Z. Returns whether it wrote to A.
    fn memory(&mut self, x: u8, y: u8, z: u8, immediate: Option<u8>) -> bool {
        if self.predicate {
            self.predicate = false;
            return false;
        }

        // Equal operands set the predicate when the condition is false
        if immediate.is_none() && x == y && y == z {
            let condition = match x {
                0b00 => self.carry,
                0b01 => !self.carry,
                0b10 => self.zero,
                _ => !self.zero,
            };
            self.predicate = !condition;
            return false;
        }

        let address = self.read_register(y);
        // BYT forms always store their immediate
        let store = match immediate {
            Some(imm) => Some(imm),
            None if x != z => Some(self.read_register(z)),
            None => None,
        };
        let load = x != y;

        self.carry = address as usize >= RAM_SIZE && self.io.status(address);
        let contents = self.read_memory(address, load);
        self.zero = contents == 0;

        let wrote_a = load && self.write_register(x, contents);
        if let Some(value) = store {
            self.write_memory(address, value);
        }
        wrote_a
    }
}

/// Subtracts, with carry meaning no borrow
fn subtract(a: u8, b: u8) -> (u8, bool) {
    (a.wrapping_sub(b), a >= b)
}

/// The devices on Lyminal's MMIO ports
#[derive(Debug)]
pub struct Io {
    /// NES style buttons, as one byte
    pub gamepad: u8,
    input: VecDeque<u8>,
    output: Vec<u8>,
    screen_x: u8,
    screen_y: u8,
    pixels: [bool; SCREEN_SIZE * SCREEN_SIZE],
    /// Bumped whenever the screen changes, so the UI only redraws then
    screen_version: u64,
}

impl Default for Io {
    fn default() -> Self {
        Self {
            gamepad: 0,
            input: VecDeque::new(),
            output: Vec::new(),
            screen_x: 0,
            screen_y: 0,
            pixels: [false; SCREEN_SIZE * SCREEN_SIZE],
            screen_version: 0,
        }
    }
}

impl Io {
    /// Queues bytes for programs to read from the ASCII port
    pub fn send_input(&mut self, bytes: &[u8]) {
        self.input.extend(bytes);
    }

    /// Everything programs have written to the ASCII port
    pub fn output(&self) -> &[u8] {
        &self.output
    }

    pub fn pixels(&self) -> &[bool; SCREEN_SIZE * SCREEN_SIZE] {
        &self.pixels
    }

    pub fn screen_version(&self) -> u64 {
        self.screen_version
    }

    fn pixel_index(&self) -> Option<usize> {
        let (x, y) = (self.screen_x as usize, self.screen_y as usize);
        (x < SCREEN_SIZE && y < SCREEN_SIZE).then_some(y * SCREEN_SIZE + x)
    }

    fn set_pixel(&mut self, lit: bool) {
        if let Some(index) = self.pixel_index() {
            if self.pixels[index] != lit {
                self.pixels[index] = lit;
                self.screen_version += 1;
            }
        }
    }

    /// The port's status bit, which accesses copy into the carry flag
    fn status(&self, port: u8) -> bool {
        match port {
            SCREEN_PLOT => self.pixel_index().is_some_and(|i| self.pixels[i]),
            ASCII_IO => !self.input.is_empty(),
            GAMEPAD => self.gamepad != 0,
            _ => false,
        }
    }

    /// Reads a port without side effects
    fn peek(&self, port: u8) -> u8 {
        match port {
            SCREEN_X => self.screen_x,
            SCREEN_Y => self.screen_y,
            SCREEN_PLOT => self.pixel_index().is_some_and(|i| self.pixels[i]) as u8,
            ASCII_IO => self.input.front().copied().unwrap_or(0),
            GAMEPAD => self.gamepad,
            _ => 0,
        }
    }

    fn read(&mut self, port: u8) -> u8 {
        match port {
            ASCII_IO => self.input.pop_front().unwrap_or(0),
            _ => self.peek(port),
        }
    }

    fn write(&mut self, port: u8, value: u8) {
        match port {
            SCREEN_X => self.screen_x = value,
            SCREEN_Y => self.screen_y = value,
            SCREEN_PLOT => self.set_pixel(value != 0),
            SCREEN_CONTROL if value == 0 => {
                self.pixels.fill(false);
                self.screen_version += 1;
            }
            ASCII_IO => self.output.push(value),
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use smc_assembler::{assembler::backends::LyminalUniNone, compile};

    use super::*;

    fn load(source: &str) -> Lyminal {
        let program = compile(source, &LyminalUniNone, false).expect("program should assemble");
        let mut cpu = Lyminal::new();
        cpu.load_program(&program);
        cpu
    }

    fn run(source: &str, steps: usize) -> Lyminal {
        let mut cpu = load(source);
        for _ in 0..steps {
            cpu.step();
        }
        cpu
    }

    #[test]
    fn writes_ram_then_halts() {
        // No loops, so it runs off the end and halts within 256 steps
        let cpu = run("LSI X 0\nMST [X] 'H'\nLSI X 1\nMST [X] 'i'", 256);
        assert!(cpu.halted());
        assert_eq!(&cpu.ram()[..2], b"Hi");
    }

    #[test]
    fn loads_programs_larger_than_the_address_space() {
        let mut cpu = Lyminal::new();
        let mut program = vec![0; 300];
        // LSI Y 1 at 255 and 299; only the first is reachable
        program[255] = 0b01_01_10_10;
        program[299] = 0b01_01_10_10;
        cpu.load_program(&program);
        assert_eq!(cpu.rom().len(), 300);

        for _ in 0..256 {
            cpu.step();
        }
        assert_eq!(cpu.registers()[1], 1);
        assert!(cpu.halted(), "the 8 bit PC can't reach 256");

        cpu.load_program(&[0b01_01_10_10]);
        assert_eq!(cpu.rom().len(), ADDRESS_SPACE, "small programs are padded");
    }

    #[test]
    fn halts_when_the_pc_passes_255() {
        let mut cpu = Lyminal::new();
        cpu.load_program(&[]);
        for _ in 0..255 {
            cpu.step();
        }
        assert!(!cpu.halted());

        // Runs the instruction at 255, then halts
        cpu.step();
        assert!(cpu.halted());
        assert_eq!(cpu.cycles(), 256);

        cpu.step();
        assert_eq!(cpu.cycles(), 256, "halted CPUs don't run");

        cpu.reset();
        assert!(!cpu.halted());
    }

    #[test]
    fn a_jump_at_255_keeps_running() {
        let mut program = vec![0; 256];
        // JMP X, with X = 0
        program[255] = 0b10_11_11_00;
        let mut cpu = Lyminal::new();
        cpu.load_program(&program);
        for _ in 0..256 {
            cpu.step();
        }
        assert!(!cpu.halted());
        assert_eq!(cpu.pc(), 0);
    }

    #[test]
    fn a_byt_at_255_halts_without_running() {
        let mut program = vec![0; 256];
        program[255] = 0b11_000001;
        // INC Y Y runs once at the start, and would run again if the PC wrapped
        program[0] = 0b01_01_01_11;
        let mut cpu = Lyminal::new();
        cpu.load_program(&program);
        for _ in 0..256 {
            cpu.step();
        }
        assert!(cpu.halted());
        assert_eq!(cpu.cycles(), 255);
        assert_eq!(cpu.registers()[1], 1);
    }

    #[test]
    fn arithmetic_and_flags() {
        let cpu = run("MOV X 200\nMOV Y 100\nADD Z X Y", 3);
        assert_eq!(cpu.registers()[2], 44);
        assert!(cpu.carry(), "the add overflowed");

        let cpu = run("LSI X 2\nSUB Y X 2", 2);
        assert_eq!(cpu.registers()[1], 0);
        assert!(cpu.zero() && cpu.carry(), "no borrow sets carry");

        let cpu = run("LSI X 1\nSUB Y X 2", 2);
        assert_eq!(cpu.registers()[1], 255);
        assert!(!cpu.carry(), "a borrow clears carry");
    }

    #[test]
    fn a_mode_lasts_one_instruction() {
        let cpu = run("MOV A X\nINC Y A", 2);
        assert_eq!(cpu.registers()[1], 1, "INC Y A reads A = 0 in A mode");
        assert!(!cpu.a_mode());

        // Out of A mode again, NOT Z Y is -1 - Y
        let cpu = run("MOV A X\nINC Y A\nNOT Z Y", 3);
        assert_eq!(cpu.registers()[2], !1);
    }

    #[test]
    fn byt_keeps_a_mode() {
        // ADD X A 5 is BYT + SUB X A -5, reading A's value
        let cpu = run("LSI A 2\nADD X A 5", 2);
        assert_eq!(cpu.registers()[0], 7);
    }

    #[test]
    fn jumps_and_links() {
        let cpu = run("JAL X .target\nLSI Y 1\n.target\nLSI Z 2", 2);
        assert_eq!(cpu.registers(), [2, 0, 2], "JAL links the address after it");
    }

    #[test]
    fn predicates_skip_the_next_memory_instruction() {
        // Z is clear, so PIZ skips the jump
        let cpu = run("LSI X 1\nPIZ\nJMP 0\nLSI Y 2", 4);
        assert_eq!(cpu.registers()[1], 2);
        assert_eq!(cpu.pc(), 5);

        // An A_D in between still runs, and the flag waits for the next MEM
        let cpu = run("LSI X 1\nPIZ\nINC Y X\nJMP 0\nLSI Z 2", 5);
        assert_eq!(cpu.registers(), [1, 2, 2]);
    }

    #[test]
    fn ascii_io() {
        let cpu = run("MOV Y 252\nMST [Y] 'H'\nMST [Y] 'i'", 3);
        assert_eq!(cpu.io.output(), b"Hi");

        let mut cpu = load("MOV Y 252\nMLD X [Y]\nMLD Z [Y]");
        cpu.io.send_input(b"k");
        cpu.step();
        cpu.step();
        assert_eq!(cpu.registers()[0], b'k');
        assert!(cpu.carry(), "status shows a byte was waiting");
        cpu.step();
        assert_eq!(cpu.registers()[2], 0);
        assert!(!cpu.carry() && cpu.zero());
    }

    #[test]
    fn gamepad_and_screen() {
        let program = compile(
            "MOV Y 253\nMLD X [Y]\nMOV Y 248\nMST [Y] 3\nMOV Y 249\nMST [Y] 4\nMOV Y 250\nMST [Y] 1",
            &LyminalUniNone,
            false,
        )
        .unwrap();
        let mut cpu = Lyminal::new();
        cpu.load_program(&program);
        cpu.io.gamepad = 0b1000_0001;
        for _ in 0..8 {
            cpu.step();
        }
        assert_eq!(cpu.registers()[0], 0b1000_0001);
        assert!(cpu.io.pixels()[4 * SCREEN_SIZE + 3]);
        assert_eq!(cpu.io.pixels().iter().filter(|&&lit| lit).count(), 1);
    }
}
