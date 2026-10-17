use arbitrary_int::u10;

use crate::{
    assembler::{
        AssemblerError,
        backends::{Lowered, Machine, Target, lower_each},
    },
    lexer::token::{Register, Span},
    parser::{
        ParsedItem,
        operations::{Address, Immediate, OperationWithArgs, SkipFlag},
    },
};

const REGISTER_COUNT: u8 = 4;

pub struct TauAnalyzersNone;

impl Target for TauAnalyzersNone {
    fn name(&self) -> &'static str {
        "tau-analyzers-none"
    }

    fn extensions(&self) -> &'static [&'static str] {
        &["tasm"]
    }

    fn lower(&self, items: &[ParsedItem]) -> (Vec<Lowered>, Vec<AssemblerError>) {
        lower_each(items, |op, span| {
            Ok(vec![lower_operation(op.clone(), span)?])
        })
    }
}

fn assemble_2reg(
    span: &Span,
    opcode: u8,
    register: Register,
    register1: Register,
) -> Result<Machine, AssemblerError> {
    // XXXX + DD + SS
    Ok(Machine::bytes(vec![
        opcode << 4
            | (register.check(REGISTER_COUNT, span)? & 0b11) << 2
            | (register1.check(REGISTER_COUNT, span)? & 0b11),
    ]))
}

fn assemble_1reg(
    span: &Span,
    operation: u8,
    register: Register,
) -> Result<Machine, AssemblerError> {
    // Format: 1100 + DD + OO
    Ok(Machine::bytes(vec![
        0b1100 << 4 | (register.check(REGISTER_COUNT, span)? & 0b11) << 2 | (operation & 0b11),
    ]))
}

fn assemble_immediate(
    span: &Span,
    operation: u8,
    register: Register,
    immediate: Immediate,
) -> Result<Machine, AssemblerError> {
    // Format: 1101 + DD + OO + IIIIIIII
    let first_byte =
        0b1101 << 4 | (register.check(REGISTER_COUNT, span)? & 0b11) << 2 | (operation & 0b11);

    Ok(Machine::new(2, move |resolver| {
        let immediate = resolver.immediate(immediate)?;
        if immediate < -128 || immediate > 255 {
            return Err(AssemblerError::ImmediateOutOfRange(
                resolver.span.clone(),
                immediate,
            ));
        }

        Ok(vec![first_byte, immediate as u8])
    }))
}

fn check_address_value(address: i128, span: &Span) -> Result<u16, AssemblerError> {
    let val: u16 = address
        .try_into()
        .map_err(|_| AssemblerError::AddressOutOfRange(span.clone(), address))?;

    let val = u10::try_new(val)
        .map_err(|_| AssemblerError::AddressOutOfRange(span.clone(), address))?
        .value();

    Ok((val as u16) & 0b0011_1111_1111)
}

fn assemble_branch(operation: u8, address: Address) -> Machine {
    // Format: 1110 + II + OO + IIIIIIII
    // II: upper 2 bits of 10-bit immediate
    // OO: operation code
    // IIIIIIII: lower 8 bits of 10-bit immediate
    Machine::new(2, move |resolver| {
        let address_original = resolver.address(address)?;
        let address_checked = check_address_value(address_original, resolver.span)?;

        let imm_upper = ((address_checked >> 8) & 0b11) as u8; // Upper 2 bits
        let imm_lower = (address_checked & 0xFF) as u8; // Lower 8 bits

        let first_byte = 0b1110 << 4 | imm_upper << 2 | (operation & 0b11);

        Ok(vec![first_byte, imm_lower])
    })
}

fn assemble_implicit(operation: u8) -> Machine {
    // Format: 1111 + OOOO
    Machine::bytes(vec![0b1111 << 4 | (operation & 0b1111)])
}

fn assemble_skip(flag: SkipFlag) -> Machine {
    // Format: 1111 + 1FFF
    let flag_bits = match flag {
        SkipFlag::Never => 0b000,
        SkipFlag::IfZero => 0b001,
        SkipFlag::IfNotZero => 0b010,
        SkipFlag::IfNegative => 0b011,
        SkipFlag::IfNotNegative => 0b100,
        SkipFlag::Always => 0b101,
    };
    Machine::bytes(vec![0b1111_1 << 3 | (flag_bits & 0b111)])
}

fn lower_operation(operation: OperationWithArgs, span: &Span) -> Result<Machine, AssemblerError> {
    let operation = match operation {
        OperationWithArgs::Clr1(r1) => OperationWithArgs::Ani2(r1, Immediate::Value(0)),
        _ => operation,
    };

    Ok(match operation {
        // 2 operand instructions
        OperationWithArgs::Add2(r1, r2) => assemble_2reg(span, 0b0000, r1, r2)?,
        OperationWithArgs::Sub2(r1, r2) => assemble_2reg(span, 0b0001, r1, r2)?,
        OperationWithArgs::Xor2(r1, r2) => assemble_2reg(span, 0b0010, r1, r2)?,
        OperationWithArgs::And2(r1, r2) => assemble_2reg(span, 0b0011, r1, r2)?,
        OperationWithArgs::Or2(r1, r2) => assemble_2reg(span, 0b0100, r1, r2)?,
        OperationWithArgs::Cmp2(r1, r2) => assemble_2reg(span, 0b0101, r1, r2)?,
        OperationWithArgs::Cpy2(r1, r2) => assemble_2reg(span, 0b0110, r1, r2)?,
        OperationWithArgs::Adc2(r1, r2) => assemble_2reg(span, 0b0111, r1, r2)?,
        OperationWithArgs::Mld2(r1, r2) => assemble_2reg(span, 0b1000, r1, r2)?,
        OperationWithArgs::Mst2(r1, r2) => assemble_2reg(span, 0b1001, r1, r2)?,
        OperationWithArgs::Pld2(r1, r2) => assemble_2reg(span, 0b1010, r1, r2)?,
        OperationWithArgs::Pst2(r1, r2) => assemble_2reg(span, 0b1011, r1, r2)?,

        // 1 operand instructions
        OperationWithArgs::Rsh1(r1) => assemble_1reg(span, 0b00, r1)?,
        OperationWithArgs::Inv1(r1) => assemble_1reg(span, 0b01, r1)?,
        OperationWithArgs::Inc1(r1) => assemble_1reg(span, 0b10, r1)?,
        OperationWithArgs::Dec1(r1) => assemble_1reg(span, 0b11, r1)?,

        // Immediate instructions
        OperationWithArgs::Ldi2(r1, imm) => assemble_immediate(span, 0b00, r1, imm)?,
        OperationWithArgs::Adi2(r1, imm) => assemble_immediate(span, 0b01, r1, imm)?,
        OperationWithArgs::Cpi2(r1, imm) => assemble_immediate(span, 0b10, r1, imm)?,
        OperationWithArgs::Ani2(r1, imm) => assemble_immediate(span, 0b11, r1, imm)?,

        OperationWithArgs::Jmp(addr) => assemble_branch(0b00, addr),
        OperationWithArgs::Cal(addr) => assemble_branch(0b01, addr),

        // Implicit instructions (except skip)
        OperationWithArgs::Bkl => assemble_implicit(0b0000),
        OperationWithArgs::Bkr => assemble_implicit(0b0001),
        OperationWithArgs::Hlt => assemble_implicit(0b0010),
        OperationWithArgs::Ret => assemble_implicit(0b0011),

        // Skip instructions
        OperationWithArgs::Skp(flag) => assemble_skip(flag),

        _ => {
            return Err(AssemblerError::UnsupportedOperation(
                span.clone(),
                operation,
            ));
        }
    })
}
