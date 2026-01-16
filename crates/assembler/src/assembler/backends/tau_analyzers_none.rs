use arbitrary_int::u10;

use crate::{
    assembler::{
        AssemblerError, LabelMap, backends::Backend, get_address_value, get_immediate_value,
    },
    lexer::token::{Register, Span},
    parser::{
        DefineMap,
        operations::{Address, Immediate, OperationWithArgs, SkipFlag},
    },
};

pub fn instruction_byte_size(op: &OperationWithArgs) -> usize {
    use OperationWithArgs::*;
    match op {
        Ldi2(..) | Adi2(..) | Cpi2(..) | Ani2(..) | Jmp(..) | Cal(..) => 2,
        _ => 1,
    }
}

fn assemble_2reg(
    span: &Span,
    opcode: u8,
    register: Register,
    register1: Register,
) -> Result<u8, AssemblerError> {
    // XXXX + DD + SS
    let backend = Backend::TauAnalyzersNone;

    Ok(opcode << 4
        | (register.check(&backend, &span)? & 0b11) << 2
        | (register1.check(&backend, &span)? & 0b11))
}

fn assemble_1reg(span: &Span, operation: u8, register: Register) -> Result<u8, AssemblerError> {
    let backend = Backend::TauAnalyzersNone;
    // Format: 1100 + DD + OO
    Ok(0b1100 << 4 | (register.check(&backend, span)? & 0b11) << 2 | (operation & 0b11))
}

fn assemble_immediate(
    span: &Span,
    defines: &DefineMap,
    operation: u8,
    register: Register,
    immediate: Immediate,
) -> Result<[u8; 2], AssemblerError> {
    let backend = Backend::TauAnalyzersNone;
    // Format: 1101 + DD + OO + IIIIIIII
    let first_byte =
        0b1101 << 4 | (register.check(&backend, span)? & 0b11) << 2 | (operation & 0b11);

    let immediate = get_immediate_value(span, defines, immediate)?;
    if immediate < -128 || immediate > 255 {
        return Err(AssemblerError::ImmediateOutOfRange(span.clone(), immediate));
    }

    let immediate = immediate as u8;

    Ok([first_byte, immediate])
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

fn assemble_branch(
    span: &Span,
    defines: &DefineMap,
    labels: &LabelMap,
    operation: u8,
    address: Address,
) -> Result<[u8; 2], AssemblerError> {
    // Format: 1110 + II + OO + IIIIIIII
    // II: upper 2 bits of 10-bit immediate
    // OO: operation code
    // IIIIIIII: lower 8 bits of 10-bit immediate

    let address_original = get_address_value(span, defines, labels, address)?;
    let address_checked = check_address_value(address_original, span)?;

    let imm_upper = ((address_checked >> 8) & 0b11) as u8; // Upper 2 bits
    let imm_lower = (address_checked & 0xFF) as u8; // Lower 8 bits

    let first_byte = 0b1110 << 4 | imm_upper << 2 | (operation & 0b11);

    Ok([first_byte, imm_lower])
}

fn assemble_implicit(operation: u8) -> u8 {
    // Format: 1111 + OOOO
    0b1111 << 4 | (operation & 0b1111)
}

fn assemble_skip(flag: SkipFlag) -> u8 {
    // Format: 1111 + 1FFF
    let flag_bits = match flag {
        SkipFlag::Never => 0b000,
        SkipFlag::IfZero => 0b001,
        SkipFlag::IfNotZero => 0b010,
        SkipFlag::IfNegative => 0b011,
        SkipFlag::IfNotNegative => 0b100,
        SkipFlag::Always => 0b101,
    };
    0b1111_1 << 3 | (flag_bits & 0b111)
}

pub fn assemble_operation(
    defines: &DefineMap,
    labels: &LabelMap,
    operation: OperationWithArgs,
    span: Span,
) -> Result<Vec<u8>, AssemblerError> {
    let mut result = Vec::new();

    let operation = match operation {
        OperationWithArgs::Clr1(r1) => OperationWithArgs::Ani2(r1, Immediate::Value(0)),
        _ => operation,
    };

    match operation {
        // 2 operand instructions
        OperationWithArgs::Add2(r1, r2) => {
            result.push(assemble_2reg(&span, 0b0000, r1, r2)?);
        }
        OperationWithArgs::Sub2(r1, r2) => {
            result.push(assemble_2reg(&span, 0b0001, r1, r2)?);
        }
        OperationWithArgs::Xor2(r1, r2) => {
            result.push(assemble_2reg(&span, 0b0010, r1, r2)?);
        }
        OperationWithArgs::And2(r1, r2) => {
            result.push(assemble_2reg(&span, 0b0011, r1, r2)?);
        }
        OperationWithArgs::Or2(r1, r2) => {
            result.push(assemble_2reg(&span, 0b0100, r1, r2)?);
        }
        OperationWithArgs::Cmp2(r1, r2) => {
            result.push(assemble_2reg(&span, 0b0101, r1, r2)?);
        }
        OperationWithArgs::Cpy2(r1, r2) => {
            result.push(assemble_2reg(&span, 0b0110, r1, r2)?);
        }
        OperationWithArgs::Adc2(r1, r2) => {
            result.push(assemble_2reg(&span, 0b0111, r1, r2)?);
        }
        OperationWithArgs::Mld2(r1, r2) => {
            result.push(assemble_2reg(&span, 0b1000, r1, r2)?);
        }
        OperationWithArgs::Mst2(r1, r2) => {
            result.push(assemble_2reg(&span, 0b1001, r1, r2)?);
        }
        OperationWithArgs::Pld2(r1, r2) => {
            result.push(assemble_2reg(&span, 0b1010, r1, r2)?);
        }
        OperationWithArgs::Pst2(r1, r2) => {
            result.push(assemble_2reg(&span, 0b1011, r1, r2)?);
        }

        // 1 operand instructions
        OperationWithArgs::Rsh1(r1) => {
            result.push(assemble_1reg(&span, 0b00, r1)?);
        }
        OperationWithArgs::Inv1(r1) => {
            result.push(assemble_1reg(&span, 0b01, r1)?);
        }
        OperationWithArgs::Inc1(r1) => {
            result.push(assemble_1reg(&span, 0b10, r1)?);
        }
        OperationWithArgs::Dec1(r1) => {
            result.push(assemble_1reg(&span, 0b11, r1)?);
        }

        // Immediate instructions
        OperationWithArgs::Ldi2(r1, imm) => {
            let bytes = assemble_immediate(&span, defines, 0b00, r1, imm)?;
            result.extend_from_slice(&bytes);
        }
        OperationWithArgs::Adi2(r1, imm) => {
            let bytes = assemble_immediate(&span, defines, 0b01, r1, imm)?;
            result.extend_from_slice(&bytes);
        }
        OperationWithArgs::Cpi2(r1, imm) => {
            let bytes = assemble_immediate(&span, defines, 0b10, r1, imm)?;
            result.extend_from_slice(&bytes);
        }
        OperationWithArgs::Ani2(r1, imm) => {
            let bytes = assemble_immediate(&span, defines, 0b11, r1, imm)?;
            result.extend_from_slice(&bytes);
        }

        OperationWithArgs::Jmp(addr) => {
            let bytes = assemble_branch(&span, defines, labels, 0b00, addr)?;
            result.extend_from_slice(&bytes);
        }
        OperationWithArgs::Cal(addr) => {
            let bytes = assemble_branch(&span, defines, labels, 0b01, addr)?;
            result.extend_from_slice(&bytes);
        }

        // Implicit instructions (except skip)
        OperationWithArgs::Bkl => {
            result.push(assemble_implicit(0b0000));
        }
        OperationWithArgs::Bkr => {
            result.push(assemble_implicit(0b0001));
        }
        OperationWithArgs::Hlt => {
            result.push(assemble_implicit(0b0010));
        }
        OperationWithArgs::Ret => {
            result.push(assemble_implicit(0b0011));
        }

        // Skip instructions
        OperationWithArgs::Skp(flag) => {
            result.push(assemble_skip(flag));
        }

        _ => return Err(AssemblerError::UnsupportedOperation(span, operation)),
    }

    Ok(result)
}
