use arbitrary_int::{i4, u10};

use crate::{
    assembler::{
        AssemblerError, LabelMap, backends::Backend, get_address_value, get_immediate_value,
        get_offset_value,
    },
    lexer::token::{Condition, Register, Span},
    parser::{
        DefineMap,
        operations::{Immediate, OperationWithArgs},
    },
};

pub fn insert_before(defines: &mut DefineMap) -> Result<(), AssemblerError> {
    let ports = [
        "pixel_x",
        "pixel_y",
        "draw_pixel",
        "clear_pixel",
        "load_pixel",
        "buffer_screen",
        "clear_screen_buffer",
        "write_char",
        "buffer_chars",
        "clear_chars_buffer",
        "show_number",
        "clear_number",
        "signed_mode",
        "unsigned_mode",
        "rng",
        "controller_input",
    ];
    for (i, port) in ports.into_iter().enumerate() {
        defines.insert(port.to_string(), (i + 240) as f64);
    }

    Ok(())
}

pub fn instruction_byte_size(op: &OperationWithArgs) -> usize {
    match op {
        _ => 1,
    }
}

pub fn assemble_operation(
    defines: &DefineMap,
    labels: &LabelMap,
    operation: OperationWithArgs,
    span: Span,
) -> Result<u16, AssemblerError> {
    let backend = Backend::BatPU2;

    // Instruction lowering
    let operation = match operation {
        OperationWithArgs::Cmp2(r1, r2) => OperationWithArgs::Sub3(r1, r2, Register::R0),
        OperationWithArgs::Mov2(r1, r2) => OperationWithArgs::Add3(r1, Register::R0, r2),
        OperationWithArgs::Lsh2(r1, r2) => OperationWithArgs::Add3(r1, r1, r2),
        OperationWithArgs::Not2(r1, r2) => OperationWithArgs::Nor3(r1, Register::R0, r2),
        OperationWithArgs::Neg2(r1, r2) => OperationWithArgs::Sub3(Register::R0, r1, r2),
        OperationWithArgs::Inc1(r1) => OperationWithArgs::Adi2(r1, Immediate::Value(1)),
        OperationWithArgs::Dec1(r1) => OperationWithArgs::Adi2(r1, Immediate::Value(-1)),
        _ => operation,
    };

    match operation {
        OperationWithArgs::Nop => Ok(0b0000 << 12),
        OperationWithArgs::Hlt => Ok(0b0001 << 12),
        OperationWithArgs::Add3(r1, r2, r3) => Ok(0b0010 << 12
            | (((r1.check(&backend, &span)? & 0xF) as u16) << 8)
            | (((r2.check(&backend, &span)? & 0xF) as u16) << 4)
            | ((r3.check(&backend, &span)? & 0xF) as u16)),
        OperationWithArgs::Sub3(r1, r2, r3) => Ok(0b0011 << 12
            | (((r1.check(&backend, &span)? & 0xF) as u16) << 8)
            | (((r2.check(&backend, &span)? & 0xF) as u16) << 4)
            | ((r3.check(&backend, &span)? & 0xF) as u16)),
        OperationWithArgs::Nor3(r1, r2, r3) => Ok(0b0100 << 12
            | (((r1.check(&backend, &span)? & 0xF) as u16) << 8)
            | (((r2.check(&backend, &span)? & 0xF) as u16) << 4)
            | ((r3.check(&backend, &span)? & 0xF) as u16)),
        OperationWithArgs::And3(r1, r2, r3) => Ok(0b0101 << 12
            | (((r1.check(&backend, &span)? & 0xF) as u16) << 8)
            | (((r2.check(&backend, &span)? & 0xF) as u16) << 4)
            | ((r3.check(&backend, &span)? & 0xF) as u16)),
        OperationWithArgs::Xor3(r1, r2, r3) => Ok(0b0110 << 12
            | (((r1.check(&backend, &span)? & 0xF) as u16) << 8)
            | (((r2.check(&backend, &span)? & 0xF) as u16) << 4)
            | ((r3.check(&backend, &span)? & 0xF) as u16)),
        OperationWithArgs::Rsh2(r1, r2) => Ok(0b0111 << 12
            | (((r1.check(&backend, &span)? & 0xF) as u16) << 8)
            | ((r2.check(&backend, &span)? & 0xF) as u16)),
        OperationWithArgs::Ldi2(r1, immediate) => Ok(0b1000 << 12
            | (((r1.check(&backend, &span)? & 0xF) as u16) << 8)
            | (check_immediate_value(get_immediate_value(&span, defines, immediate)?, &span)?)),
        OperationWithArgs::Adi2(r1, immediate) => Ok(0b1001 << 12
            | (((r1.check(&backend, &span)? & 0xF) as u16) << 8)
            | (check_immediate_value(get_immediate_value(&span, defines, immediate)?, &span)?)),
        OperationWithArgs::Jmp(address) => Ok(0b1010 << 12
            | check_address_value(get_address_value(&span, defines, labels, address)?, &span)?),
        OperationWithArgs::Brh(condition, address) => {
            let condition = match condition {
                Condition::Equal => 0b00,
                Condition::NotEqual => 0b01,
                Condition::GreaterEqual => 0b10,
                Condition::Less => 0b11,
                _ => return Err(AssemblerError::InvalidCondition(span, condition)),
            };

            Ok(0b1011 << 12
                | (condition << 10)
                | check_address_value(get_address_value(&span, defines, labels, address)?, &span)?)
        }
        OperationWithArgs::Cal(address) => Ok(0b1100 << 12
            | (check_address_value(get_address_value(&span, defines, labels, address)?, &span)?
                & 0b0000_0011_1111_1111)),
        OperationWithArgs::Ret => Ok(0b1101_0000_0000_0000),
        OperationWithArgs::Lod(r1, r2, offset) => {
            let offset = match offset {
                Some(offset) => {
                    check_offset_value(get_offset_value(&span, defines, offset)?, &span)?
                }
                None => 0,
            };

            Ok(0b1110 << 12
                | (((r1.check(&backend, &span)? & 0xF) as u16) << 8)
                | (((r2.check(&backend, &span)? & 0xF) as u16) << 4)
                | offset)
        }
        OperationWithArgs::Str(r1, r2, offset) => {
            let offset = match offset {
                Some(offset) => {
                    check_offset_value(get_offset_value(&span, defines, offset)?, &span)?
                }
                None => 0,
            };

            Ok(0b1111 << 12
                | (((r1.check(&backend, &span)? & 0xF) as u16) << 8)
                | (((r2.check(&backend, &span)? & 0xF) as u16) << 4)
                | offset)
        }
        _ => Err(AssemblerError::UnsupportedOperation(span, operation)),
    }
}

fn check_immediate_value(immediate: i128, span: &Span) -> Result<u16, AssemblerError> {
    if immediate < -128 || immediate > 255 {
        return Err(AssemblerError::ImmediateOutOfRange(span.clone(), immediate));
    }

    Ok((immediate as u8) as u16)
}

fn check_offset_value(offset: i128, span: &Span) -> Result<u16, AssemblerError> {
    let val: i8 = offset
        .try_into()
        .map_err(|_| AssemblerError::OffsetOutOfRange(span.clone(), offset))?;

    let val = i4::try_new(val)
        .map_err(|_| AssemblerError::OffsetOutOfRange(span.clone(), offset))?
        .value();

    Ok((val as u16) & 0xF)
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
