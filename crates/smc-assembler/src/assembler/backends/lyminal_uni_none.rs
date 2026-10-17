use crate::{
    assembler::{AssemblerError, LabelMap, backends::Backend},
    lexer::token::Span,
    parser::{DefineMap, operations::OperationWithArgs},
};

pub fn assemble_operation(
    defines: &DefineMap,
    labels: &LabelMap,
    operation: OperationWithArgs,
    span: Span,
) -> Result<Vec<u8>, AssemblerError> {
    let backend = Backend::LyminalUniNone;

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
        _ => Err(AssemblerError::UnsupportedOperation(span, operation)),
    }

    Ok(Vec::new())
}

pub fn instruction_byte_size(op: &OperationWithArgs) -> usize {
    use OperationWithArgs::*;
    match op {
        _ => 1,
    }
}
