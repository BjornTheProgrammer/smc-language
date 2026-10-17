use smc_assembler::{CompileError, assembler::backends::Backend, compile};

fn assemble(source: &str) -> Vec<u8> {
    compile(source, Backend::LyminalUniNone, false).expect("compilation should succeed")
}

fn error_count(source: &str) -> usize {
    match compile(source, Backend::LyminalUniNone, false) {
        Err(CompileError::AssembleError(errors)) => errors.len(),
        other => panic!("expected assembler errors, got {:?}", other),
    }
}

#[test]
fn arithmetic() {
    assert_eq!(assemble("ADD X Z Y"), [0b00_00_01_10]);
    assert_eq!(assemble("AND X Y Z"), [0b00_00_10_01]);
    assert_eq!(assemble("SUB X Y Z"), [0b01_00_01_10]);
    assert_eq!(assemble("SUB X Y Y"), [0b01_00_01_01]);
    assert_eq!(assemble("ADD X Y 5"), [0b11_111110, 0b01_00_01_11]);
    assert_eq!(assemble("SUB X Y 5"), [0b11_000001, 0b01_00_01_01]);
    assert_eq!(assemble("AND X Y 0x0F"), [0b11_000011, 0b00_00_01_11]);
}

#[test]
fn pseudo_instructions() {
    assert_eq!(assemble("MOV X Y"), [0b00_00_11_01]);
    assert_eq!(assemble("MOV X X"), Vec::<u8>::new());
    assert_eq!(assemble("MOV Y 100"), [0b11_011001, 0b00_01_11_00]);
    assert_eq!(assemble("LDI Y 100"), [0b11_011001, 0b00_01_11_00]);
    assert_eq!(assemble("LSI X -2"), [0b00_00_11_11]);
    assert_eq!(assemble("LSI X -1"), [0b01_00_00_00]);
    assert_eq!(assemble("LSI Y 0"), [0b01_01_01_01]);
    assert_eq!(assemble("LSI Z 2"), [0b01_10_11_11]);
    assert_eq!(assemble("MOV Z 1"), [0b01_10_10_10]);
    assert_eq!(assemble("NOT Z X"), [0b01_10_11_00]);
    assert_eq!(assemble("INC X Y"), [0b01_00_01_11]);
    assert_eq!(assemble("INC X"), [0b01_00_00_11]);
    assert_eq!(assemble("DEC X Y"), [0b00_00_01_11]);
}

#[test]
fn memory() {
    assert_eq!(assemble("MLD X [Y]"), [0b10_00_01_00]);
    assert_eq!(assemble("MLD X [X]"), [0b10_11_00_11, 0b01_00_01_01]);
    assert_eq!(assemble("MST [Z] X"), [0b10_10_10_00]);
    assert_eq!(assemble("MST [X] -1"), [0b11_111111, 0b10_00_00_11]);
    assert_eq!(assemble("SWP X [Y] Z"), [0b10_00_01_10]);
    assert_eq!(assemble("SWP X [Y] 72"), [0b11_010010, 0b10_00_01_00]);
}

#[test]
fn control_flow() {
    assert_eq!(
        assemble("PIC PNC PIZ PNZ"),
        [0b10_00_00_00, 0b10_01_01_01, 0b10_10_10_10, 0b10_11_11_11]
    );
    assert_eq!(assemble("JMP X"), [0b10_11_11_00]);
    assert_eq!(assemble("JMP 10"), [0b11_000010, 0b10_11_11_10]);
    assert_eq!(assemble("JAL Y Z"), [0b10_01_11_10]);
    assert_eq!(assemble("JAL Y 10"), [0b11_000010, 0b10_01_11_10]);
    assert_eq!(assemble("PC Z"), [0b10_10_11_10]);
}

#[test]
fn a_mode() {
    // MOV A X, then the A mode patch A + 2 - 1
    assert_eq!(assemble("MOV A X\nINC Y A"), [0b00_11_11_00, 0b01_01_10_10]);
    assert_eq!(assemble("MOV A X\nDEC Y A"), [0b00_11_11_00, 0b01_01_00_00]);
    assert_eq!(assemble("MOV A X\nMOV Y A"), [0b00_11_11_00, 0b01_01_01_01]);
    assert_eq!(assemble("MOV A X\nNOT Y A"), [0b00_11_11_00, 0b01_01_11_11]);
    assert_eq!(
        assemble("MOV A X\nADD Y A 1"),
        [0b00_11_11_00, 0b01_01_10_10]
    );
    assert_eq!(
        assemble("MOV A X\nSUB Y A 1"),
        [0b00_11_11_00, 0b01_01_00_00]
    );
    // SUB A A would be !A in A mode, so it lowers to AND X A 0
    assert_eq!(
        assemble("MOV A Y\nSUB X A A"),
        [0b00_11_11_01, 0b11_000000, 0b00_00_11_00]
    );
    // Writing A as a discard is fine when the next line doesn't rely on A reading -1
    assert_eq!(assemble("SWP A [X] Y\nPIZ"), [0b10_11_00_01, 0b10_10_10_10]);
}

#[test]
fn lut_example() {
    let source = "
        LDI X 15
        MST [X] -1
        .init
        MOV X Y
        INC Y X
        SWP A [X] Y
        PIZ
        JMP .init

        SWP A [Z] 72
        SWP Z [A] 101
    ";

    assert_eq!(
        assemble(source),
        [
            0b11_000011,
            0b00_00_11_11, // LDI X 15
            0b11_111111,
            0b10_00_00_11, // MST [X] -1
            0b00_00_11_01, // MOV X Y
            0b01_01_00_11, // INC Y X
            0b10_11_00_01, // SWP A [X] Y
            0b10_10_10_10, // PIZ
            0b11_000001,
            0b10_11_11_00, // JMP .init (4)
            0b11_010010,
            0b10_11_10_00, // SWP A [Z] 72
            0b11_011001,
            0b10_10_11_01, // SWP Z [A] 101
        ]
    );
}

#[test]
fn a_register_rules() {
    assert_eq!(error_count("ADD X A Y"), 1);
    assert_eq!(error_count("JMP A"), 1);
    assert_eq!(error_count("JAL A X"), 1);
    // MOV Y X uses A as -1, which the previous line changed
    assert_eq!(error_count("MOV A X\nMOV Y X"), 1);
    // A label means the next line might be reached without A written
    assert_eq!(error_count("MOV A X\n.label\nADD X A Y"), 2);
}

#[test]
fn invalid_operands() {
    assert_eq!(error_count("SWP X [X] Y"), 1);
    assert_eq!(error_count("SWP X [Y] X"), 1);
    assert_eq!(error_count("MST [X] X"), 1);
    assert_eq!(error_count("JAL X X"), 1);
    assert_eq!(error_count("LSI X 3"), 1);
    assert_eq!(error_count("ADD X Y 300"), 1);
    // A predicate only covers a single instruction
    assert_eq!(error_count("PIZ\nMLD X [X]"), 1);
}
