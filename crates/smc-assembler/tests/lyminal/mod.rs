use smc_assembler::{
    CompileError, assembler::backends::LyminalUniNone, compile, save::convert::convert_to_tau,
};
use std::{fs, path::PathBuf};

pub fn test_compilation(program_name: &str) {
    use pretty_assertions::assert_eq;

    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push(format!("tests/lyminal/programs/{}.lasm", program_name));

    let mut expected_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    expected_path.push(format!("tests/lyminal/expected/{}.lym", program_name));

    let source = fs::read_to_string(path).expect("Should be able to read the source file");
    let result = compile(&source, &LyminalUniNone, false).expect("compilation should succeed");
    // Lyminal output is one byte per line, the same as Tau's
    let lym_output = convert_to_tau(result).expect("conversion to lym should succeed");

    assert_eq!(
        lym_output,
        fs::read_to_string(expected_path).expect("Should be able to read the expected output")
    );
}

#[test]
fn compiles_hello_world() {
    test_compilation("hello-world");
}

fn assemble(source: &str) -> Vec<u8> {
    compile(source, &LyminalUniNone, false).expect("compilation should succeed")
}

fn error_count(source: &str) -> usize {
    match compile(source, &LyminalUniNone, false) {
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
        assemble("PIC\nPNC\nPIZ\nPNZ"),
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

        SWP A [Z] 'H'
        SWP Z [A] 'e'
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

#[test]
fn syntax() {
    // Commas between operands are optional
    assert_eq!(assemble("ADD X, Z, Y"), assemble("ADD X Z Y"));
    // A label after an instruction on the same line is an operand
    assert_eq!(assemble(".top JMP .top"), [0b11_000000, 0b10_11_11_00]);
    // Statements end at line breaks, so a second mnemonic is an extra operand
    assert_eq!(error_count("PIC PNC"), 1);
    // Mnemonics from other targets are unknown
    assert_eq!(error_count("NOP"), 1);
}

#[test]
fn characters_are_ascii() {
    assert_eq!(assemble("SWP A [Z] 'H'"), assemble("SWP A [Z] 72"));
    assert_eq!(assemble("SWP Z [X] ','"), assemble("SWP Z [X] 44"));
    assert_eq!(assemble("MST [X] ':'"), assemble("MST [X] 58"));
}

#[test]
fn characters_can_be_escaped() {
    assert_eq!(assemble(r"MST [X] '\n'"), assemble("MST [X] 10"));
    assert_eq!(assemble(r"MST [X] '\t'"), assemble("MST [X] 9"));
    assert_eq!(assemble(r"MST [X] '\0'"), assemble("MST [X] 0"));
    assert_eq!(assemble(r"MST [X] '\''"), assemble("MST [X] 39"));
    assert_eq!(assemble(r"MST [X] '\\'"), assemble("MST [X] 92"));
    // A lone backslash is still a backslash
    assert_eq!(assemble(r"MST [X] '\'"), assemble("MST [X] 92"));
}

#[test]
fn saves_one_byte_per_line() {
    let mut path = std::path::PathBuf::from(env!("CARGO_TARGET_TMPDIR"));
    path.push("lyminal_lines.lym");

    smc_assembler::save::save_file(&path, assemble("SWP A [Z] 'H'"), None)
        .expect("saving should succeed");
    assert_eq!(
        std::fs::read_to_string(&path).expect("file should exist"),
        "11010010\n10111000\n"
    );
}

#[test]
fn reports_each_instruction() {
    use smc_assembler::{assembler::Assembler, lexer::Lexer, parser::Parser};

    let source = "LDI X 15\nMOV X X\nMLD Y [Y]\nMST [X] 'd'";
    let parsed = Parser::new(Lexer::new(source).collect()).parse();
    let result = Assembler::new(&LyminalUniNone, parsed).assemble();
    let instructions: Vec<_> = result
        .instructions
        .iter()
        .map(|i| {
            (
                &source[i.span.start()..i.span.end()],
                i.address,
                i.bytes.clone(),
            )
        })
        .collect();

    assert_eq!(
        instructions,
        [
            ("LDI X 15", 0, vec![0b11_000011, 0b00_00_11_11]),
            ("MOV X X", 2, vec![]),
            // Loads through A, then moves it out
            ("MLD Y [Y]", 2, vec![0b10_11_01_11, 0b01_01_01_01]),
            // The span includes the closing quote
            ("MST [X] 'd'", 4, vec![0b11_011001, 0b10_00_00_00]),
        ]
    );
}
