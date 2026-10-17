use smc_assembler::{assembler::backends::BatPu2, compile, save::convert::convert_to_mc};
use std::{fs, path::PathBuf};

pub fn test_compilation(program_name: &str) {
    use pretty_assertions::assert_eq;

    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push(format!("tests/batpu2/programs/{}.smc", program_name));

    let mut expected_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    expected_path.push(format!("tests/batpu2/expected/{}.mc", program_name));

    let source = fs::read_to_string(path).expect("Should be able to read the source file");
    let result = compile(&source, &BatPu2, false).expect("compilation should succeed");
    let mc_output = convert_to_mc(result).expect("conversion to mc should succeed");

    assert_eq!(
        mc_output,
        fs::read_to_string(expected_path).expect("Should be able to read the expected output")
    );
}

#[test]
fn compiles_connect4() {
    test_compilation("connect4");
}

#[test]
fn compiles_calculator() {
    test_compilation("calculator");
}

#[test]
fn compiles_2048() {
    test_compilation("2048");
}

#[test]
fn compiles_dvd() {
    test_compilation("dvd");
}

#[test]
fn compiles_gol() {
    test_compilation("gol");
}

#[test]
fn compiles_helloworld() {
    test_compilation("helloworld");
}

#[test]
fn compiles_maze() {
    test_compilation("maze");
}

#[test]
fn compiles_minesweeper() {
    test_compilation("minesweeper");
}

#[test]
fn compiles_tetris() {
    test_compilation("tetris");
}

#[test]
fn characters_use_the_display_encoding() {
    use smc_assembler::CompileError;

    // 'A' is 1 on the BatPU-2 character display, not 65
    assert_eq!(
        compile("ldi r1 'A'", &BatPu2, false).expect("compilation should succeed"),
        compile("ldi r1 1", &BatPu2, false).expect("compilation should succeed")
    );
    // ',' has no encoding on the display
    assert!(matches!(
        compile("ldi r1 ','", &BatPu2, false),
        Err(CompileError::AssembleError(_))
    ));
}

#[test]
fn condition_words_are_not_reserved() {
    // `z` is a condition only where BRH expects one
    let result = compile("define z 5\nldi r1 z\nbrh z 0", &BatPu2, false)
        .expect("compilation should succeed");
    assert_eq!(result, [0x81, 0x05, 0xB0, 0x00]);
}

#[test]
fn field_ranges() {
    let assembles = |source: &str| compile(source, &BatPu2, false).is_ok();

    // 8 bit immediates, signed or unsigned
    assert!(assembles("ldi r1 -128") && assembles("ldi r1 255"));
    assert!(!assembles("ldi r1 -129") && !assembles("ldi r1 256"));
    // 10 bit addresses
    assert!(assembles("jmp 1023"));
    assert!(!assembles("jmp 1024") && !assembles("jmp -1"));
    // Signed 4 bit offsets
    assert!(assembles("lod r1 r2 -8") && assembles("lod r1 r2 7"));
    assert!(!assembles("lod r1 r2 -9") && !assembles("lod r1 r2 8"));
}
