use smc_assembler::{compile, convert_to_mc};
use std::{fs, path::PathBuf};

pub fn test_compilation(program_name: &str) {
    use pretty_assertions::assert_eq;

    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push(format!("tests/programs/{}.smc", program_name));

    let mut expected_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    expected_path.push(format!("tests/expected/{}.mc", program_name));

    let result = compile(path, false).expect("compilation should succeed");
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
