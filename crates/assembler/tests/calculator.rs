use std::{fs, path::PathBuf};

use smc_assembler::{compile, convert_to_mc};

#[test]
fn compiles_calculator() {
    use pretty_assertions::assert_eq;

    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("tests/programs/calculator.smc");

    let mut expected_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    expected_path.push("tests/expected/calculator.mc");

    let result = compile(path, false).expect("compilation should succeed");

    let mc_output = convert_to_mc(result).expect("conversion to mc should succeed");

    assert_eq!(
        mc_output,
        fs::read_to_string(expected_path).expect("Should be able to read the expected output")
    );
}
