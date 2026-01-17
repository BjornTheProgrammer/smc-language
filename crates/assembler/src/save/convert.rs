use std::fmt::Write as _;

pub fn convert_to_mc(input: Vec<u8>) -> Result<String, std::fmt::Error> {
    let bytes = input.chunks(2);

    let mut output = String::new();
    for byte in bytes {
        let byte1 = byte[0];
        let byte2 = byte.get(1).unwrap_or(&0u8);
        writeln!(output, "{:08b}{:08b}", byte1, byte2)?;
    }
    Ok(output)
}

pub fn convert_to_tau(input: Vec<u8>) -> Result<String, std::fmt::Error> {
    let mut output = String::new();
    for byte in input {
        writeln!(output, "{:08b}", byte)?;
    }
    Ok(output)
}
