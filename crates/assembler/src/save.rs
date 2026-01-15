use mc_schem::{Block, Region, Schematic};
use std::{fs::OpenOptions, io::Write as _, path::Path};

use crate::CompileError;

fn make_block(id: &str) -> Block {
    Block::from_id(id).unwrap_or_else(|_| Block::air())
}

fn generate_instruction_positions() -> Vec<[i32; 3]> {
    let mem_start_pos = [-4, -1, 2];
    let mut pos_list = Vec::with_capacity(1024);

    for i in 0..2 {
        for j in 0..32 {
            let mut pos = mem_start_pos;
            if i == 1 {
                pos[0] -= 2;
            }

            pos[2] += 2 * j;
            if j >= 16 {
                pos[2] += 4;
            }

            for k in 0..16 {
                pos_list.push(pos);

                if k % 2 == 0 {
                    pos[0] -= 7;
                    pos[2] += if j < 16 { 1 } else { -1 };
                } else {
                    pos[0] -= 7;
                    pos[2] -= if j < 16 { 1 } else { -1 };
                }
            }
        }
    }

    pos_list
}

fn write_instructions(
    region: &mut Region,
    pos_list: &[[i32; 3]],
    lines: &[String],
    offset: [i32; 3],
) {
    for (address, line) in lines.iter().enumerate() {
        let face = if address < 512 { "east" } else { "west" };
        let mut new_pos = pos_list[address];

        let byte1 = &line[8..]; // Lower byte
        let byte2 = &line[..8]; // Upper byte

        // Write lower byte
        for char in byte1.chars() {
            let block = if char == '1' {
                make_block(&format!("minecraft:repeater[facing={}]", face))
            } else {
                make_block("minecraft:purple_wool")
            };
            let pos = [
                (new_pos[0] - offset[0]),
                (new_pos[1] - offset[1]),
                (new_pos[2] - offset[2]),
            ];
            let _ = region.set_block(pos, &block);
            new_pos[1] -= 2;
        }

        new_pos[1] -= 2;

        // Write upper byte
        for char in byte2.chars() {
            let block = if char == '1' {
                make_block(&format!("minecraft:repeater[facing={}]", face))
            } else {
                make_block("minecraft:purple_wool")
            };
            let pos = [
                (new_pos[0] - offset[0]),
                (new_pos[1] - offset[1]),
                (new_pos[2] - offset[2]),
            ];
            let _ = region.set_block(pos, &block);
            new_pos[1] -= 2;
        }
    }
}

fn reset_program_counter(region: &mut Region, offset: [i32; 3]) {
    let pc_start_pos = [-21, -1, -16];
    let mut pos = pc_start_pos;

    for _ in 0..10 {
        let block = make_block("minecraft:repeater[facing=north,locked=true,powered=false]");
        let p = [
            (pos[0] - offset[0]),
            (pos[1] - offset[1]),
            (pos[2] - offset[2]),
        ];
        let _ = region.set_block(p, &block);
        pos[1] -= 2;
    }
}

fn reset_call_stack(region: &mut Region, offset: [i32; 3]) {
    let push_start_pos = [-9, -1, -22];
    let pull_start_pos = [-8, -1, -21];

    // Push positions
    for i in 0..16 {
        let mut pos = push_start_pos;
        pos[2] -= i * 3;
        for _ in 0..10 {
            let block = make_block("minecraft:repeater[facing=south,locked=true,powered=false]");
            let p = [
                (pos[0] - offset[0]),
                (pos[1] - offset[1]),
                (pos[2] - offset[2]),
            ];
            let _ = region.set_block(p, &block);
            pos[1] -= 2;
        }
    }

    // Pull positions
    for i in 0..16 {
        let mut pos = pull_start_pos;
        pos[2] -= i * 3;
        for _ in 0..10 {
            let block = make_block("minecraft:repeater[facing=north,locked=true,powered=false]");
            let p = [
                (pos[0] - offset[0]),
                (pos[1] - offset[1]),
                (pos[2] - offset[2]),
            ];
            let _ = region.set_block(p, &block);
            pos[1] -= 2;
        }
    }
}

fn reset_flags(region: &mut Region, offset: [i32; 3]) {
    let flag_start_pos = [-26, -17, -60];
    let mut pos = flag_start_pos;

    let block = make_block("minecraft:repeater[facing=west,locked=true,powered=false]");
    let p = [
        (pos[0] - offset[0]),
        (pos[1] - offset[1]),
        (pos[2] - offset[2]),
    ];
    let _ = region.set_block(p, &block);

    pos[2] -= 4;
    let p = [
        (pos[0] - offset[0]),
        (pos[1] - offset[1]),
        (pos[2] - offset[2]),
    ];
    let _ = region.set_block(p, &block);
}

fn reset_data_memory(region: &mut Region, offset: [i32; 3]) {
    let data_start_pos = [-47, -3, -9];
    let mut pos_list_north = Vec::new();

    for i in 0..4 {
        let mut pos = data_start_pos;
        pos[2] -= 16 * i;
        for j in 0..16 {
            pos_list_north.push(pos);
            pos[0] -= 2;
            if j % 2 == 0 {
                pos[1] += 1;
            } else {
                pos[1] -= 1;
            }
        }

        let mut pos = data_start_pos;
        pos[2] -= 16 * i;
        pos[0] -= 36;
        pos[1] += 1;
        for j in 0..16 {
            pos_list_north.push(pos);
            pos[0] -= 2;
            if j % 2 == 0 {
                pos[1] -= 1;
            } else {
                pos[1] += 1;
            }
        }
    }

    // North-facing repeaters (excluding last 3)
    let block_north = make_block("minecraft:repeater[facing=north,locked=true,powered=false]");
    for pos in &pos_list_north[..pos_list_north.len().saturating_sub(3)] {
        let mut x = *pos;
        for _ in 0..8 {
            let p = [(x[0] - offset[0]), (x[1] - offset[1]), (x[2] - offset[2])];
            let _ = region.set_block(p, &block_north);
            x[1] -= 2;
        }
    }

    // South-facing repeaters
    let block_south = make_block("minecraft:repeater[facing=south,locked=true,powered=false]");
    for pos in &pos_list_north {
        let mut x = *pos;
        x[2] -= 2;
        for _ in 0..8 {
            let p = [(x[0] - offset[0]), (x[1] - offset[1]), (x[2] - offset[2])];
            let _ = region.set_block(p, &block_south);
            x[1] -= 2;
        }
    }
}

fn reset_registers(region: &mut Region, offset: [i32; 3]) {
    let reg_start_pos = [-35, -3, -12];
    let mut pos_list_east = Vec::new();

    let mut pos = reg_start_pos;
    for i in 0..15 {
        pos_list_east.push(pos);
        pos[2] -= 2;
        if i % 2 == 0 {
            pos[1] -= 1;
        } else {
            pos[1] += 1;
        }
    }

    let block_east = make_block("minecraft:repeater[facing=east,locked=true,powered=false]");
    let block_west = make_block("minecraft:repeater[facing=west,locked=true,powered=false]");

    for pos in &pos_list_east {
        let mut x = *pos;
        for _ in 0..8 {
            let p = [(x[0] - offset[0]), (x[1] - offset[1]), (x[2] - offset[2])];
            let _ = region.set_block(p, &block_east);
            x[1] -= 2;
        }

        let mut x = *pos;
        x[0] += 2;
        for _ in 0..8 {
            let p = [(x[0] - offset[0]), (x[1] - offset[1]), (x[2] - offset[2])];
            let _ = region.set_block(p, &block_west);
            x[1] -= 2;
        }
    }
}

pub fn make_schematic(data: &[u8]) -> Result<Schematic, CompileError> {
    let mut lines: Vec<String> = Vec::new();
    for word in data.chunks(2) {
        lines.push(format!("{:08b}{:08b}", word[0], word[1]));
    }

    // Pad to 1024 lines
    while lines.len() < 1024 {
        lines.push("0000000000000000".to_string());
    }

    // Calculate bounding box (we need to find min/max coordinates)
    // Based on the Python code, approximate bounds:
    // X: roughly -120 to 0
    // Y: roughly -40 to 0
    // Z: roughly -70 to 70
    let offset = [-130, -50, -80]; // Offset to make all coordinates positive
    let shape = [150, 60, 160]; // Size of the region

    let mut schematic = Schematic::new();
    let mut region = Region::with_shape(shape);

    let pos_list = generate_instruction_positions();
    write_instructions(&mut region, &pos_list, &lines, offset);
    reset_program_counter(&mut region, offset);
    reset_call_stack(&mut region, offset);
    reset_flags(&mut region, offset);
    reset_data_memory(&mut region, offset);
    reset_registers(&mut region, offset);

    schematic.regions.push(region);
    Ok(schematic)
}

pub fn save_file<P: AsRef<Path>>(output: P, data: Vec<u8>) -> Result<(), CompileError> {
    let output = output.as_ref();
    let extension = output
        .extension()
        .unwrap_or_default()
        .to_str()
        .unwrap_or("");

    // Check if it's a schematic format
    if extension == "litematic" || extension == "nbt" || extension == "schem" {
        let schematic = make_schematic(&data)?;

        schematic
            .save_to_file(&output.display().to_string())
            .map_err(|e| CompileError::SchematicSaveFailed(e))?;

        return Ok(());
    }

    // Default: write as text file
    let mut file = OpenOptions::new()
        .create(true)
        .truncate(true)
        .write(true)
        .open(output)
        .map_err(CompileError::WriteFileError)?;

    for word in data.chunks(2) {
        match word.get(1) {
            Some(second_byte) => writeln!(file, "{:08b}{:08b}", word[0], second_byte)
                .map_err(CompileError::WriteFileError)?,
            None => writeln!(file, "{:08b}", word[0]).map_err(CompileError::WriteFileError)?,
        }
    }

    Ok(())
}
