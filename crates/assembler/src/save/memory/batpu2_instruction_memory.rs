use mc_schem::{Region, Schematic};

use crate::{
    CompileError,
    save::{convert::convert_to_mc, memory::make_block},
};

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
    lines: &[&str],
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

pub fn make_schematic(data: Vec<u8>) -> Result<Schematic, CompileError> {
    let binding = convert_to_mc(data).map_err(CompileError::FormatError)?;
    let mut lines = binding.lines().collect::<Vec<&str>>();

    // Pad to 1024 lines
    while lines.len() < 1024 {
        lines.push("0000000000000000");
    }

    let offset = [-130, -50, -80];
    let shape = [150, 60, 160];

    let mut schematic = Schematic::new();
    let mut region = Region::with_shape(shape);

    let pos_list = generate_instruction_positions();
    write_instructions(&mut region, &pos_list, &lines, offset);

    schematic.regions.push(region);
    Ok(schematic)
}
