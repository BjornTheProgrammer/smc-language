use mc_schem::{Block, Region, Schematic, schem::DataVersion};

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

/// Every block of the program, at its position relative to where the original BatPU-2
/// tooling has you stand to paste
fn instruction_blocks(pos_list: &[[i32; 3]], lines: &[&str]) -> Vec<([i32; 3], Block)> {
    let mut blocks = Vec::with_capacity(lines.len() * 16);
    for (address, line) in lines.iter().enumerate() {
        let face = if address < 512 { "east" } else { "west" };
        let mut new_pos = pos_list[address];

        let byte1 = &line[8..]; // Lower byte
        let byte2 = &line[..8]; // Upper byte

        for (i, char) in byte1.chars().chain(byte2.chars()).enumerate() {
            // A gap between the lower and upper byte
            if i == 8 {
                new_pos[1] -= 2;
            }
            let block = if char == '1' {
                make_block(&format!("minecraft:repeater[facing={}]", face))
            } else {
                make_block("minecraft:purple_wool")
            };
            blocks.push((new_pos, block));
            new_pos[1] -= 2;
        }
    }
    blocks
}

pub fn make_schematic(data: Vec<u8>) -> Result<Schematic, CompileError> {
    let binding = convert_to_mc(data).map_err(CompileError::FormatError)?;
    let mut lines = binding.lines().collect::<Vec<&str>>();

    // Pad to 1024 lines
    while lines.len() < 1024 {
        lines.push("0000000000000000");
    }

    let pos_list = generate_instruction_positions();
    let blocks = instruction_blocks(&pos_list, &lines);

    // The region is exactly the ROM, so pasting without -a only clears air inside it
    let mut min = [i32::MAX; 3];
    let mut max = [i32::MIN; 3];
    for (pos, _) in &blocks {
        for axis in 0..3 {
            min[axis] = min[axis].min(pos[axis]);
            max[axis] = max[axis].max(pos[axis]);
        }
    }
    let shape = [0, 1, 2].map(|axis| max[axis] - min[axis] + 1);

    let mut region = Region::with_shape(shape);
    for (pos, block) in &blocks {
        let _ = region.set_block([0, 1, 2].map(|axis| pos[axis] - min[axis]), block);
    }

    // You stand on address 0's first bit, the north-east corner of the top layer
    let [x, y, z] = pos_list[0];
    let stand = [x, y + 1, z];

    let mut schematic = Schematic::new();
    // The BatPU-2 world is 1.18.2, which also saves `.schem` as Sponge v2 so every
    // WorldEdit 7 can load it
    schematic.metadata.mc_data_version = DataVersion::Java_1_18_2 as i32;
    // Where the region starts relative to you, which WorldEdit applies on paste
    schematic.metadata.schem_we_offset = Some([0, 1, 2].map(|axis| min[axis] - stand[axis]));

    schematic.regions.push(region);
    Ok(schematic)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pastes_standing_on_the_first_bit_in_1_18_2() {
        let path = std::env::temp_dir().join("smc-batpu2-memory.schem");
        let path = path.to_str().unwrap();
        make_schematic(vec![0xFF, 0xFF])
            .unwrap()
            .save_to_file(path)
            .unwrap();

        let (loaded, _) = Schematic::from_file(path).unwrap();
        assert_eq!(
            loaded.metadata.mc_data_version,
            DataVersion::Java_1_18_2 as i32
        );
        assert_eq!(loaded.metadata.schem_version, 2);
        assert_eq!(loaded.metadata.schem_we_offset, Some([-107, -33, 0]));
    }
}
