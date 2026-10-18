//! Hex editing of Lyminal memory and registers: select bytes, type hex over them, and copy
//! or paste them as hex.

use iced::keyboard::{Key, Modifiers, key::Named};

use crate::emulators::lyminal::{self, Lyminal};

/// Bytes per row in the memory view, which up and down move by
pub const ROW: usize = 8;

/// An editable area of the machine
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Region {
    Rom,
    Ram,
    /// X, Y, Z, then A
    Registers,
}

impl Region {
    pub fn len(self, cpu: &Lyminal) -> usize {
        match self {
            Region::Rom => cpu.rom().len(),
            Region::Ram => lyminal::RAM_SIZE,
            Region::Registers => 4,
        }
    }

    pub fn read(self, cpu: &Lyminal, index: usize) -> u8 {
        match self {
            Region::Rom => cpu.rom()[index],
            Region::Ram => cpu.ram()[index],
            Region::Registers => match index {
                3 => cpu.a(),
                _ => cpu.registers()[index],
            },
        }
    }

    fn write(self, cpu: &mut Lyminal, index: usize, value: u8) {
        match self {
            Region::Rom => cpu.set_rom(index, value),
            Region::Ram => cpu.set_ram(index, value),
            Region::Registers => cpu.set_register(index, value),
        }
    }

    /// Bytes per row, for moving up and down
    fn row(self) -> usize {
        match self {
            Region::Registers => 2,
            _ => ROW,
        }
    }
}

/// What the app should do after a key press
#[derive(Debug, PartialEq, Eq)]
pub enum KeyAction {
    /// The editor used the key
    Handled,
    /// The key isn't for the editor, so it can go to the gamepad
    Ignored,
    Copy(String),
    Paste,
}

#[derive(Debug, Default)]
pub struct Editor {
    region: Option<Region>,
    /// Where the selection started; shift extends from here
    anchor: usize,
    /// Where typing writes
    cursor: usize,
    /// The first hex digit of a byte being typed
    pending: Option<u8>,
}

impl Editor {
    /// Selects a byte, or extends the selection to it
    pub fn select(&mut self, region: Region, index: usize, extend: bool) {
        if !extend || self.region != Some(region) {
            self.anchor = index;
        }
        self.region = Some(region);
        self.cursor = index;
        self.pending = None;
    }

    /// Drops a half typed byte
    pub fn cancel_digit(&mut self) {
        self.pending = None;
    }

    pub fn clear(&mut self) {
        self.region = None;
        self.pending = None;
    }

    pub fn region(&self) -> Option<Region> {
        self.region
    }

    /// The selected indexes, first to last
    pub fn range(&self) -> std::ops::RangeInclusive<usize> {
        self.anchor.min(self.cursor)..=self.anchor.max(self.cursor)
    }

    pub fn is_selected(&self, region: Region, index: usize) -> bool {
        self.region == Some(region) && self.range().contains(&index)
    }

    pub fn is_cursor(&self, region: Region, index: usize) -> bool {
        self.region == Some(region) && self.cursor == index
    }

    /// The digit typed so far at the cursor
    pub fn pending(&self) -> Option<u8> {
        self.pending
    }

    pub fn key(&mut self, key: &Key, modifiers: Modifiers, cpu: &mut Lyminal) -> KeyAction {
        let Some(region) = self.region else {
            return KeyAction::Ignored;
        };

        if modifiers.command() {
            return match key.as_ref() {
                Key::Character("c") => KeyAction::Copy(self.copy_hex(cpu)),
                Key::Character("v") => KeyAction::Paste,
                _ => KeyAction::Ignored,
            };
        }

        let step = |delta: isize| {
            self.cursor
                .saturating_add_signed(delta)
                .min(region.len(cpu) - 1)
        };
        let moved = match key.as_ref() {
            Key::Named(Named::ArrowLeft) => Some(step(-1)),
            Key::Named(Named::ArrowRight) => Some(step(1)),
            Key::Named(Named::ArrowUp) => Some(step(-(region.row() as isize))),
            Key::Named(Named::ArrowDown) => Some(step(region.row() as isize)),
            _ => None,
        };
        if let Some(index) = moved {
            self.select(region, index, modifiers.shift());
            return KeyAction::Handled;
        }

        match key.as_ref() {
            Key::Named(Named::Escape) => {
                match self.pending {
                    Some(_) => self.pending = None,
                    None => self.clear(),
                }
                KeyAction::Handled
            }
            Key::Character(c) => match c.chars().next().and_then(|c| c.to_digit(16)) {
                Some(digit) => {
                    self.type_digit(digit as u8, region, cpu);
                    KeyAction::Handled
                }
                None => KeyAction::Ignored,
            },
            _ => KeyAction::Ignored,
        }
    }

    /// The first digit waits; the second writes the byte and moves to the next one
    fn type_digit(&mut self, digit: u8, region: Region, cpu: &mut Lyminal) {
        let Some(high) = self.pending.take() else {
            self.pending = Some(digit);
            return;
        };

        region.write(cpu, self.cursor, high << 4 | digit);
        let next = (self.cursor + 1).min(region.len(cpu) - 1);
        self.select(region, next, false);
    }

    /// The selection as hex bytes, `48 65 6C`
    pub fn copy_hex(&self, cpu: &Lyminal) -> String {
        let Some(region) = self.region else {
            return String::new();
        };
        self.range()
            .map(|i| format!("{:02X}", region.read(cpu, i)))
            .collect::<Vec<_>>()
            .join(" ")
    }

    /// The selection as text, with unprintable bytes as `.`
    pub fn copy_text(&self, cpu: &Lyminal) -> String {
        let Some(region) = self.region else {
            return String::new();
        };
        self.range()
            .map(|i| match region.read(cpu, i) {
                byte if byte.is_ascii_graphic() || byte == b' ' => byte as char,
                _ => '.',
            })
            .collect()
    }

    /// Writes hex bytes from the start of the selection and selects what was written.
    /// Bytes past the end of the region are dropped.
    pub fn paste(&mut self, text: &str, cpu: &mut Lyminal) -> Result<usize, String> {
        let Some(region) = self.region else {
            return Ok(0);
        };
        let bytes = parse_hex(text)?;
        if bytes.is_empty() {
            return Ok(0);
        }

        let start = *self.range().start();
        let written = bytes.len().min(region.len(cpu) - start);
        for (offset, &byte) in bytes[..written].iter().enumerate() {
            region.write(cpu, start + offset, byte);
        }

        self.select(region, start, false);
        self.select(region, start + written - 1, true);
        Ok(written)
    }
}

/// Parses hex bytes separated by spaces or commas, or written together like `48656C`
fn parse_hex(text: &str) -> Result<Vec<u8>, String> {
    let digits: String = text
        .split(|c: char| c.is_whitespace() || c == ',')
        .map(|token| token.trim_start_matches("0x").trim_start_matches("0X"))
        .flat_map(|token| {
            // A lone digit is a whole byte, so `F` pastes as 0F
            if token.len() == 1 {
                format!("0{token}").chars().collect::<Vec<_>>()
            } else {
                token.chars().collect()
            }
        })
        .collect();

    if let Some(bad) = digits.chars().find(|c| !c.is_ascii_hexdigit()) {
        return Err(format!("Cannot paste `{bad}`, expected hex bytes"));
    }
    if digits.len() % 2 != 0 {
        return Err("Cannot paste an odd number of hex digits".to_string());
    }

    Ok((0..digits.len())
        .step_by(2)
        .map(|i| u8::from_str_radix(&digits[i..i + 2], 16).expect("checked hex digits"))
        .collect())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn press(editor: &mut Editor, cpu: &mut Lyminal, key: &str) -> KeyAction {
        editor.key(&Key::Character(key.into()), Modifiers::empty(), cpu)
    }

    fn arrow(editor: &mut Editor, cpu: &mut Lyminal, named: Named, shift: bool) {
        let modifiers = if shift {
            Modifiers::SHIFT
        } else {
            Modifiers::empty()
        };
        editor.key(&Key::Named(named), modifiers, cpu);
    }

    #[test]
    fn typing_two_digits_writes_a_byte_and_moves_on() {
        let (mut editor, mut cpu) = (Editor::default(), Lyminal::new());
        editor.select(Region::Ram, 4, false);

        assert_eq!(press(&mut editor, &mut cpu, "4"), KeyAction::Handled);
        assert_eq!(editor.pending(), Some(4));
        assert_eq!(cpu.ram()[4], 0, "one digit doesn't write yet");

        press(&mut editor, &mut cpu, "A");
        assert_eq!(cpu.ram()[4], 0x4A);
        assert!(editor.is_cursor(Region::Ram, 5));
        assert_eq!(editor.pending(), None);
    }

    #[test]
    fn edits_rom_and_registers() {
        let (mut editor, mut cpu) = (Editor::default(), Lyminal::new());
        editor.select(Region::Rom, 0, false);
        press(&mut editor, &mut cpu, "c");
        press(&mut editor, &mut cpu, "3");
        assert_eq!(cpu.rom()[0], 0xC3);

        editor.select(Region::Registers, 3, false);
        press(&mut editor, &mut cpu, "7");
        press(&mut editor, &mut cpu, "f");
        assert_eq!(cpu.a(), 0x7F);
        assert!(
            editor.is_cursor(Region::Registers, 3),
            "stays on the last register"
        );
    }

    #[test]
    fn non_hex_keys_go_to_the_gamepad() {
        let (mut editor, mut cpu) = (Editor::default(), Lyminal::new());
        assert_eq!(
            press(&mut editor, &mut cpu, "a"),
            KeyAction::Ignored,
            "nothing selected"
        );

        editor.select(Region::Ram, 0, false);
        assert_eq!(press(&mut editor, &mut cpu, "w"), KeyAction::Ignored);
        assert_eq!(
            press(&mut editor, &mut cpu, "a"),
            KeyAction::Handled,
            "a is a hex digit"
        );
    }

    #[test]
    fn arrows_move_and_shift_extends() {
        let (mut editor, mut cpu) = (Editor::default(), Lyminal::new());
        editor.select(Region::Ram, 9, false);

        arrow(&mut editor, &mut cpu, Named::ArrowDown, false);
        assert!(editor.is_cursor(Region::Ram, 9 + ROW));
        arrow(&mut editor, &mut cpu, Named::ArrowRight, true);
        arrow(&mut editor, &mut cpu, Named::ArrowRight, true);
        assert_eq!(editor.range(), 17..=19);

        // Moving stops at the edges
        editor.select(Region::Ram, 0, false);
        arrow(&mut editor, &mut cpu, Named::ArrowUp, false);
        assert!(editor.is_cursor(Region::Ram, 0));
    }

    #[test]
    fn escape_cancels_a_digit_then_deselects() {
        let (mut editor, mut cpu) = (Editor::default(), Lyminal::new());
        editor.select(Region::Ram, 0, false);
        press(&mut editor, &mut cpu, "4");

        editor.key(&Key::Named(Named::Escape), Modifiers::empty(), &mut cpu);
        assert_eq!(editor.pending(), None);
        assert_eq!(editor.region(), Some(Region::Ram));

        editor.key(&Key::Named(Named::Escape), Modifiers::empty(), &mut cpu);
        assert_eq!(editor.region(), None);
    }

    #[test]
    fn copies_and_pastes_hex() {
        let (mut editor, mut cpu) = (Editor::default(), Lyminal::new());
        for (i, byte) in b"Hello".iter().enumerate() {
            cpu.set_ram(i, *byte);
        }
        editor.select(Region::Ram, 0, false);
        editor.select(Region::Ram, 4, true);
        assert_eq!(editor.copy_hex(&cpu), "48 65 6C 6C 6F");
        assert_eq!(editor.copy_text(&cpu), "Hello");

        let copy = editor.key(&Key::Character("c".into()), Modifiers::CTRL, &mut cpu);
        assert_eq!(copy, KeyAction::Copy("48 65 6C 6C 6F".to_string()));

        editor.select(Region::Rom, 2, false);
        assert_eq!(editor.paste("48 65 6c, 0x6C 6F", &mut cpu), Ok(5));
        assert_eq!(&cpu.rom()[2..7], b"Hello");
        assert_eq!(editor.range(), 2..=6, "selects what was pasted");

        assert_eq!(editor.paste("C30F", &mut cpu), Ok(2));
        assert_eq!(&cpu.rom()[2..4], &[0xC3, 0x0F]);
    }

    #[test]
    fn paste_stops_at_the_end_and_rejects_non_hex() {
        let (mut editor, mut cpu) = (Editor::default(), Lyminal::new());
        editor.select(Region::Ram, lyminal::RAM_SIZE - 1, false);
        assert_eq!(editor.paste("11 22 33", &mut cpu), Ok(1));
        assert_eq!(cpu.ram()[lyminal::RAM_SIZE - 1], 0x11);

        assert!(editor.paste("12 zz", &mut cpu).is_err());
        assert!(editor.paste("123", &mut cpu).is_err());
    }
}
