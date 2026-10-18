//! The BatPU-2 controller: eight buttons read by programs as a single byte.

use iced::keyboard::{self, Key, key::Named};
use iced::widget::{Space, column, container, mouse_area, row, text};
use iced::{Alignment, Element, Length};

use crate::ui;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Button {
    Start,
    Select,
    A,
    B,
    Up,
    Right,
    Down,
    Left,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Message {
    Pressed(Button),
    Released(Button),
}

impl Button {
    /// Bit position in the controller input byte (Start is the most significant bit).
    fn mask(self) -> u8 {
        let bit = match self {
            Button::Start => 7,
            Button::Select => 6,
            Button::A => 5,
            Button::B => 4,
            Button::Up => 3,
            Button::Right => 2,
            Button::Down => 1,
            Button::Left => 0,
        };
        1 << bit
    }

    fn from_key(key: &Key) -> Option<Self> {
        match key.as_ref() {
            Key::Named(Named::ArrowUp) => Some(Button::Up),
            Key::Named(Named::ArrowDown) => Some(Button::Down),
            Key::Named(Named::ArrowLeft) => Some(Button::Left),
            Key::Named(Named::ArrowRight) => Some(Button::Right),
            Key::Named(Named::Enter) => Some(Button::Start),
            Key::Named(Named::Backspace) => Some(Button::Select),
            Key::Character(c) => match c.to_ascii_lowercase().as_str() {
                "w" => Some(Button::Up),
                "s" => Some(Button::Down),
                "a" => Some(Button::Left),
                "d" => Some(Button::Right),
                "x" | "k" => Some(Button::A),
                "z" | "j" => Some(Button::B),
                _ => None,
            },
            _ => None,
        }
    }
}

#[derive(Debug, Default)]
pub struct Controller {
    state: u8,
}

impl Controller {
    pub fn update(&mut self, message: Message) {
        match message {
            Message::Pressed(button) => self.state |= button.mask(),
            Message::Released(button) => self.state &= !button.mask(),
        }
    }

    /// The byte a program sees when it reads the controller port.
    pub fn state(&self) -> u8 {
        self.state
    }

    fn is_held(&self, button: Button) -> bool {
        self.state & button.mask() != 0
    }

    /// The button press or release a key event means, if any
    pub fn message_for(event: &keyboard::Event) -> Option<Message> {
        match event {
            keyboard::Event::KeyPressed {
                key, repeat: false, ..
            } => Button::from_key(key).map(Message::Pressed),
            keyboard::Event::KeyReleased { key, .. } => {
                Button::from_key(key).map(Message::Released)
            }
            _ => None,
        }
    }

    pub fn view(&self) -> Element<'_, Message> {
        const KEY: f32 = 30.0;
        let gap = || Space::new().width(KEY).height(KEY);

        let dpad = column![
            row![gap(), self.key(Button::Up, "▲", KEY, KEY, false), gap()],
            row![
                self.key(Button::Left, "◀", KEY, KEY, false),
                gap(),
                self.key(Button::Right, "▶", KEY, KEY, false),
            ],
            row![gap(), self.key(Button::Down, "▼", KEY, KEY, false), gap()],
        ]
        .spacing(2);

        let menu = row![
            self.key(Button::Select, "SELECT", 64.0, 22.0, true),
            self.key(Button::Start, "START", 64.0, 22.0, true),
        ]
        .spacing(8);

        let face = row![
            column![
                Space::new().height(18),
                self.key(Button::B, "B", 40.0, 40.0, true)
            ],
            column![
                self.key(Button::A, "A", 40.0, 40.0, true),
                Space::new().height(18)
            ],
        ]
        .spacing(10);

        let input = column![
            text(ui::spaced("INPUT"))
                .size(9)
                .font(ui::BOLD)
                .color(ui::TEXT_DIM),
            text(format!("{:04b} {:04b}", self.state >> 4, self.state & 0x0F))
                .size(14)
                .font(ui::MONO)
                .color(if self.state != 0 {
                    ui::ACCENT_BRIGHT
                } else {
                    ui::TEXT_MUTED
                }),
            text("WASD / ↑↓←→ · Z/J = B · X/K = A · Enter = Start · ⌫ = Select")
                .size(10)
                .color(ui::TEXT_DIM),
        ]
        .spacing(3)
        .align_x(Alignment::Center);

        row![
            dpad,
            container(column![menu, input].spacing(12).align_x(Alignment::Center))
                .center_x(Length::Fill),
            face,
        ]
        .align_y(Alignment::Center)
        .into()
    }

    fn key<'a>(
        &self,
        button: Button,
        label: &'a str,
        width: f32,
        height: f32,
        round: bool,
    ) -> Element<'a, Message> {
        let size = if label.len() > 3 { 9 } else { 12 };

        mouse_area(
            container(text(label).size(size).font(ui::BOLD))
                .width(width)
                .height(height)
                .center_x(width)
                .center_y(height)
                .style(ui::pad_key(self.is_held(button), round)),
        )
        .on_press(Message::Pressed(button))
        .on_release(Message::Released(button))
        .on_exit(Message::Released(button))
        .into()
    }
}
