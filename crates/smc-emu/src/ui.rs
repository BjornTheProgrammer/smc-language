//! Shared colors, fonts and widget styles for the emulator UI.

use iced::border::Radius;
use iced::font::Weight;
use iced::gradient::Linear;
use iced::widget::{button, container, pane_grid};
use iced::{Background, Border, Color, Degrees, Font, Shadow, Theme, Vector, color};

pub const BACKGROUND: Color = color!(0x0b0d16);
pub const TOOLBAR: Color = color!(0x0f121d);
pub const SURFACE: Color = color!(0x131726);
pub const SURFACE_RAISED: Color = color!(0x1a1f33);
pub const BORDER: Color = color!(0x232941);
pub const BORDER_STRONG: Color = color!(0x343c5c);

pub const TEXT: Color = color!(0xe7e9f2);
pub const TEXT_MUTED: Color = color!(0x8189a3);
pub const TEXT_DIM: Color = color!(0x4c5470);

pub const ACCENT: Color = color!(0xf5a524);
pub const ACCENT_BRIGHT: Color = color!(0xffc35a);
pub const ON_ACCENT: Color = color!(0x1a1206);
pub const SECONDARY: Color = color!(0xa996ff);

pub const BOLD: Font = Font {
    weight: Weight::Bold,
    ..Font::DEFAULT
};
pub const SEMIBOLD: Font = Font {
    weight: Weight::Semibold,
    ..Font::DEFAULT
};
pub const MONO: Font = Font::MONOSPACE;

const RADIUS: f32 = 6.0;

pub fn theme() -> Theme {
    Theme::custom(
        "SMC",
        iced::theme::Palette {
            background: BACKGROUND,
            text: TEXT,
            primary: ACCENT,
            success: color!(0x3ddc97),
            warning: ACCENT,
            danger: color!(0xff5c7a),
        },
    )
}

/// Spreads the characters of a label apart, since iced has no letter spacing.
pub fn spaced(label: &str) -> String {
    label
        .chars()
        .map(|c| c.to_string())
        .collect::<Vec<_>>()
        .join(" ")
}

pub fn with_alpha(color: Color, a: f32) -> Color {
    Color { a, ..color }
}

pub fn app_background(_theme: &Theme) -> container::Style {
    container::Style {
        background: Some(BACKGROUND.into()),
        text_color: Some(TEXT),
        ..container::Style::default()
    }
}

pub fn toolbar(_theme: &Theme) -> container::Style {
    container::Style {
        background: Some(TOOLBAR.into()),
        border: Border {
            color: BORDER,
            width: 1.0,
            radius: 0.0.into(),
        },
        ..container::Style::default()
    }
}

pub fn card(_theme: &Theme) -> container::Style {
    container::Style {
        background: Some(SURFACE.into()),
        border: Border {
            color: BORDER,
            width: 1.0,
            radius: RADIUS.into(),
        },
        ..container::Style::default()
    }
}

/// The panel framing the display, faintly tinted with the accent colors.
pub fn display_card(_theme: &Theme) -> container::Style {
    container::Style {
        background: Some(Background::Gradient(
            Linear::new(Degrees(120.0))
                .add_stop(0.0, color!(0x1a1610))
                .add_stop(0.55, color!(0x111421))
                .add_stop(1.0, color!(0x161228))
                .into(),
        )),
        ..card(_theme)
    }
}

pub fn inset(_theme: &Theme) -> container::Style {
    container::Style {
        background: Some(color!(0x0d1019).into()),
        border: Border {
            color: BORDER,
            width: 1.0,
            radius: 4.0.into(),
        },
        ..container::Style::default()
    }
}

pub fn logo_mark(_theme: &Theme) -> container::Style {
    container::Style {
        background: Some(Background::Gradient(
            Linear::new(Degrees(135.0))
                .add_stop(0.0, ACCENT_BRIGHT)
                .add_stop(1.0, color!(0xe0731a))
                .into(),
        )),
        border: Border {
            radius: 5.0.into(),
            ..Border::default()
        },
        text_color: Some(ON_ACCENT),
        ..container::Style::default()
    }
}

pub fn divider(_theme: &Theme) -> container::Style {
    container::Style {
        background: Some(BORDER.into()),
        ..container::Style::default()
    }
}

pub fn status_pill(active: bool) -> impl Fn(&Theme) -> container::Style {
    move |_theme| {
        let tint = if active { ACCENT } else { TEXT_DIM };
        container::Style {
            background: Some(with_alpha(tint, 0.1).into()),
            border: Border {
                color: with_alpha(tint, 0.45),
                width: 1.0,
                radius: 999.0.into(),
            },
            text_color: Some(if active { ACCENT_BRIGHT } else { TEXT_MUTED }),
            ..container::Style::default()
        }
    }
}

pub fn flag(set: bool) -> impl Fn(&Theme) -> container::Style {
    move |_theme| container::Style {
        background: Some(if set {
            ACCENT.into()
        } else {
            SURFACE_RAISED.into()
        }),
        border: Border {
            color: if set { ACCENT } else { BORDER },
            width: 1.0,
            radius: 4.0.into(),
        },
        text_color: Some(if set { ON_ACCENT } else { TEXT_DIM }),
        ..container::Style::default()
    }
}

/// A controller key; lights up while held.
pub fn pad_key(held: bool, round: bool) -> impl Fn(&Theme) -> container::Style {
    move |_theme| container::Style {
        background: Some(if held {
            Background::Gradient(
                Linear::new(Degrees(180.0))
                    .add_stop(0.0, ACCENT_BRIGHT)
                    .add_stop(1.0, ACCENT)
                    .into(),
            )
        } else {
            SURFACE_RAISED.into()
        }),
        border: Border {
            color: if held { ACCENT } else { BORDER_STRONG },
            width: 1.0,
            radius: if round { 999.0 } else { 5.0 }.into(),
        },
        shadow: Shadow {
            color: if held {
                with_alpha(ACCENT, 0.45)
            } else {
                Color::BLACK
            },
            offset: Vector::new(0.0, if held { 0.0 } else { 2.0 }),
            blur_radius: if held { 14.0 } else { 0.0 },
        },
        text_color: Some(if held { ON_ACCENT } else { TEXT_MUTED }),
        ..container::Style::default()
    }
}

pub fn run_button(_theme: &Theme, status: button::Status) -> button::Style {
    let (top, bottom, glow) = match status {
        button::Status::Hovered => (color!(0xffd27a), ACCENT, 0.5),
        button::Status::Pressed => (color!(0xf0a028), color!(0xd88a12), 0.2),
        button::Status::Disabled => (color!(0x5a4a2c), color!(0x46391f), 0.0),
        button::Status::Active => (ACCENT_BRIGHT, ACCENT, 0.3),
    };

    button::Style {
        background: Some(Background::Gradient(
            Linear::new(Degrees(180.0))
                .add_stop(0.0, top)
                .add_stop(1.0, bottom)
                .into(),
        )),
        text_color: ON_ACCENT,
        border: Border {
            radius: RADIUS.into(),
            ..Border::default()
        },
        shadow: Shadow {
            color: with_alpha(ACCENT, glow),
            offset: Vector::new(0.0, 2.0),
            blur_radius: 14.0,
        },
        ..button::Style::default()
    }
}

pub fn tool_button(_theme: &Theme, status: button::Status) -> button::Style {
    let (background, border, text_color) = match status {
        button::Status::Hovered => (SURFACE_RAISED, BORDER_STRONG, TEXT),
        button::Status::Pressed => (SURFACE, BORDER, TEXT),
        button::Status::Disabled => (SURFACE, BORDER, TEXT_DIM),
        button::Status::Active => (SURFACE, BORDER, TEXT_MUTED),
    };

    button::Style {
        background: Some(background.into()),
        text_color,
        border: Border {
            color: border,
            width: 1.0,
            radius: RADIUS.into(),
        },
        ..button::Style::default()
    }
}

pub fn tab(active: bool) -> impl Fn(&Theme, button::Status) -> button::Style {
    move |_theme, status| {
        let hovered = matches!(status, button::Status::Hovered);
        button::Style {
            background: Some(if active {
                with_alpha(ACCENT, 0.12).into()
            } else if hovered {
                SURFACE_RAISED.into()
            } else {
                Color::TRANSPARENT.into()
            }),
            text_color: if active {
                ACCENT_BRIGHT
            } else if hovered {
                TEXT
            } else {
                TEXT_MUTED
            },
            border: Border {
                color: if active {
                    with_alpha(ACCENT, 0.35)
                } else {
                    Color::TRANSPARENT
                },
                width: 1.0,
                radius: 4.0.into(),
            },
            ..button::Style::default()
        }
    }
}

pub fn dock_grid(_theme: &Theme) -> pane_grid::Style {
    pane_grid::Style {
        hovered_region: pane_grid::Highlight {
            background: with_alpha(ACCENT, 0.12).into(),
            border: Border {
                color: with_alpha(ACCENT, 0.7),
                width: 2.0,
                radius: RADIUS.into(),
            },
        },
        picked_split: pane_grid::Line {
            color: ACCENT,
            width: 2.0,
        },
        hovered_split: pane_grid::Line {
            color: with_alpha(ACCENT, 0.5),
            width: 2.0,
        },
    }
}

pub fn panel_header(_theme: &Theme) -> container::Style {
    container::Style {
        background: Some(SURFACE_RAISED.into()),
        border: Border {
            color: BORDER,
            width: 1.0,
            radius: Radius {
                top_left: RADIUS,
                top_right: RADIUS,
                bottom_right: 0.0,
                bottom_left: 0.0,
            },
        },
        text_color: Some(TEXT),
        ..container::Style::default()
    }
}

pub fn panel_body(_theme: &Theme) -> container::Style {
    container::Style {
        background: Some(SURFACE.into()),
        border: Border {
            color: BORDER,
            width: 1.0,
            radius: RADIUS.into(),
        },
        ..container::Style::default()
    }
}

pub fn icon_button(active: bool) -> impl Fn(&Theme, button::Status) -> button::Style {
    move |_theme, status| {
        let hovered = matches!(status, button::Status::Hovered | button::Status::Pressed);
        button::Style {
            background: Some(if active {
                with_alpha(ACCENT, 0.15).into()
            } else if hovered {
                BORDER_STRONG.into()
            } else {
                Color::TRANSPARENT.into()
            }),
            text_color: if active || hovered { TEXT } else { TEXT_MUTED },
            border: Border {
                radius: 3.0.into(),
                ..Border::default()
            },
            ..button::Style::default()
        }
    }
}

/// Outline of the little window drawn in dock buttons.
pub fn glyph_frame(color: Color) -> impl Fn(&Theme) -> container::Style {
    move |_theme| container::Style {
        border: Border {
            color,
            width: 1.0,
            radius: 2.0.into(),
        },
        ..container::Style::default()
    }
}

/// The filled strip showing which side a dock button pins to.
pub fn glyph_fill(color: Color) -> impl Fn(&Theme) -> container::Style {
    move |_theme| container::Style {
        background: Some(color.into()),
        ..container::Style::default()
    }
}

/// A byte in the memory view: tinted when selected, outlined at the cursor
pub fn byte_cell(selected: bool, cursor: bool) -> impl Fn(&Theme) -> container::Style {
    move |_theme| container::Style {
        background: selected.then(|| with_alpha(ACCENT, 0.18).into()),
        border: Border {
            color: if cursor { ACCENT } else { Color::TRANSPARENT },
            width: 1.0,
            radius: 2.0.into(),
        },
        ..container::Style::default()
    }
}

/// A line of the source listing: highlighted at the PC, tinted while its bytes are selected
pub fn listing_line(current: bool, selected: bool) -> impl Fn(&Theme) -> container::Style {
    move |_theme| container::Style {
        background: match (current, selected) {
            (true, _) => Some(with_alpha(ACCENT, 0.22).into()),
            (false, true) => Some(with_alpha(ACCENT, 0.1).into()),
            (false, false) => None,
        },
        border: Border {
            radius: 2.0.into(),
            ..Border::default()
        },
        ..container::Style::default()
    }
}

/// An inset outlined in the accent color while selected for editing
pub fn inset_selected(selected: bool) -> impl Fn(&Theme) -> container::Style {
    move |theme| {
        let mut style = inset(theme);
        if selected {
            style.border.color = ACCENT;
        }
        style
    }
}
