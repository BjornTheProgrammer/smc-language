use iced::widget::canvas::{self, Cache, Canvas, Geometry, Path, Stroke};
use iced::{Color, Element, Fill, Point, Rectangle, Renderer, Size, Theme};
use std::collections::HashSet;

pub const GRID_SIZE: usize = 50;

pub struct Screen {
    /// Pixels per side
    size: usize,
    pixels: HashSet<(usize, usize)>,
    cache: Cache,
}

impl Default for Screen {
    fn default() -> Self {
        Self {
            size: GRID_SIZE,
            pixels: HashSet::new(),
            cache: Cache::default(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Message {
    TurnOn(usize, usize),
    TurnOff(usize, usize),
    Clear,
}

impl Screen {
    pub fn update(&mut self, message: Message) {
        match message {
            Message::TurnOn(x, y) => {
                if x < self.size && y < self.size {
                    self.pixels.insert((x, y));
                    self.cache.clear();
                }
            }
            Message::TurnOff(x, y) => {
                self.pixels.remove(&(x, y));
                self.cache.clear();
            }
            Message::Clear => {
                self.pixels.clear();
                self.cache.clear();
            }
        }
    }

    pub fn size(&self) -> usize {
        self.size
    }

    /// Replaces the whole picture with a `size` × `size` frame, row by row
    pub fn show(&mut self, size: usize, frame: &[bool]) {
        self.size = size;
        self.pixels = frame
            .iter()
            .enumerate()
            .filter(|(_, lit)| **lit)
            .map(|(i, _)| (i % size, i / size))
            .collect();
        self.cache.clear();
    }

    pub fn view(&self) -> Element<'_, Message> {
        Canvas::new(self).width(Fill).height(Fill).into()
    }
}

impl canvas::Program<Message> for Screen {
    type State = ();

    fn draw(
        &self,
        _state: &Self::State,
        renderer: &Renderer,
        _theme: &Theme,
        bounds: Rectangle,
        _cursor: iced::mouse::Cursor,
    ) -> Vec<Geometry> {
        let geometry = self.cache.draw(renderer, bounds.size(), |frame| {
            // Fit the square matrix inside the available space and center it.
            const BEZEL: f32 = 14.0;
            let side = bounds.width.min(bounds.height) - BEZEL * 2.0;
            let cell = side / self.size as f32;
            let origin = Point::new((bounds.width - side) / 2.0, (bounds.height - side) / 2.0);

            // Bezel hugging the matrix
            let bezel = Path::rounded_rectangle(
                Point::new(origin.x - BEZEL, origin.y - BEZEL),
                Size::new(side + BEZEL * 2.0, side + BEZEL * 2.0),
                8.0.into(),
            );
            frame.fill(&bezel, Color::from_rgb8(7, 8, 13));
            frame.stroke(
                &bezel,
                Stroke::default()
                    .with_width(1.0)
                    .with_color(Color::from_rgb8(42, 48, 72)),
            );
            let gap = (cell * 0.12).max(1.0);
            let lit = Color::from_rgb8(255, 190, 92);
            let glow = Color::from_rgba8(245, 165, 36, 0.18);
            let unlit = Color::from_rgb8(20, 23, 36);

            for y in 0..self.size {
                for x in 0..self.size {
                    let top_left =
                        Point::new(origin.x + x as f32 * cell, origin.y + y as f32 * cell);

                    if self.pixels.contains(&(x, y)) {
                        let bloom = cell * 0.35;
                        frame.fill_rectangle(
                            Point::new(top_left.x - bloom, top_left.y - bloom),
                            Size::new(cell + bloom * 2.0, cell + bloom * 2.0),
                            glow,
                        );
                        frame.fill_rectangle(top_left, Size::new(cell, cell), lit);
                    } else {
                        frame.fill_rectangle(
                            Point::new(top_left.x + gap / 2.0, top_left.y + gap / 2.0),
                            Size::new(cell - gap, cell - gap),
                            unlit,
                        );
                    }
                }
            }
        });

        vec![geometry]
    }
}
