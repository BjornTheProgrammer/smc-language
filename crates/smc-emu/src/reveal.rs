//! Scrolling a list of fixed height rows so some of them are in view

use std::ops::RangeInclusive;

use iced::advanced::widget::operation::Scrollable;
use iced::advanced::widget::operation::scrollable::AbsoluteOffset;
use iced::advanced::widget::{Id, Operation, operate};
use iced::{Rectangle, Task, Vector};

/// Scrolls the scrollable with `id` to show `rows` when any of them are off screen, centering
/// them if they fit. Rows start `top` pixels into the content and are `pitch` pixels apart.
pub fn rows<T: Send + 'static>(
    id: impl Into<Id>,
    top: f32,
    pitch: f32,
    rows: RangeInclusive<usize>,
) -> Task<T> {
    operate(Reveal::new(id.into(), top, pitch, rows))
}

struct Reveal {
    id: Id,
    /// Where the rows start and end in the content, in pixels
    top: f32,
    bottom: f32,
}

impl Reveal {
    fn new(id: Id, top: f32, pitch: f32, rows: RangeInclusive<usize>) -> Self {
        Self {
            id,
            top: top + *rows.start() as f32 * pitch,
            bottom: top + (*rows.end() + 1) as f32 * pitch,
        }
    }
}

impl<T> Operation<T> for Reveal {
    fn traverse(&mut self, operate: &mut dyn FnMut(&mut dyn Operation<T>)) {
        operate(self);
    }

    fn scrollable(
        &mut self,
        id: Option<&Id>,
        bounds: Rectangle,
        content_bounds: Rectangle,
        translation: Vector,
        state: &mut dyn Scrollable,
    ) {
        if id != Some(&self.id) {
            return;
        }
        let (view_top, view_bottom) = (translation.y, translation.y + bounds.height);
        if self.top >= view_top && self.bottom <= view_bottom {
            return;
        }

        // Center the rows, or show where they start when they're taller than the view
        let centered = (self.top + self.bottom - bounds.height) / 2.0;
        let max = (content_bounds.height - bounds.height).max(0.0);
        let y = centered.min(self.top).clamp(0.0, max);
        state.scroll_to(AbsoluteOffset {
            x: None,
            y: Some(y),
        });
    }
}

#[cfg(test)]
mod tests {
    use iced::{Point, Size};

    use super::*;

    /// Records where it was asked to scroll
    struct Recorder(Option<f32>);

    impl Scrollable for Recorder {
        fn snap_to(
            &mut self,
            _: iced::advanced::widget::operation::scrollable::RelativeOffset<Option<f32>>,
        ) {
        }
        fn scroll_to(&mut self, offset: AbsoluteOffset<Option<f32>>) {
            self.0 = offset.y;
        }
        fn scroll_by(&mut self, _: AbsoluteOffset, _: Rectangle, _: Rectangle) {}
    }

    /// A view 10 rows tall over 100 rows of 10px, scrolled to `offset`
    fn scroll(id: &'static str, rows: RangeInclusive<usize>, offset: f32) -> Option<f32> {
        let mut reveal = Reveal::new(Id::new("list"), 0.0, 10.0, rows);
        let mut state = Recorder(None);
        Operation::<()>::scrollable(
            &mut reveal,
            Some(&Id::new(id)),
            Rectangle::new(Point::ORIGIN, Size::new(100.0, 100.0)),
            Rectangle::new(Point::ORIGIN, Size::new(100.0, 1000.0)),
            Vector::new(0.0, offset),
            &mut state,
        );
        state.0
    }

    #[test]
    fn leaves_rows_that_are_already_in_view() {
        assert_eq!(scroll("list", 2..=3, 0.0), None);
        assert_eq!(scroll("list", 50..=51, 450.0), None);
    }

    #[test]
    fn centers_rows_that_are_off_screen() {
        // Rows 50 and 51 span 500..520, so the 100px view starts at 460
        assert_eq!(scroll("list", 50..=51, 0.0), Some(460.0));
        assert_eq!(
            scroll("list", 0..=0, 300.0),
            Some(0.0),
            "but not before the start"
        );
        assert_eq!(scroll("list", 99..=99, 0.0), Some(900.0), "or past the end");
    }

    #[test]
    fn shows_the_start_of_rows_taller_than_the_view() {
        assert_eq!(scroll("list", 20..=40, 0.0), Some(200.0));
    }

    #[test]
    fn ignores_other_scrollables() {
        assert_eq!(scroll("other", 50..=51, 0.0), None);
    }
}
