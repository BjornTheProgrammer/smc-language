//! Dockable panel layout: every panel can be pinned to the left, right or bottom edge,
//! stacked together, resized, closed and reopened in place. The layout is saved between
//! runs.

use std::path::PathBuf;

use iced::widget::pane_grid::{
    self, Axis, Configuration, DragEvent, Edge, Node, Pane, Region, ResizeEvent, Target,
};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Panel {
    Screen,
    Gamepad,
    Debugger,
    Memory,
    Console,
    Source,
}

impl Panel {
    pub const ALL: [Panel; 6] = [
        Panel::Screen,
        Panel::Gamepad,
        Panel::Debugger,
        Panel::Memory,
        Panel::Console,
        Panel::Source,
    ];

    pub fn title(self) -> &'static str {
        match self {
            Panel::Screen => "SCREEN",
            Panel::Gamepad => "GAMEPAD",
            Panel::Debugger => "DEBUGGER",
            Panel::Memory => "MEMORY",
            Panel::Console => "CONSOLE",
            Panel::Source => "SOURCE",
        }
    }

    fn index(self) -> usize {
        self as usize
    }

    /// The share of the window a panel takes when docked alone to an edge
    fn share(self, axis: Axis) -> f32 {
        match (self, axis) {
            (Panel::Screen, _) => 0.6,
            (Panel::Gamepad, _) => 0.22,
            (_, Axis::Vertical) => 0.24,
            (_, Axis::Horizontal) => 0.3,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Message {
    Dock(Panel, Edge),
    Close(Panel),
    Toggle(Panel),
    Dragged(DragEvent),
    Resized(ResizeEvent),
}

pub struct Workspace {
    panes: pane_grid::State<Panel>,
    /// Where each panel is docked, remembered while it is closed. The screen and gamepad
    /// start undocked, so other panels don't stack onto them.
    edges: [Option<Edge>; Panel::ALL.len()],
    /// Where the layout is saved; `None` keeps it in memory only
    path: Option<PathBuf>,
}

impl Default for Workspace {
    /// The screen with the gamepad under it, and the debugger and memory stacked on the
    /// right. The console starts closed, ready to open at the bottom, and the source on
    /// the left.
    fn default() -> Self {
        let column = |top, bottom, ratio| Configuration::Split {
            axis: Axis::Horizontal,
            ratio,
            a: Box::new(Configuration::Pane(top)),
            b: Box::new(Configuration::Pane(bottom)),
        };

        Self {
            panes: pane_grid::State::with_configuration(Configuration::Split {
                axis: Axis::Vertical,
                ratio: 0.74,
                a: Box::new(column(Panel::Screen, Panel::Gamepad, 0.8)),
                b: Box::new(column(Panel::Debugger, Panel::Memory, 0.5)),
            }),
            edges: [
                None,
                None,
                Some(Edge::Right),
                Some(Edge::Right),
                Some(Edge::Bottom),
                Some(Edge::Left),
            ],
            path: None,
        }
    }
}

impl Workspace {
    /// The saved layout, or the default if there is none or it can't be read
    pub fn load() -> Self {
        let path = dirs::config_dir().map(|dir| dir.join("smc-emu").join("layout.json"));
        let saved = path
            .as_ref()
            .and_then(|path| std::fs::read_to_string(path).ok())
            .and_then(|json| Self::from_json(&json));

        Self {
            path,
            ..saved.unwrap_or_default()
        }
    }

    /// Writes the layout to its file, if it has one
    pub fn save(&self) {
        let Some(path) = &self.path else {
            return;
        };
        let written = path
            .parent()
            .map_or(Ok(()), std::fs::create_dir_all)
            .and_then(|()| std::fs::write(path, self.to_json()));
        if let Err(error) = written {
            eprintln!(
                "Cannot save the panel layout to {}: {error}",
                path.display()
            );
        }
    }

    pub fn update(&mut self, message: Message) {
        match message {
            Message::Dock(panel, edge) => {
                if let Some(pane) = self.find(panel) {
                    self.place(panel, pane, edge);
                }
            }
            Message::Close(panel) => self.close(panel),
            Message::Toggle(panel) => match self.is_open(panel) {
                true => self.close(panel),
                false => self.open(panel),
            },
            Message::Dragged(DragEvent::Dropped { pane, target }) => self.dropped(pane, target),
            Message::Dragged(_) => {}
            Message::Resized(ResizeEvent { split, ratio }) => self.panes.resize(split, ratio),
        }
    }

    pub fn panes(&self) -> &pane_grid::State<Panel> {
        &self.panes
    }

    pub fn is_open(&self, panel: Panel) -> bool {
        self.find(panel).is_some()
    }

    /// Whether the panel can close, which the last open panel can't
    pub fn can_close(&self, panel: Panel) -> bool {
        self.is_open(panel) && self.panes.len() > 1
    }

    pub fn edge(&self, panel: Panel) -> Option<Edge> {
        self.edges[panel.index()]
    }

    fn find(&self, panel: Panel) -> Option<Pane> {
        self.panes
            .iter()
            .find(|(_, open)| **open == panel)
            .map(|(pane, _)| *pane)
    }

    fn close(&mut self, panel: Panel) {
        if self.can_close(panel)
            && let Some(pane) = self.find(panel)
        {
            self.panes.close(pane);
        }
    }

    /// Reopens a closed panel on the edge it was last docked to.
    fn open(&mut self, panel: Panel) {
        let Some((&any, _)) = self.panes.iter().next() else {
            return;
        };
        let edge = self.edge(panel).unwrap_or(match panel {
            Panel::Screen => Edge::Left,
            Panel::Gamepad => Edge::Bottom,
            _ => Edge::Right,
        });
        if let Some((pane, _)) = self.panes.split(Axis::Vertical, any, panel) {
            self.place(panel, pane, edge);
        }
    }

    /// Docks `pane` to `edge`, stacking it with any panel already docked there.
    fn place(&mut self, panel: Panel, pane: Pane, edge: Edge) {
        self.edges[panel.index()] = Some(edge);

        let neighbor = Panel::ALL
            .into_iter()
            .filter(|other| {
                *other != panel && self.edge(*other).is_some_and(|e| same_edge(e, edge))
            })
            .find_map(|other| self.find(other));

        match neighbor {
            Some(neighbor) => {
                let stack = match edge {
                    Edge::Left | Edge::Right => Edge::Bottom,
                    Edge::Top | Edge::Bottom => Edge::Right,
                };
                self.panes
                    .drop(pane, Target::Pane(neighbor, Region::Edge(stack)));
            }
            None => {
                self.panes.move_to_edge(pane, edge);
                self.settle(panel, pane);
            }
        }
    }

    fn dropped(&mut self, pane: Pane, target: Target) {
        let Some(&panel) = self.panes.get(pane) else {
            return;
        };

        // Work out which dock the panel ended up in, so reopening it later
        // puts it back there.
        let edge = match target {
            Target::Edge(edge) => Some(edge),
            Target::Pane(target, _) => self.panes.get(target).and_then(|other| self.edge(*other)),
        };

        self.panes.drop(pane, target);

        self.edges[panel.index()] = edge;
        self.settle(panel, pane);
    }

    /// Gives a panel docked alone to a window edge its own share of the space instead of
    /// an even split.
    fn settle(&mut self, panel: Panel, pane: Pane) {
        let Node::Split { id, axis, a, b, .. } = self.panes.layout() else {
            return;
        };
        let (split, axis) = (*id, *axis);
        let share = panel.share(axis);

        match (a.as_ref(), b.as_ref()) {
            (Node::Pane(p), _) if *p == pane => self.panes.resize(split, share),
            (_, Node::Pane(p)) if *p == pane => self.panes.resize(split, 1.0 - share),
            _ => {}
        }
    }

    fn to_json(&self) -> String {
        let saved = Saved {
            layout: self.layout(self.panes.layout()),
            edges: Panel::ALL
                .into_iter()
                .filter_map(|panel| Some((panel, DockEdge::from(self.edge(panel)?))))
                .collect(),
        };
        serde_json::to_string_pretty(&saved).expect("layouts always serialize")
    }

    /// A saved layout, if it's valid: each panel at most once, and at least one
    fn from_json(json: &str) -> Option<Self> {
        let saved: Saved = serde_json::from_str(json).ok()?;

        let mut seen = Vec::new();
        if !saved.layout.panels(&mut seen) || seen.is_empty() {
            return None;
        }

        let mut workspace = Self {
            panes: pane_grid::State::with_configuration(saved.layout.configuration()),
            ..Self::default()
        };
        workspace.edges = [None; Panel::ALL.len()];
        for (panel, edge) in saved.edges {
            workspace.edges[panel.index()] = Some(edge.into());
        }
        Some(workspace)
    }

    fn layout(&self, node: &Node) -> Layout {
        match node {
            Node::Split {
                axis, ratio, a, b, ..
            } => Layout::Split {
                vertical: *axis == Axis::Vertical,
                ratio: *ratio,
                a: Box::new(self.layout(a)),
                b: Box::new(self.layout(b)),
            },
            Node::Pane(pane) => {
                Layout::Panel(self.panes.get(*pane).copied().unwrap_or(Panel::Screen))
            }
        }
    }
}

/// The layout as saved to disk
#[derive(Debug, Serialize, Deserialize)]
struct Saved {
    layout: Layout,
    edges: Vec<(Panel, DockEdge)>,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
enum Layout {
    Split {
        /// Side by side, rather than one above the other
        vertical: bool,
        ratio: f32,
        a: Box<Layout>,
        b: Box<Layout>,
    },
    Panel(Panel),
}

impl Layout {
    /// Collects the panels into `seen`, returning false if one appears twice
    fn panels(&self, seen: &mut Vec<Panel>) -> bool {
        match self {
            Layout::Split { a, b, .. } => a.panels(seen) && b.panels(seen),
            Layout::Panel(panel) if seen.contains(panel) => false,
            Layout::Panel(panel) => {
                seen.push(*panel);
                true
            }
        }
    }

    fn configuration(&self) -> Configuration<Panel> {
        match self {
            Layout::Split {
                vertical,
                ratio,
                a,
                b,
            } => Configuration::Split {
                axis: if *vertical {
                    Axis::Vertical
                } else {
                    Axis::Horizontal
                },
                ratio: ratio.clamp(0.05, 0.95),
                a: Box::new(a.configuration()),
                b: Box::new(b.configuration()),
            },
            Layout::Panel(panel) => Configuration::Pane(*panel),
        }
    }
}

/// `Edge` doesn't implement `Serialize`.
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
enum DockEdge {
    Top,
    Bottom,
    Left,
    Right,
}

impl From<Edge> for DockEdge {
    fn from(edge: Edge) -> Self {
        match edge {
            Edge::Top => DockEdge::Top,
            Edge::Bottom => DockEdge::Bottom,
            Edge::Left => DockEdge::Left,
            Edge::Right => DockEdge::Right,
        }
    }
}

impl From<DockEdge> for Edge {
    fn from(edge: DockEdge) -> Self {
        match edge {
            DockEdge::Top => Edge::Top,
            DockEdge::Bottom => Edge::Bottom,
            DockEdge::Left => Edge::Left,
            DockEdge::Right => Edge::Right,
        }
    }
}

/// `Edge` doesn't implement `PartialEq`.
pub fn same_edge(a: Edge, b: Edge) -> bool {
    std::mem::discriminant(&a) == std::mem::discriminant(&b)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn layout(workspace: &Workspace) -> Layout {
        workspace.layout(workspace.panes.layout())
    }

    /// Which side of the root split `panel` is alone on, if it is
    fn root_side(workspace: &Workspace, panel: Panel) -> Option<(Axis, bool)> {
        let pane = workspace.find(panel)?;
        let Node::Split { axis, a, b, .. } = workspace.panes.layout() else {
            return None;
        };
        match (a.as_ref(), b.as_ref()) {
            (Node::Pane(p), _) if *p == pane => Some((*axis, true)),
            (_, Node::Pane(p)) if *p == pane => Some((*axis, false)),
            _ => None,
        }
    }

    #[test]
    fn starts_with_the_screen_and_gamepad_beside_stacked_tools() {
        let workspace = Workspace::default();

        for panel in [
            Panel::Screen,
            Panel::Gamepad,
            Panel::Debugger,
            Panel::Memory,
        ] {
            assert!(workspace.is_open(panel), "{panel:?}");
        }
        assert!(!workspace.is_open(Panel::Console));
        assert_eq!(
            layout(&workspace),
            Layout::Split {
                vertical: true,
                ratio: 0.74,
                a: Box::new(Layout::Split {
                    vertical: false,
                    ratio: 0.8,
                    a: Box::new(Layout::Panel(Panel::Screen)),
                    b: Box::new(Layout::Panel(Panel::Gamepad)),
                }),
                b: Box::new(Layout::Split {
                    vertical: false,
                    ratio: 0.5,
                    a: Box::new(Layout::Panel(Panel::Debugger)),
                    b: Box::new(Layout::Panel(Panel::Memory)),
                }),
            }
        );
    }

    #[test]
    fn closed_panel_reopens_where_it_was_docked() {
        let mut workspace = Workspace::default();
        workspace.update(Message::Dock(Panel::Debugger, Edge::Left));

        workspace.update(Message::Close(Panel::Debugger));
        assert!(!workspace.is_open(Panel::Debugger));

        workspace.update(Message::Toggle(Panel::Debugger));
        assert_eq!(
            root_side(&workspace, Panel::Debugger),
            Some((Axis::Vertical, true))
        );
    }

    #[test]
    fn the_last_panel_stays_open() {
        let mut workspace = Workspace::default();
        for panel in [Panel::Gamepad, Panel::Debugger, Panel::Memory] {
            workspace.update(Message::Close(panel));
        }
        assert!(!workspace.can_close(Panel::Screen));

        workspace.update(Message::Toggle(Panel::Screen));
        assert!(workspace.is_open(Panel::Screen));
        assert_eq!(workspace.panes.len(), 1);
    }

    #[test]
    fn saves_and_restores_the_layout() {
        let mut workspace = Workspace::default();
        workspace.update(Message::Dock(Panel::Screen, Edge::Right));
        workspace.update(Message::Toggle(Panel::Console));
        workspace.update(Message::Close(Panel::Gamepad));

        let restored = Workspace::from_json(&workspace.to_json()).expect("saved layout loads");
        assert_eq!(layout(&restored), layout(&workspace));
        assert!(same_edge(
            restored.edge(Panel::Screen).unwrap(),
            Edge::Right
        ));
        assert!(restored.edge(Panel::Gamepad).is_none(), "never docked");
        assert!(!restored.is_open(Panel::Gamepad));
        assert!(restored.path.is_none());
    }

    #[test]
    fn writes_the_layout_file() {
        let path = std::env::temp_dir()
            .join("smc-emu-test")
            .join("layout.json");
        let _ = std::fs::remove_file(&path);
        let mut workspace = Workspace {
            path: Some(path.clone()),
            ..Workspace::default()
        };
        workspace.update(Message::Toggle(Panel::Console));
        workspace.save();

        let json = std::fs::read_to_string(&path).expect("layout was saved");
        let restored = Workspace::from_json(&json).expect("saved layout loads");
        assert!(restored.is_open(Panel::Console));
    }

    #[test]
    fn rejects_broken_saves() {
        assert!(Workspace::from_json("not json").is_none());

        let twice = r#"{"layout":{"Split":{"vertical":true,"ratio":0.5,
            "a":{"Panel":"Screen"},"b":{"Panel":"Screen"}}},"edges":[]}"#;
        assert!(Workspace::from_json(twice).is_none());
    }
}
