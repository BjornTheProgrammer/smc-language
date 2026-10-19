// No console window behind the GUI on Windows; debug builds keep it for logs
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

use std::path::{Path, PathBuf};
use std::time::{Duration, SystemTime};

use iced::{
    Alignment, Color, Element, Length, Size, Subscription, Task, clipboard,
    keyboard::{self, Modifiers},
    mouse, time,
    widget::{
        Column, PaneGrid, Row, Space, button, column, container, mouse_area, pane_grid, responsive,
        row, scrollable, scrollable::Scrollbar, slider, space, text, text_input,
    },
    window,
};

use iced::widget::pane_grid::Edge;

use crate::{
    controller::Controller,
    editor::{Editor, KeyAction, Region},
    emulators::{
        Emulator, Listing,
        batpu_2::Batpu2Emulator,
        lyminal::{self, Flag, Lyminal},
    },
    screen::Screen,
    workspace::{Panel, Workspace},
};

pub mod controller;
pub mod editor;
pub mod emulators;
pub mod screen;
pub mod ui;
pub mod workspace;

const PROGRAM_CAPACITY: usize = 1023;
const MEMORY_ROW_BYTES: usize = 8;
/// The fastest clock that can be typed in, in Hz; the slider goes up to `SLIDER_MAX`
const CLOCK_MAX: u32 = 1_000_000;
const SLIDER_MAX: u32 = 1000;
/// How often the run loop executes a batch of instructions
const TICK: Duration = Duration::from_millis(16);
/// How often the loaded program's file is checked for changes
const RELOAD_CHECK: Duration = Duration::from_millis(500);

fn main() -> iced::Result {
    iced::application(App::new, App::update, App::view)
        .subscription(App::subscription)
        .theme(ui::theme())
        .title("SMC Emulator")
        .window(window::Settings {
            min_size: Some(Size::new(720.0, 480.0)),
            ..window::Settings::default()
        })
        .window_size((1280.0, 800.0))
        .centered()
        .run()
}

#[derive(Default)]
pub struct App {
    running: bool,
    game_speed: u32,
    /// The clock as typed, while it's being edited
    clock_input: Option<String>,
    program: Vec<u8>,
    program_pos: usize,
    emulator: Emulator,
    screen: Screen,
    controller: Controller,
    workspace: Workspace,
    /// The loaded program's file, which RESET keeps
    program_path: Option<PathBuf>,
    /// When that file was last changed, to reload it when it's saved again
    program_modified: Option<SystemTime>,
    /// The program's source and the ROM bytes of each line, for assembled programs
    listing: Option<Listing>,
    /// The last load result or error, shown in the status bar and console
    notice: Option<String>,
    console_input: String,
    /// Instructions owed to the clock that didn't fit in a whole tick
    clock_debt: f64,
    screen_version: u64,
    editor: Editor,
    /// Held modifier keys, so shift-click extends a selection
    modifiers: Modifiers,
}

impl App {
    /// Starts empty, or with the program given on the command line
    pub fn new() -> Self {
        let mut app = Self {
            game_speed: 100,
            workspace: Workspace::load(),
            ..Self::default()
        };
        // Size the screen to the machine before it draws anything
        if let Emulator::Lyminal(cpu) = &app.emulator {
            app.screen.show(lyminal::SCREEN_SIZE, cpu.io.pixels());
        }
        if let Some(path) = std::env::args_os().nth(1) {
            app.load(Path::new(&path));
        }
        app
    }

    pub fn update(&mut self, message: Message) -> Task<Message> {
        match message {
            Message::Start => {
                self.running = !self.running && self.can_run();
                self.editor.cancel_digit();
            }
            Message::Step => {
                if let Some(cpu) = self.lyminal_mut() {
                    cpu.step();
                }
                self.note_halt();
                self.sync_screen();
            }
            Message::Reset => {
                self.running = false;
                self.notice = None;
                if let Some(cpu) = self.lyminal_mut() {
                    cpu.reset();
                }
                self.sync_screen();
            }
            Message::Load => {
                return Task::perform(
                    rfd::AsyncFileDialog::new()
                        .add_filter("Lyminal", &["lasm", "lym"])
                        .pick_file(),
                    |file| Message::Loaded(file.map(|file| file.path().to_path_buf())),
                );
            }
            Message::Loaded(Some(path)) => {
                self.editor.clear();
                self.load(&path);
            }
            Message::Loaded(None) => {}
            Message::Tick => self.run_tick(),
            Message::CheckProgram => self.reload_if_changed(),
            Message::ConsoleInput(input) => self.console_input = input,
            Message::ConsoleSubmit => {
                let mut line = std::mem::take(&mut self.console_input).into_bytes();
                line.push(b'\n');
                if let Some(cpu) = self.lyminal_mut() {
                    cpu.io.send_input(&line);
                }
            }
            Message::Key(event) => return self.key(event),
            Message::SelectByte(region, index) => {
                self.editor.select(region, index, self.modifiers.shift());
            }
            Message::SelectLine(index) => {
                if let Some(line) = self.listing.as_ref().and_then(|l| l.lines.get(index))
                    && let Some(start) = line.address
                {
                    self.editor.select(Region::Rom, start, false);
                    self.editor
                        .select(Region::Rom, start + line.bytes.len() - 1, true);
                }
            }
            Message::CopyHex => {
                if let Some(cpu) = self.lyminal() {
                    return clipboard::write(self.editor.copy_hex(cpu));
                }
            }
            Message::CopyText => {
                if let Some(cpu) = self.lyminal() {
                    return clipboard::write(self.editor.copy_text(cpu));
                }
            }
            Message::Paste => return clipboard::read().map(Message::Pasted),
            Message::Pasted(Some(text)) => self.paste(&text),
            Message::Pasted(None) => {}
            Message::Screen(screen_msg) => self.screen.update(screen_msg),
            Message::Controller(controller_msg) => {
                self.controller.update(controller_msg);
                let state = self.controller.state();
                if let Some(cpu) = self.lyminal_mut() {
                    cpu.io.gamepad = state;
                }
            }
            Message::ToggleFlag(flag) => {
                if !self.running
                    && let Some(cpu) = self.lyminal_mut()
                {
                    cpu.toggle_flag(flag);
                }
            }
            Message::SetSpeed(speed) => {
                self.game_speed = speed;
                self.clock_input = None;
            }
            Message::ClockInput(input) => {
                let digits: String = input.chars().filter(char::is_ascii_digit).collect();
                if let Ok(speed) = digits.parse::<u32>() {
                    self.game_speed = speed.clamp(1, CLOCK_MAX);
                }
                self.clock_input = Some(digits);
            }
            Message::ClockSubmit => self.clock_input = None,
            Message::Workspace(workspace_msg) => {
                self.workspace.update(workspace_msg);
                self.workspace.save();
            }
        }

        Task::none()
    }

    pub fn subscription(&self) -> Subscription<Message> {
        let keys = keyboard::listen().map(Message::Key);
        let tick = self
            .running
            .then(|| time::every(TICK).map(|_| Message::Tick));
        let reload = self
            .program_path
            .is_some()
            .then(|| time::every(RELOAD_CHECK).map(|_| Message::CheckProgram));
        Subscription::batch([Some(keys), tick, reload].into_iter().flatten())
    }

    /// Keys edit the selected bytes while paused, and press gamepad buttons otherwise
    fn key(&mut self, event: keyboard::Event) -> Task<Message> {
        if let keyboard::Event::ModifiersChanged(modifiers) = event {
            self.modifiers = modifiers;
        }

        if let keyboard::Event::KeyPressed { key, modifiers, .. } = &event
            && !self.running
            && let Emulator::Lyminal(cpu) = &mut self.emulator
        {
            match self.editor.key(key, *modifiers, cpu) {
                KeyAction::Handled => return Task::none(),
                KeyAction::Copy(text) => return clipboard::write(text),
                KeyAction::Paste => return clipboard::read().map(Message::Pasted),
                KeyAction::Ignored => {}
            }
        }

        match Controller::message_for(&event) {
            Some(message) => self.update(Message::Controller(message)),
            None => Task::none(),
        }
    }

    fn paste(&mut self, text: &str) {
        if self.running {
            return;
        }
        let Emulator::Lyminal(cpu) = &mut self.emulator else {
            return;
        };
        self.notice = Some(match self.editor.paste(text, cpu) {
            Ok(1) => "Pasted 1 byte".to_string(),
            Ok(count) => format!("Pasted {count} bytes"),
            Err(error) => error,
        });
    }

    fn load(&mut self, path: &Path) {
        self.running = false;
        self.notice = Some(match self.open(path) {
            Ok(()) => format!("Loaded {}", path.display()),
            Err(error) => error,
        });
    }

    /// Swaps in the program at `path` on a fresh machine, keeping the gamepad state
    fn open(&mut self, path: &Path) -> Result<(), String> {
        let (mut emulator, listing) = Emulator::load(path)?;
        if let Emulator::Lyminal(cpu) = &mut emulator {
            cpu.io.gamepad = self.controller.state();
        }
        self.emulator = emulator;
        self.listing = listing;
        self.program_path = Some(path.to_path_buf());
        self.program_modified = modified(path);
        self.screen_version = u64::MAX;
        self.sync_screen();
        Ok(())
    }

    /// Reloads the program when its file was saved since it was loaded. It keeps running
    /// if it was; a version that doesn't assemble leaves the old one in place.
    fn reload_if_changed(&mut self) {
        let Some(path) = self.program_path.clone() else {
            return;
        };
        // Missing while an editor swaps in the saved file, so try again next time
        let Some(modified) = modified(&path) else {
            return;
        };
        if self.program_modified == Some(modified) {
            return;
        }
        // Don't retry a broken save until it changes again
        self.program_modified = Some(modified);

        let running = self.running;
        match self.open(&path) {
            Ok(()) => {
                self.editor.clear();
                self.running = running;
                self.notice = Some(format!("Reloaded {}", path.display()));
            }
            Err(error) => {
                self.notice = Some(format!("Reload failed, kept the last version. {error}"))
            }
        }
    }

    /// Runs the instructions the clock owes since the last tick
    fn run_tick(&mut self) {
        self.clock_debt += self.game_speed as f64 * TICK.as_secs_f64();
        let steps = self.clock_debt as u64;
        self.clock_debt -= steps as f64;

        if let Some(cpu) = self.lyminal_mut() {
            for _ in 0..steps {
                cpu.step();
                if cpu.halted() {
                    break;
                }
            }
        }
        self.note_halt();
        self.sync_screen();
    }

    /// Whether a program is loaded that can run, which a halted one can't until reset
    fn can_run(&self) -> bool {
        self.lyminal().is_some_and(|cpu| !cpu.halted())
    }

    fn halted(&self) -> bool {
        self.lyminal().is_some_and(Lyminal::halted)
    }

    /// Pauses once the program halts
    fn note_halt(&mut self) {
        if self.halted() {
            self.running = false;
            self.notice = Some("Halted: the PC passed 255. RESET to run again".to_string());
        }
    }

    /// Redraws the display when the program changed the screen
    fn sync_screen(&mut self) {
        let Emulator::Lyminal(cpu) = &self.emulator else {
            return;
        };
        if cpu.io.screen_version() != self.screen_version {
            self.screen_version = cpu.io.screen_version();
            self.screen.show(lyminal::SCREEN_SIZE, cpu.io.pixels());
        }
    }

    fn lyminal(&self) -> Option<&Lyminal> {
        match &self.emulator {
            Emulator::Lyminal(cpu) => Some(cpu),
            Emulator::Batpu2(_) => None,
        }
    }

    fn lyminal_mut(&mut self) -> Option<&mut Lyminal> {
        match &mut self.emulator {
            Emulator::Lyminal(cpu) => Some(cpu),
            Emulator::Batpu2(_) => None,
        }
    }

    pub fn view(&self) -> Element<'_, Message> {
        let grid = PaneGrid::new(self.workspace.panes(), |_pane, panel, _maximized| {
            let body = match panel {
                Panel::Screen => self.screen_panel(),
                Panel::Gamepad => self.gamepad_panel(),
                Panel::Debugger => scrollable(self.debugger()).spacing(4).into(),
                Panel::Memory => self.memory(),
                Panel::Console => self.console(),
                Panel::Source => self.source(),
            };

            pane_grid::Content::new(container(body).padding(8))
                .title_bar(self.panel_title_bar(*panel))
                .style(ui::panel_body)
        })
        .spacing(8)
        .min_size(120)
        .on_drag(|event| Message::Workspace(workspace::Message::Dragged(event)))
        .on_resize(6, |event| {
            Message::Workspace(workspace::Message::Resized(event))
        })
        .style(ui::dock_grid);

        container(column![
            self.toolbar(),
            container(grid).padding(8).height(Length::Fill),
            self.status_bar(),
        ])
        .width(Length::Fill)
        .height(Length::Fill)
        .style(ui::app_background)
        .into()
    }

    /// The display, with its size and whether the machine is running
    fn screen_panel(&self) -> Element<'_, Message> {
        column![
            row![
                text(format!("{0}×{0}", self.screen.size()))
                    .size(11)
                    .font(ui::MONO)
                    .color(ui::TEXT_DIM),
                space().width(Length::Fill),
                container(
                    text(match () {
                        _ if self.running => "● RUNNING",
                        _ if self.halted() => "■ HALTED",
                        _ => "○ IDLE",
                    })
                    .size(9)
                    .font(ui::BOLD),
                )
                .padding([2, 8])
                .style(ui::status_pill(self.running)),
            ]
            .spacing(8)
            .align_y(Alignment::Center),
            self.screen.view().map(Message::Screen),
        ]
        .spacing(8)
        .into()
    }

    fn gamepad_panel(&self) -> Element<'_, Message> {
        // The pad sizes itself to the space inside the padding
        const PADDING: [f32; 2] = [4.0, 8.0];
        responsive(|size| {
            let inner = Size::new(
                size.width - PADDING[1] * 2.0,
                size.height - PADDING[0] * 2.0,
            );
            container(self.controller.view(inner).map(Message::Controller))
                .padding(PADDING)
                .center_y(Length::Fill)
                .into()
        })
        .into()
    }

    fn panel_title_bar(&self, panel: Panel) -> pane_grid::TitleBar<'_, Message> {
        let current = self.workspace.edge(panel);
        let dock = |edge: Edge| {
            let active = current.is_some_and(|current| workspace::same_edge(edge, current));
            button(dock_glyph(
                edge,
                if active { ui::ACCENT } else { ui::TEXT_MUTED },
            ))
            .padding(4)
            .style(ui::icon_button(active))
            .on_press(Message::Workspace(workspace::Message::Dock(panel, edge)))
        };

        let controls = row![
            dock(Edge::Left),
            dock(Edge::Bottom),
            dock(Edge::Right),
            button(text("×").size(14).line_height(1.0))
                .padding([1, 6])
                .style(ui::icon_button(false))
                .on_press_maybe(
                    self.workspace
                        .can_close(panel)
                        .then_some(Message::Workspace(workspace::Message::Close(panel))),
                ),
        ]
        .spacing(2)
        .align_y(Alignment::Center);

        // Only the close button when the panel is too narrow for the dock buttons
        let compact = button(text("×").size(14).line_height(1.0))
            .padding([1, 6])
            .style(ui::icon_button(false))
            .on_press_maybe(
                self.workspace
                    .can_close(panel)
                    .then_some(Message::Workspace(workspace::Message::Close(panel))),
            );

        pane_grid::TitleBar::new(
            row![text("⠿").size(12).color(ui::TEXT_DIM), label(panel.title()),]
                .spacing(8)
                .align_y(Alignment::Center),
        )
        .controls(pane_grid::Controls::dynamic(controls, compact))
        .always_show_controls()
        .padding([4, 8])
        .style(ui::panel_header)
    }

    fn toolbar(&self) -> Element<'_, Message> {
        let brand = row![
            container(text("S").size(13).font(ui::BOLD))
                .width(24)
                .height(24)
                .center_x(24)
                .center_y(24)
                .style(ui::logo_mark),
            text("SMC").size(14).font(ui::BOLD),
            text(ui::spaced("EMULATOR")).size(9).color(ui::TEXT_MUTED),
        ]
        .spacing(8)
        .align_y(Alignment::Center);

        let can_run = self.can_run();
        let run = button(
            text(if self.running {
                "❚❚  PAUSE"
            } else {
                "▶  RUN"
            })
            .size(12)
            .font(ui::BOLD),
        )
        .padding([5, 16])
        .style(ui::run_button)
        .on_press_maybe(can_run.then_some(Message::Start));

        let typed = match &self.clock_input {
            Some(input) => input.clone(),
            None => self.game_speed.to_string(),
        };
        let clock = row![
            label("CLOCK"),
            slider(
                1..=SLIDER_MAX,
                self.game_speed.min(SLIDER_MAX),
                Message::SetSpeed
            )
            .width(120)
            .step(1u32),
            text_input("Hz", &typed)
                .on_input(Message::ClockInput)
                .on_submit(Message::ClockSubmit)
                .width(56)
                .size(11)
                .font(ui::MONO)
                .align_x(iced::alignment::Horizontal::Right),
            text("Hz").size(11).font(ui::MONO).color(ui::TEXT_MUTED),
        ]
        .spacing(8)
        .align_y(Alignment::Center);

        // Groups stay whole and wrap onto more lines when the window is narrow
        let content = row![
            brand,
            row![
                run,
                tool_button("STEP", (can_run && !self.running).then_some(Message::Step)),
                tool_button("RESET", self.lyminal().is_some().then_some(Message::Reset)),
            ]
            .spacing(8),
            row![
                tool_button("LOAD", Some(Message::Load)),
                tool_button("CLEAR", Some(Message::Screen(screen::Message::Clear))),
            ]
            .spacing(8),
            clock,
            row![
                label("PANELS"),
                Row::with_children(Panel::ALL.map(|panel| panel_toggle(
                    panel,
                    self.workspace.is_open(panel),
                    !self.workspace.is_open(panel) || self.workspace.can_close(panel),
                )))
                .spacing(4),
            ]
            .spacing(8)
            .align_y(Alignment::Center),
        ]
        .spacing(16)
        .align_y(Alignment::Center)
        .wrap()
        .vertical_spacing(6);

        container(content)
            .padding([6, 12])
            .width(Length::Fill)
            .style(ui::toolbar)
            .into()
    }

    fn debugger(&self) -> Element<'_, Message> {
        match &self.emulator {
            Emulator::Batpu2(cpu) => self.batpu2_debugger(cpu),
            Emulator::Lyminal(cpu) => self.lyminal_debugger(cpu),
        }
    }

    fn lyminal_debugger<'a>(&'a self, cpu: &'a Lyminal) -> Element<'a, Message> {
        let register = |index: usize, name: &str| {
            let value = Region::Registers.read(cpu, index);
            let selected = self.editor.is_selected(Region::Registers, index);
            let shown = match self.editor.pending() {
                Some(digit) if self.editor.is_cursor(Region::Registers, index) => {
                    format!("{digit:X}_")
                }
                _ => format!("{value:02X}"),
            };
            mouse_area(register_tile(name, value, shown, selected))
                .on_press(Message::SelectByte(Region::Registers, index))
                .interaction(mouse::Interaction::Pointer)
        };
        let registers = column![
            row![register(0, "X"), register(1, "Y")].spacing(4),
            row![register(2, "Z"), register(3, "A")].spacing(4),
        ]
        .spacing(4);

        let state = column![
            readout("PC", format!("0x{:02X}", cpu.pc())),
            readout(
                "STATUS",
                match () {
                    _ if cpu.halted() => "Halted",
                    _ if self.running => "Running",
                    _ => "Paused",
                }
                .to_string()
            ),
            readout("CYCLES", cpu.cycles().to_string()),
            readout("PROGRAM", format!("{} B", self.program_size(cpu))),
            readout("GAMEPAD", format!("{:08b}", cpu.io.gamepad)),
        ]
        .spacing(4);

        column![
            label("REGISTERS"),
            registers,
            // The flags move under the label when the panel is narrow
            row![
                container(label("FLAGS")).width(64),
                row![
                    self.lyminal_flag("Z", cpu.zero(), Flag::Zero),
                    self.lyminal_flag("C", cpu.carry(), Flag::Carry),
                    self.lyminal_flag("P", cpu.predicate(), Flag::Predicate),
                    self.lyminal_flag("A", cpu.a_mode(), Flag::AMode),
                ]
                .spacing(4),
            ]
            .align_y(Alignment::Center)
            .wrap()
            .vertical_spacing(4),
            text(
                "P: next MEM is skipped · A: A reads its value, not -1 · click to flip while paused"
            )
            .size(10)
            .color(ui::TEXT_DIM),
            horizontal_divider(),
            label("STATE"),
            state,
        ]
        .spacing(8)
        .into()
    }

    /// A flag tile that flips the bit when clicked while paused
    fn lyminal_flag<'a>(&self, name: &'static str, set: bool, which: Flag) -> Element<'a, Message> {
        let tile = flag(name, set);
        match self.running {
            true => tile,
            false => mouse_area(tile)
                .on_press(Message::ToggleFlag(which))
                .interaction(mouse::Interaction::Pointer)
                .into(),
        }
    }

    /// Bytes up to the last non-zero byte of ROM
    fn program_size(&self, cpu: &Lyminal) -> usize {
        cpu.rom()
            .iter()
            .rposition(|&byte| byte != 0)
            .map_or(0, |i| i + 1)
    }

    fn batpu2_debugger<'a>(&'a self, cpu: &'a Batpu2Emulator) -> Element<'a, Message> {
        let registers = cpu.registers();
        let register_grid = Column::with_children((0..4).map(|i| {
            row![
                register(&format!("r{i}"), registers[i]),
                register(&format!("r{}", i + 4), registers[i + 4])
            ]
            .spacing(4)
            .into()
        }))
        .spacing(4);

        let state = column![
            readout("PC", format!("0x{:03X}", self.program_pos)),
            readout(
                "PROGRAM",
                format!("{} / {PROGRAM_CAPACITY} B", self.program.len())
            ),
            readout("INPUT", format!("0x{:02X}", self.controller.state())),
        ]
        .spacing(4);

        column![
            label("REGISTERS"),
            register_grid,
            row![
                label("FLAGS"),
                space().width(Length::Fill),
                flag("Z", cpu.zero_flag()),
                flag("C", cpu.carry_flag()),
            ]
            .spacing(4)
            .align_y(Alignment::Center),
            horizontal_divider(),
            label("STATE"),
            state,
            horizontal_divider(),
            label("BREAKPOINTS"),
            text("None set").size(11).color(ui::TEXT_DIM),
        ]
        .spacing(8)
        .into()
    }

    fn memory(&self) -> Element<'_, Message> {
        match &self.emulator {
            Emulator::Batpu2(cpu) => self.batpu2_memory(cpu),
            Emulator::Lyminal(cpu) => column![
                self.edit_bar(),
                self.editable_section("PROGRAM ROM", Region::Rom, cpu, Some(cpu.pc() as usize)),
                self.editable_section("DATA RAM", Region::Ram, cpu, None),
            ]
            .spacing(8)
            .into(),
        }
    }

    /// Copy and paste for the selection, or how to start editing
    fn edit_bar(&self) -> Element<'_, Message> {
        let Some(region) = self.editor.region() else {
            return text("Click a byte to edit · shift-click selects a range")
                .size(10)
                .color(ui::TEXT_DIM)
                .into();
        };

        let range = self.editor.range();
        let count = range.end() - range.start() + 1;
        let paste = (!self.running).then_some(Message::Paste);
        let hint = match self.running {
            true => "Pause to edit",
            false => "Type hex · Ctrl+C / Ctrl+V · Esc",
        };

        row![
            text(format!("{count} B"))
                .size(10)
                .font(ui::MONO)
                .color(ui::TEXT_MUTED),
            tool_button("COPY", Some(Message::CopyHex)),
        ]
        .push((region == Region::Ram).then(|| tool_button("COPY TEXT", Some(Message::CopyText))))
        .push(tool_button("PASTE", paste))
        .push(text(hint).size(10).color(ui::TEXT_DIM))
        .spacing(6)
        .align_y(Alignment::Center)
        .wrap()
        .vertical_spacing(4)
        .into()
    }

    /// A hex dump of `region` whose bytes can be selected and typed over
    fn editable_section<'a>(
        &'a self,
        title: &str,
        region: Region,
        cpu: &'a Lyminal,
        current: Option<usize>,
    ) -> Element<'a, Message> {
        let ascii = region == Region::Ram;
        let rows = (0..region.len(cpu)).step_by(MEMORY_ROW_BYTES).map(|start| {
            let indexes = start..(start + MEMORY_ROW_BYTES).min(region.len(cpu));
            let cells = Row::with_children(indexes.clone().map(|i| {
                let byte = region.read(cpu, i);
                let selected = self.editor.is_selected(region, i);
                let cursor = self.editor.is_cursor(region, i);
                let shown = match self.editor.pending() {
                    Some(digit) if cursor => format!("{digit:X}_"),
                    _ => format!("{byte:02X}"),
                };
                let color = match () {
                    _ if current == Some(i) => ui::ACCENT,
                    _ if selected => ui::TEXT,
                    _ => ui::TEXT_MUTED,
                };

                mouse_area(
                    container(text(shown).size(11).font(ui::MONO).color(color))
                        .padding([0, 2])
                        .style(ui::byte_cell(selected, cursor)),
                )
                .on_press(Message::SelectByte(region, i))
                .interaction(mouse::Interaction::Pointer)
                .into()
            }))
            .spacing(1);

            let chars: String = indexes
                .map(|i| match region.read(cpu, i) {
                    b if b.is_ascii_graphic() || b == b' ' => b as char,
                    _ => '·',
                })
                .collect();

            row![
                text(format!("{start:02X}"))
                    .size(11)
                    .font(ui::MONO)
                    .color(ui::SECONDARY),
                cells,
            ]
            .push(ascii.then(|| text(chars).size(11).font(ui::MONO).color(ui::TEXT_DIM)))
            .spacing(10)
            .align_y(Alignment::Center)
            .into()
        });

        column![
            row![
                label(title),
                space().width(Length::Fill),
                text(format!("{} B", region.len(cpu)))
                    .size(10)
                    .font(ui::MONO)
                    .color(ui::TEXT_DIM),
            ],
            container(
                scrollable(
                    Column::with_children(rows)
                        .spacing(1)
                        .padding(iced::Padding::new(6.0).left(8.0)),
                )
                // Rows keep their width; narrow panels scroll sideways
                .direction(scrollable::Direction::Both {
                    vertical: Scrollbar::new().spacing(4),
                    horizontal: Scrollbar::new().spacing(4),
                })
                .width(Length::Fill)
                .height(Length::Fill),
            )
            .height(Length::Fill)
            .width(Length::Fill)
            .style(ui::inset),
        ]
        .spacing(8)
        .into()
    }

    /// The program's source next to the ROM bytes each line became, with the line at the PC
    /// highlighted. Clicking a line selects its bytes in the memory panel.
    fn source(&self) -> Element<'_, Message> {
        let (Some(listing), Some(cpu)) = (&self.listing, self.lyminal()) else {
            return text("Load a .lasm program to see which ROM bytes each line became")
                .size(11)
                .color(ui::TEXT_DIM)
                .into();
        };

        // Fixed columns so the source lines up; monospace digits are about 7px wide
        const CHAR: f32 = 7.0;
        let byte_chars = listing
            .lines
            .iter()
            .map(|l| l.bytes.len() * 3)
            .max()
            .unwrap_or(0);
        let text_chars = listing
            .lines
            .iter()
            .map(|l| l.text.len())
            .max()
            .unwrap_or(0);
        let width = (4 + byte_chars + text_chars) as f32 * CHAR + 28.0;

        let current = listing.line_at(cpu.pc() as usize);
        let rows = listing.lines.iter().enumerate().map(|(index, line)| {
            let range = line
                .address
                .map(|start| start..start + line.bytes.len())
                .unwrap_or_default();
            let live: Vec<u8> = range
                .clone()
                .map(|address| cpu.rom().get(address).copied().unwrap_or(0))
                .collect();
            let edited = live != line.bytes;
            let selected = range
                .clone()
                .any(|address| self.editor.is_selected(Region::Rom, address));

            let row = container(
                row![
                    text(line.address.map(|a| format!("{a:02X}")).unwrap_or_default())
                        .size(11)
                        .font(ui::MONO)
                        .color(ui::SECONDARY)
                        .width(3.0 * CHAR),
                    text(
                        live.iter()
                            .map(|b| format!("{b:02X}"))
                            .collect::<Vec<_>>()
                            .join(" ")
                    )
                    .size(11)
                    .font(ui::MONO)
                    .color(if edited {
                        ui::ACCENT_BRIGHT
                    } else {
                        ui::TEXT_MUTED
                    })
                    .width(byte_chars as f32 * CHAR),
                    text(&line.text)
                        .size(11)
                        .font(ui::MONO)
                        .color(match line.address {
                            Some(_) => ui::TEXT,
                            None => ui::TEXT_DIM,
                        })
                        .wrapping(text::Wrapping::None),
                ]
                .spacing(10),
            )
            .width(width)
            .padding([0, 4])
            .style(ui::listing_line(current == Some(index), selected));

            match line.address {
                Some(_) => mouse_area(row)
                    .on_press(Message::SelectLine(index))
                    .interaction(mouse::Interaction::Pointer)
                    .into(),
                None => row.into(),
            }
        });

        column![
            text("Click a line to select its bytes in ROM")
                .size(10)
                .color(ui::TEXT_DIM),
            container(
                scrollable(
                    Column::with_children(rows)
                        .spacing(1)
                        .padding(iced::Padding::new(6.0).left(4.0)),
                )
                .direction(scrollable::Direction::Both {
                    vertical: Scrollbar::new().spacing(4),
                    horizontal: Scrollbar::new().spacing(4),
                })
                .width(Length::Fill)
                .height(Length::Fill),
            )
            .height(Length::Fill)
            .width(Length::Fill)
            .style(ui::inset),
        ]
        .spacing(8)
        .into()
    }

    fn console(&self) -> Element<'_, Message> {
        let Some(cpu) = self.lyminal() else {
            return text("Load a Lyminal program to use the ASCII port")
                .size(11)
                .color(ui::TEXT_DIM)
                .into();
        };

        let output = String::from_utf8_lossy(cpu.io.output()).into_owned();
        let notice = self
            .notice
            .as_ref()
            .filter(|notice| !notice.starts_with("Loaded"))
            .map(|notice| {
                text(notice.clone())
                    .size(11)
                    .font(ui::MONO)
                    .color(ui::ACCENT)
            });

        column![
            row![
                label("ASCII PORT"),
                space().width(Length::Fill),
                text(format!("port {}", lyminal::ASCII_IO))
                    .size(10)
                    .font(ui::MONO)
                    .color(ui::TEXT_DIM),
            ],
            container(
                scrollable(
                    column![text(output).size(12).font(ui::MONO)]
                        .push(notice)
                        .spacing(6)
                        .padding(6),
                )
                .anchor_bottom()
                .width(Length::Fill)
                .height(Length::Fill),
            )
            .height(Length::Fill)
            .width(Length::Fill)
            .style(ui::inset),
            text_input(
                "Type input for the program, then Enter",
                &self.console_input
            )
            .on_input(Message::ConsoleInput)
            .on_submit(Message::ConsoleSubmit)
            .size(12)
            .font(ui::MONO),
        ]
        .spacing(8)
        .into()
    }

    fn batpu2_memory<'a>(&'a self, cpu: &'a Batpu2Emulator) -> Element<'a, Message> {
        let rows = cpu
            .program()
            .chunks(MEMORY_ROW_BYTES)
            .enumerate()
            .map(|(i, bytes)| {
                let address = i * MEMORY_ROW_BYTES;
                let current = (address..address + MEMORY_ROW_BYTES).contains(&self.program_pos);
                let hex = bytes
                    .iter()
                    .map(|b| format!("{b:02X}"))
                    .collect::<Vec<_>>()
                    .join(" ");

                row![
                    text(format!("{address:03X}"))
                        .size(11)
                        .font(ui::MONO)
                        .color(if current { ui::ACCENT } else { ui::SECONDARY }),
                    text(hex).size(11).font(ui::MONO).color(if current {
                        ui::TEXT
                    } else {
                        ui::TEXT_MUTED
                    }),
                ]
                .spacing(10)
                .into()
            });

        column![
            row![
                label("PROGRAM ROM"),
                space().width(Length::Fill),
                text(format!("{PROGRAM_CAPACITY} B"))
                    .size(10)
                    .font(ui::MONO)
                    .color(ui::TEXT_DIM),
            ],
            container(
                scrollable(
                    Column::with_children(rows)
                        .spacing(1)
                        .padding(iced::Padding::new(6.0).left(8.0).right(16.0)),
                )
                .width(Length::Fill)
                .height(Length::Fill),
            )
            .height(Length::Fill)
            .width(Length::Fill)
            .style(ui::inset),
        ]
        .spacing(8)
        .into()
    }

    fn status_bar(&self) -> Element<'_, Message> {
        let item = |name: &'static str, value: String| {
            row![
                text(name).size(10).color(ui::TEXT_DIM),
                text(value).size(10).font(ui::MONO).color(ui::TEXT_MUTED),
            ]
            .spacing(4)
        };

        let architecture = match self.emulator {
            Emulator::Batpu2(_) => "BatPU-2",
            Emulator::Lyminal(_) => "Lyminal",
        };

        container(
            row![
                item("ARCH", architecture.to_string()),
                match self.lyminal() {
                    Some(cpu) => item("PC", format!("0x{:02X}", cpu.pc())),
                    None => item("PC", format!("0x{:03X}", self.program_pos)),
                },
                match self.lyminal() {
                    Some(cpu) => item("ROM", format!("{} B", self.program_size(cpu))),
                    None => item(
                        "ROM",
                        format!("{}/{PROGRAM_CAPACITY} B", self.program.len())
                    ),
                },
                item("CLOCK", format!("{} Hz", self.game_speed)),
                item("INPUT", format!("{:08b}", self.controller.state())),
                space().width(Length::Fill),
                text(
                    self.notice
                        .as_deref()
                        .and_then(|notice| notice.lines().next())
                        .unwrap_or_default()
                        .to_string()
                )
                .size(10)
                .color(ui::TEXT_MUTED),
                text(concat!("v", env!("CARGO_PKG_VERSION")))
                    .size(10)
                    .color(ui::TEXT_DIM),
            ]
            .spacing(16),
        )
        .padding([3, 12])
        .width(Length::Fill)
        .style(ui::toolbar)
        .into()
    }
}

fn register<'a>(name: &str, value: u8) -> Element<'a, Message> {
    register_tile(name, value, format!("{value:02X}"), false)
}

/// A register's name, hex value (or `shown` while editing) and decimal value
fn register_tile<'a>(name: &str, value: u8, shown: String, selected: bool) -> Element<'a, Message> {
    container(
        row![
            text(name.to_string())
                .size(11)
                .font(ui::MONO)
                .color(ui::SECONDARY),
            space().width(Length::Fill),
            text(shown).size(12).font(ui::MONO).color(if value == 0 {
                ui::TEXT_DIM
            } else {
                ui::TEXT
            }),
            text(format!("{value:>3}"))
                .size(10)
                .font(ui::MONO)
                .color(ui::TEXT_DIM),
        ]
        .spacing(6)
        .align_y(Alignment::Center),
    )
    .padding([4, 8])
    .width(Length::Fill)
    .style(ui::inset_selected(selected))
    .into()
}

fn flag<'a>(name: &'static str, set: bool) -> Element<'a, Message> {
    container(text(name).size(11).font(ui::BOLD))
        .width(28)
        .height(22)
        .center_x(28)
        .center_y(22)
        .style(ui::flag(set))
        .into()
}

fn modified(path: &Path) -> Option<SystemTime> {
    std::fs::metadata(path)
        .and_then(|meta| meta.modified())
        .ok()
}

fn label<'a>(name: &str) -> Element<'a, Message> {
    text(ui::spaced(name))
        .wrapping(text::Wrapping::None)
        .size(9)
        .font(ui::BOLD)
        .color(ui::TEXT_MUTED)
        .into()
}

fn readout<'a>(name: &str, value: String) -> Element<'a, Message> {
    row![
        text(name.to_string()).size(11).color(ui::TEXT_MUTED),
        space().width(Length::Fill),
        text(value).size(11).font(ui::MONO),
    ]
    .into()
}

fn horizontal_divider<'a>() -> Element<'a, Message> {
    container(Space::new())
        .width(Length::Fill)
        .height(1)
        .style(ui::divider)
        .into()
}

fn tool_button(label: &str, message: Option<Message>) -> Element<'_, Message> {
    button(
        text(label)
            .size(11)
            .font(ui::BOLD)
            .wrapping(text::Wrapping::None),
    )
    .padding([5, 10])
    .style(ui::tool_button)
    .on_press_maybe(message)
    .into()
}

/// Opens or closes a panel; `enabled` is false for the last open panel
fn panel_toggle<'a>(panel: Panel, open: bool, enabled: bool) -> Element<'a, Message> {
    button(
        text(panel.title())
            .size(11)
            .font(ui::BOLD)
            .wrapping(text::Wrapping::None),
    )
    .padding([5, 7])
    .style(ui::tab(open))
    .on_press_maybe(enabled.then_some(Message::Workspace(workspace::Message::Toggle(panel))))
    .into()
}

/// A tiny window outline with a filled strip on the side the panel docks to.
fn dock_glyph<'a>(edge: Edge, color: Color) -> Element<'a, Message> {
    let strip = || container(Space::new()).style(ui::glyph_fill(color));

    let inner: Element<'a, Message> = match edge {
        Edge::Left => row![strip().width(4).height(Length::Fill), space()].into(),
        Edge::Right => row![
            space().width(Length::Fill),
            strip().width(4).height(Length::Fill)
        ]
        .into(),
        Edge::Top => column![strip().height(3).width(Length::Fill), space()].into(),
        Edge::Bottom => column![
            space().height(Length::Fill),
            strip().height(3).width(Length::Fill)
        ]
        .into(),
    };

    container(inner)
        .width(14)
        .height(11)
        .padding(1)
        .style(ui::glyph_frame(color))
        .into()
}

#[derive(Debug, Clone)]
pub enum Message {
    Start,
    Step,
    Reset,
    Load,
    Loaded(Option<PathBuf>),
    Tick,
    CheckProgram,
    ConsoleInput(String),
    ConsoleSubmit,
    Key(keyboard::Event),
    SelectByte(Region, usize),
    /// Selects the ROM bytes of a source listing line
    SelectLine(usize),
    CopyHex,
    CopyText,
    Paste,
    Pasted(Option<String>),
    SetSpeed(u32),
    ToggleFlag(Flag),
    ClockInput(String),
    ClockSubmit,
    Workspace(workspace::Message),
    Screen(screen::Message),
    Controller(controller::Message),
}

#[cfg(test)]
mod tests {
    use super::*;

    const PROGRAMS: &str = concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../smc-assembler/tests/lyminal/programs"
    );

    #[test]
    fn runs_a_loaded_program_on_the_clock() {
        let mut app = App {
            game_speed: 1000,
            ..App::default()
        };
        let _ = app.update(Message::Loaded(Some(PathBuf::from(format!(
            "{PROGRAMS}/hello-world.lasm"
        )))));
        assert!(app.lyminal().is_some(), "{:?}", app.notice);

        let _ = app.update(Message::Start);
        assert!(app.running);
        // 1000 Hz for 16 ms is 16 instructions a tick; hello world takes about 100
        for _ in 0..8 {
            let _ = app.update(Message::Tick);
        }
        let cpu = app.lyminal().unwrap();
        assert_eq!(cpu.cycles(), 128);
        assert_eq!(&cpu.ram()[..16], b"Hello, world! :3");

        let _ = app.update(Message::Reset);
        assert!(!app.running);
        assert_eq!(app.lyminal().unwrap().ram()[0], 0);
    }

    fn key_press(key: &str, modifiers: Modifiers) -> Message {
        Message::Key(keyboard::Event::KeyPressed {
            key: keyboard::Key::Character(key.into()),
            modified_key: keyboard::Key::Character(key.into()),
            physical_key: keyboard::key::Physical::Unidentified(
                keyboard::key::NativeCode::Unidentified,
            ),
            location: keyboard::Location::Standard,
            modifiers,
            text: None,
            repeat: false,
        })
    }

    fn hello_app() -> App {
        let mut app = App::default();
        let _ = app.update(Message::Loaded(Some(PathBuf::from(format!(
            "{PROGRAMS}/hello-world.lasm"
        )))));
        app
    }

    #[test]
    fn source_listing_maps_each_line_to_its_rom_bytes() {
        let mut app = hello_app();
        let listing = app
            .listing
            .clone()
            .expect("assembled programs have a listing");

        let first = &listing.lines[0];
        assert_eq!(first.text, "LDI X 15 ; We want 16 values for our LUT");
        assert_eq!(first.address, Some(0));
        assert_eq!(listing.line_at(0), Some(0));
        assert!(
            listing
                .lines
                .iter()
                .any(|line| line.text == ".init" && line.address.is_none()),
            "labels have no bytes"
        );

        // In order, the lines' bytes are the program
        let bytes: Vec<u8> = listing.lines.iter().flat_map(|l| l.bytes.clone()).collect();
        assert_eq!(&app.lyminal().unwrap().rom()[..bytes.len()], &bytes[..]);

        let second = &listing.lines[1];
        let start = second.address.unwrap();
        let _ = app.update(Message::SelectLine(1));
        assert_eq!(app.editor.range(), start..=start + second.bytes.len() - 1);
        assert_eq!(app.editor.region(), Some(Region::Rom));
    }

    #[test]
    fn keys_edit_while_paused_and_play_while_running() {
        let mut app = hello_app();
        let _ = app.update(Message::SelectByte(Region::Ram, 0));

        let _ = app.update(key_press("a", Modifiers::empty()));
        let _ = app.update(key_press("b", Modifiers::empty()));
        assert_eq!(app.lyminal().unwrap().ram()[0], 0xAB);
        assert_eq!(app.controller.state(), 0, "a didn't press Left");

        let _ = app.update(Message::Start);
        let _ = app.update(key_press("a", Modifiers::empty()));
        assert_ne!(app.controller.state(), 0, "running, a presses Left");
        assert_eq!(app.lyminal().unwrap().ram()[1], 0, "and edits nothing");
    }

    #[test]
    fn shift_click_extends_and_paste_writes() {
        let mut app = hello_app();
        let _ = app.update(Message::SelectByte(Region::Ram, 2));
        let _ = app.update(Message::Key(keyboard::Event::ModifiersChanged(
            Modifiers::SHIFT,
        )));
        let _ = app.update(Message::SelectByte(Region::Ram, 5));
        assert_eq!(app.editor.range(), 2..=5);

        let _ = app.update(Message::Pasted(Some("48 69".to_string())));
        assert_eq!(&app.lyminal().unwrap().ram()[2..4], b"Hi");
        assert_eq!(app.notice.as_deref(), Some("Pasted 2 bytes"));

        let _ = app.update(Message::Pasted(Some("nope".to_string())));
        assert!(app.notice.as_deref().unwrap().starts_with("Cannot paste"));
    }

    #[test]
    fn pauses_when_the_program_halts() {
        let mut app = App {
            game_speed: 1000,
            ..App::default()
        };
        let path = std::env::temp_dir().join("smc-emu-halt.lasm");
        // Falls off the end of the program and through empty ROM to 255
        std::fs::write(&path, "LSI X 1\n").unwrap();
        let _ = app.update(Message::Loaded(Some(path)));

        let _ = app.update(Message::Start);
        for _ in 0..20 {
            let _ = app.update(Message::Tick);
        }
        assert!(app.halted());
        assert!(!app.running, "paused itself");
        assert_eq!(app.lyminal().unwrap().cycles(), 256);
        assert!(app.notice.as_deref().unwrap().starts_with("Halted"));

        let _ = app.update(Message::Start);
        assert!(!app.running, "a halted program can't run");

        let _ = app.update(Message::Reset);
        assert!(!app.halted() && app.can_run());
        assert!(app.notice.is_none());
    }

    #[test]
    fn reloads_the_program_when_its_file_is_saved() {
        let path = std::env::temp_dir().join("smc-emu-reload.lasm");
        // Saves can land within the same clock tick, so each one moves the time forward
        let mut saved = SystemTime::now();
        let mut save = |source: &str| {
            std::fs::write(&path, source).unwrap();
            saved += Duration::from_secs(1);
            std::fs::File::options()
                .write(true)
                .open(&path)
                .and_then(|file| file.set_modified(saved))
                .unwrap();
        };
        save("LSI X 1\n");

        let mut app = App {
            game_speed: 1000,
            ..App::default()
        };
        let _ = app.update(Message::Loaded(Some(path.clone())));
        let first = app.lyminal().unwrap().rom()[0];
        let _ = app.update(Message::Start);
        let _ = app.update(Message::Tick);
        let ran = app.lyminal().unwrap().cycles();
        assert!(ran > 0);

        let _ = app.update(Message::CheckProgram);
        assert_eq!(
            app.lyminal().unwrap().cycles(),
            ran,
            "unchanged, so not reloaded"
        );

        save("LSI Y 2\n");
        let _ = app.update(Message::CheckProgram);
        let cpu = app.lyminal().unwrap();
        assert_ne!(cpu.rom()[0], first);
        assert_eq!(cpu.cycles(), 0, "starts over");
        assert!(app.running, "and keeps running");
        assert!(app.notice.as_deref().unwrap().starts_with("Reloaded"));

        let second = app.lyminal().unwrap().rom()[0];
        save("NOT AN INSTRUCTION\n");
        let _ = app.update(Message::CheckProgram);
        assert_eq!(
            app.lyminal().unwrap().rom()[0],
            second,
            "keeps the last version"
        );
        assert!(app.notice.as_deref().unwrap().starts_with("Reload failed"));

        app.notice = None;
        let _ = app.update(Message::CheckProgram);
        assert_eq!(app.notice, None, "a broken save isn't retried");
    }

    #[test]
    fn flipping_a_flag_while_paused_changes_the_branch() {
        let mut app = App::default();
        let path = std::env::temp_dir().join("smc-emu-flags.lasm");
        // Z is clear after LSI X 1, so the jump to .taken is skipped
        std::fs::write(
            &path,
            "LSI X 1\nPIZ\nJMP .taken\nLSI Y 2\n.taken\nLSI Z 1\n",
        )
        .unwrap();
        let _ = app.update(Message::Loaded(Some(path)));
        let _ = app.update(Message::Step);
        assert!(!app.lyminal().unwrap().zero());

        let _ = app.update(Message::ToggleFlag(Flag::Zero));
        assert!(app.lyminal().unwrap().zero());
        for _ in 0..3 {
            let _ = app.update(Message::Step);
        }
        let cpu = app.lyminal().unwrap();
        assert_eq!(cpu.registers()[1], 0, "jumped over LSI Y 2");
        assert_eq!(cpu.registers()[2], 1);

        let _ = app.update(Message::Start);
        let _ = app.update(Message::ToggleFlag(Flag::Carry));
        assert!(!app.lyminal().unwrap().carry(), "running ignores clicks");
    }

    #[test]
    fn clock_can_be_typed() {
        let mut app = App {
            game_speed: 100,
            ..App::default()
        };
        let typed = |input: &str, app: &mut App| {
            let _ = app.update(Message::ClockInput(input.to_string()));
            app.game_speed
        };

        assert_eq!(typed("2500", &mut app), 2500, "past the slider's 1000");
        assert_eq!(typed("12 Hz", &mut app), 12, "only digits count");
        assert_eq!(typed("", &mut app), 12, "empty keeps the last clock");
        assert_eq!(app.clock_input.as_deref(), Some(""));
        assert_eq!(typed("0", &mut app), 1);
        assert_eq!(typed("99999999", &mut app), CLOCK_MAX);

        let _ = app.update(Message::ClockSubmit);
        assert_eq!(app.clock_input, None, "Enter shows the clock in use");

        let _ = app.update(Message::ClockInput("50".to_string()));
        let _ = app.update(Message::SetSpeed(700));
        assert_eq!((app.game_speed, app.clock_input.as_deref()), (700, None));
    }

    #[test]
    fn sketch_draws_with_the_gamepad_and_echoes_the_console() {
        let mut app = App::default();
        let _ = app.update(Message::Loaded(Some(PathBuf::from(format!(
            "{PROGRAMS}/sketch.lasm"
        )))));
        let run = |app: &mut App, steps: usize| {
            for _ in 0..steps {
                let _ = app.update(Message::Step);
            }
        };
        let lit = |app: &App| -> Vec<(usize, usize)> {
            let pixels = app.lyminal().unwrap().io.pixels();
            (0..pixels.len())
                .filter(|&i| pixels[i])
                .map(|i| (i % lyminal::SCREEN_SIZE, i / lyminal::SCREEN_SIZE))
                .collect()
        };
        let hold = |app: &mut App, button: controller::Button, steps: usize| {
            let _ = app.update(Message::Controller(controller::Message::Pressed(button)));
            run(app, steps);
            let _ = app.update(Message::Controller(controller::Message::Released(button)));
        };

        run(&mut app, 200);
        assert_eq!(app.lyminal().unwrap().io.output(), b"Sketch\n");
        assert_eq!(lit(&app), [(8, 8)], "starts in the middle");

        // Each pass of the loop moves one pixel, leaving a trail
        hold(&mut app, controller::Button::Right, 200);
        hold(&mut app, controller::Button::Down, 200);
        let trail = lit(&app);
        assert!(
            trail.contains(&(9, 8)) && trail.contains(&(10, 8)),
            "{trail:?}"
        );
        let &(x, y) = trail.iter().max_by_key(|(x, y)| (*y, *x)).unwrap();
        assert!(x > 8 && y > 8, "went right then down: {trail:?}");

        let _ = app.update(Message::ConsoleInput("hi".to_string()));
        let _ = app.update(Message::ConsoleSubmit);
        run(&mut app, 300);
        assert_eq!(app.lyminal().unwrap().io.output(), b"Sketch\nhi\n");

        // A clears everything but the dot it keeps drawing
        hold(&mut app, controller::Button::A, 100);
        run(&mut app, 100);
        assert_eq!(lit(&app), [(x, y)]);
    }

    #[test]
    fn console_and_gamepad_reach_the_program() {
        let mut app = App::default();
        let path = std::env::temp_dir().join("smc-emu-echo.lasm");
        // Echo one input byte, then show the gamepad on the screen's X register
        std::fs::write(
            &path,
            "MOV Y 252\nMLD X [Y]\nMST [Y] X\nMOV Y 253\nMLD X [Y]\nMOV Y 248\nMST [Y] X\n",
        )
        .unwrap();
        let _ = app.update(Message::Loaded(Some(path)));

        let _ = app.update(Message::ConsoleInput("Q".to_string()));
        let _ = app.update(Message::ConsoleSubmit);
        let _ = app.update(Message::Controller(controller::Message::Pressed(
            controller::Button::A,
        )));
        for _ in 0..7 {
            let _ = app.update(Message::Step);
        }

        let cpu = app.lyminal().unwrap();
        assert_eq!(cpu.io.output(), b"Q");
        assert_eq!(cpu.registers()[0], app.controller.state());
        assert_ne!(app.controller.state(), 0);
    }
}
