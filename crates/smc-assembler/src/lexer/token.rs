use std::path::Path;

#[derive(Debug, PartialEq, Clone)]
pub struct TokenSpan {
    pub token: Token,
    pub span: Span,
}

impl TokenSpan {
    pub fn new(token: Token, span: Span) -> Self {
        Self { token, span }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Span {
    start: usize,
    end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        debug_assert!(
            start <= end,
            "Invalid span: start ({}) must be <= end ({})",
            start,
            end
        );
        Span { start, end }
    }

    /// Calculate line and column from a byte offset in source text
    fn offset_to_location(source: &str, offset: usize) -> (usize, usize) {
        let mut line = 1;
        let mut col = 1;
        for (i, ch) in source.char_indices() {
            if i >= offset {
                break;
            }
            if ch == '\n' {
                line += 1;
                col = 1;
            } else {
                col += 1;
            }
        }
        (line, col)
    }

    /// Calculate line and column of the start position
    pub fn start_location(&self, source: &str) -> (usize, usize) {
        Self::offset_to_location(source, self.start)
    }

    /// Calculate line and column of the end position
    pub fn end_location(&self, source: &str) -> (usize, usize) {
        Self::offset_to_location(source, self.end)
    }

    /// Calculate line and column from source text (alias for start_location)
    pub fn location(&self, source: &str) -> (usize, usize) {
        self.start_location(source)
    }

    /// Get the text snippet from source
    pub fn snippet<'a>(&self, source: &'a str) -> &'a str {
        &source[self.start..self.end.min(source.len())]
    }

    /// Get the line containing this span's start
    pub fn get_line<'a>(&self, source: &'a str) -> &'a str {
        let lines: Vec<&str> = source.lines().collect();
        let (line_num, _) = self.start_location(source);
        if line_num > 0 && line_num <= lines.len() {
            lines[line_num - 1]
        } else {
            ""
        }
    }

    pub fn start(&self) -> usize {
        self.start
    }

    pub fn end(&self) -> usize {
        self.end
    }

    /// Format an error with context from the source code
    pub fn format_error<P: AsRef<Path>>(&self, file: P, source: &str, error_msg: &str) -> String {
        let (start_line, start_col) = self.start_location(source);
        let (end_line, end_col) = self.end_location(source);
        let error_line = self.get_line(source);
        let snippet = self.snippet(source);

        // Calculate how many characters to underline
        let underline_len = snippet.chars().count().max(1);

        let line_str = format!("{start_line}");
        let file = file.as_ref().display();

        // Show range if span crosses multiple lines
        let location = if start_line == end_line {
            format!("{start_line}:{start_col}")
        } else {
            format!("{start_line}:{start_col}-{end_line}:{end_col}")
        };

        format!(
            "{} --> {file}:{location}\n{} |\n{line_str} | {error_line}\n{} | {}{}\n{} | {error_msg}\n",
            " ".repeat(line_str.len()),
            " ".repeat(line_str.len()),
            " ".repeat(line_str.len()),
            " ".repeat(start_col - 1),
            "^".repeat(underline_len),
            " ".repeat(line_str.len()),
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Condition {
    Equal,
    NotEqual,
    GreaterEqual,
    Less,
    Not,
    NotZero,
    Negative,
    NotNegative,
    /// The bare `z` keyword. Means `Equal` as a condition, or register Z on Lyminal.
    Zero,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Operation {
    Nop,
    Hlt,
    Add,
    Sub,
    Nor,
    And,
    Xor,
    Rsh,
    Ldi,
    Adi,
    Jmp,
    Brh,
    Cal,
    Ret,
    Lod,
    Str,
    Cmp,
    Mov,
    Lsh,
    Inc,
    Dec,
    Not,
    Neg,
    Or,
    Cpy,
    Adc,
    Mld,
    Mst,
    Pld,
    Pst,
    Inv,
    Cpi,
    Ani,
    Bkl,
    Bkr,
    Skp,
    Clr,
    Lsi,
    Swp,
    Jal,
    Pc,
    Pic,
    Pnc,
    Piz,
    Pnz,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Keyword {
    Operation(Operation),
    Condition(Condition),
    Define,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Register(pub u8);

impl Register {
    pub const R0: Register = Register(0);
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Keyword(Keyword),
    Label(String),
    Identifier(String),
    Number(f64),
    Comma,
    LBracket,
    RBracket,
    Eof,
    Register(Register),
}
