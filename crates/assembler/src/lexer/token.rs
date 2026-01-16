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

    /// Calculate line and column from source text
    pub fn location(&self, source: &str) -> (usize, usize) {
        let mut line = 1;
        let mut col = 1;

        for (i, ch) in source.char_indices() {
            if i >= self.start {
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

    /// Get the text snippet from source
    pub fn snippet<'a>(&self, source: &'a str) -> &'a str {
        &source[self.start..self.end.min(source.len())]
    }

    /// Get the line containing this span
    pub fn get_line<'a>(&self, source: &'a str) -> &'a str {
        let lines: Vec<&str> = source.lines().collect();
        let (line_num, _) = self.location(source);

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
        let (line, col) = self.location(source);
        let error_line = self.get_line(source);
        let snippet = self.snippet(source);

        // Calculate how many characters to underline
        let underline_len = snippet.chars().count().max(1);

        let line = format!("{line}");

        let file = file.as_ref().display();

        format!(
            "{} --> {file}:{line}:{col}\n{} |\n{line} | {error_line}\n{} | {}{}\n{} | {error_msg}\n",
            " ".repeat(line.len()),
            " ".repeat(line.len()),
            " ".repeat(line.len()),
            " ".repeat(col - 1),
            "^".repeat(underline_len),
            " ".repeat(line.len()),
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
    Eof,
    Register(Register),
}
