use arbitrary_int::{i4, u4, u10};

pub type Register = u4;

#[derive(Debug, Clone, PartialEq)]
pub enum OffsetOrSymbol {
    Offset(i4),
    Symbol(String),
    NotPresent,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ImmediateOrSymbol {
    Immediate(i8),
    Symbol(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum AddressOrSymbol {
    Address(u10),
    Symbol(String),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Condition {
    Equal,
    NotEqual,
    GreaterEqual,
    Less,
}

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
    pub fn format_error(&self, source: &str, error_msg: &str) -> String {
        let (line, col) = self.location(source);
        let error_line = self.get_line(source);
        let snippet = self.snippet(source);

        // Calculate how many characters to underline
        let underline_len = snippet.chars().count().max(1);

        format!(
            "Error at line {}, column {}: {}\n  |\n{} | {}\n  | {}{}\n",
            line,
            col,
            error_msg,
            line,
            error_line,
            " ".repeat(col - 1),
            "^".repeat(underline_len)
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // Operations
    Nop,
    Hlt,
    Add(Register, Register, Register),
    Sub(Register, Register, Register),
    Nor(Register, Register, Register),
    And(Register, Register, Register),
    Xor(Register, Register, Register),
    Rsh(Register, Register),
    Ldi(Register, ImmediateOrSymbol),
    Adi(Register, ImmediateOrSymbol),
    Jmp(AddressOrSymbol),
    Brh(Condition, AddressOrSymbol),
    Cal(AddressOrSymbol),
    Ret,
    Lod(Register, Register, OffsetOrSymbol),
    Str(Register, Register, OffsetOrSymbol),

    // Psudeo Operations
    Cmp(Register, Register), // CMP A B -> SUB A B r0
    Mov(Register, Register), // MOV A C -> ADD A r0 C
    Lsh(Register, Register), // LSH A C -> ADD A A C
    Inc(Register),           // INC A -> ADI A 1
    Dec(Register),           // DEC A -> ADI A -1
    Not(Register, Register), // NOT A C -> NOR A r0 C
    Neg(Register, Register), // NEG A C -> SUB r0 A C

    Label(String),
    Define(String, f32),

    Eof,
}
