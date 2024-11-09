use serde_derive::{Deserialize, Serialize};
use std::cmp;
use std::fmt;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct Pos {
    pub line: usize,
    pub offset: usize,
}

impl Pos {
    pub fn new(line: usize, offset: usize) -> Pos {
        Pos { line, offset }
    }

    pub fn zero() -> Pos {
        Pos::new(0, 0)
    }
}

impl fmt::Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}:{}", self.line, self.offset)
    }
}

impl cmp::PartialOrd for Pos {
    fn partial_cmp(&self, other: &Pos) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl cmp::Ord for Pos {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        let o = self.line.cmp(&other.line);
        if o == cmp::Ordering::Equal {
            self.offset.cmp(&other.offset)
        } else {
            o
        }
    }
}

impl Default for Pos {
    fn default() -> Self {
        Pos::zero()
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Serialize, Deserialize)]
#[derive(Default)]
pub enum Span {
    File { file: String, start: Pos, end: Pos },
    #[default]
    Internal,
}

impl Span {
    pub fn new(file: &str, start: Pos, end: Pos) -> Span {
        Span::File {
            file: file.into(),
            start,
            end,
        }
    }

    pub fn single(file: &str, start: Pos) -> Span {
        Span::File {
            file: file.into(),
            start,
            end: start,
        }
    }

    pub fn end(&self) -> Pos {
        if let Span::File { end, .. } = self {
            *end
        } else {
            Pos::default()
        }
    }

    pub fn merge(a: &Span, b: &Span) -> Span {
        match (a, b) {
            (
                Span::File { file, start, end },
                Span::File {
                    start: b_start,
                    end: b_end,
                    ..
                },
            ) => {
                let s = if start < b_start { start } else { b_start };
                let e = if end >= b_end { end } else { b_end };
                Span::new(file, *s, *e)
            }
            _ => Span::Internal,
        }
    }

    pub fn expanded(&self, new_end: Pos) -> Span {
        match self {
            Span::File { file, start, .. } => Span::new(file, *start, new_end),
            Span::Internal => Span::Internal,
        }
    }
}


impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            Span::File { file, start, end } => write!(f, "{}:{} -> {}", file, start, end),
            Span::Internal => write!(f, "internal"),
        }
    }
}
