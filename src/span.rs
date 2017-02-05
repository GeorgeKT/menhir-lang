use std::cmp;
use std::fmt;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct Pos
{
    pub line: usize,
    pub offset: usize,
}

impl Pos
{
    pub fn new(line: usize, offset: usize) -> Pos
    {
        Pos {
            line: line,
            offset: offset,
        }
    }

    pub fn zero() -> Pos
    {
        Pos::new(0, 0)
    }
}

impl fmt::Display for Pos
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error>
    {
        write!(f, "{}:{}", self.line, self.offset)
    }
}

impl cmp::PartialOrd for Pos
{
    fn partial_cmp(&self, other: &Pos) -> Option<cmp::Ordering>
    {
        Some(self.cmp(other))
    }
}

impl cmp::Ord for Pos
{
    fn cmp(&self, other: &Self) -> cmp::Ordering
    {
        let o = self.line.cmp(&other.line);
        if o == cmp::Ordering::Equal {
            self.offset.cmp(&other.offset)
        } else {
            o
        }
    }
}


impl Default for Pos
{
    fn default() -> Self
    {
        Pos::zero()
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct Span
{
    pub file: String,
    pub start: Pos,
    pub end: Pos,
}

impl Span
{
    pub fn new(file: &str, start: Pos, end: Pos) -> Span
    {
        Span{
            file: file.into(),
            start: start,
            end: end
        }
    }

    pub fn single(file: &str, start: Pos) -> Span
    {
        Span{
            file: file.into(),
            start: start,
            end: start
        }
    }

    pub fn merge(a: &Span, b: &Span) -> Span
    {
        let s = if a.start < b.start {a.start} else {b.start};
        let e = if a.end >= b.end {a.end} else {b.end};
        Span::new(&a.file, s, e)
    }

    pub fn expanded(&self, new_end: Pos) -> Span
    {
        Span::new(&self.file, self.start, new_end)
    }
}

impl Default for Span
{
    fn default() -> Self
    {
        Span::single("", Pos::default())
    }
}

impl fmt::Display for Span
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error>
    {
        write!(f, "{}:{} -> {}", self.file, self.start, self.end)
    }
}
