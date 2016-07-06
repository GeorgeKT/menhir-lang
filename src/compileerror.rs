use std::error::Error;
use std::cmp;
use std::convert::From;
use std::io;
use std::fmt;
use tokens::{Operator, Token};

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
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

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct Span
{
    pub start: Pos,
    pub end: Pos,
}

impl Span
{
    pub fn new(start: Pos, end: Pos) -> Span
    {
        Span{start: start, end: end}
    }

    pub fn single(start: Pos) -> Span
    {
        Span{start: start, end: start}
    }

    pub fn zero() -> Span
    {
        Span::new(Pos::zero(), Pos::zero())
    }

    pub fn merge(a: &Span, b: &Span) -> Span
    {
        let s = if a.start < b.start {a.start} else {b.start};
        let e = if a.end >= b.end {a.end} else {b.end};
        Span::new(s, e)
    }
}

impl fmt::Display for Span
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error>
    {
        write!(f, "{} -> {}", self.start, self.end)
    }
}

#[cfg(test)]
pub fn span(start_line: usize, start_offset: usize, end_line: usize, end_offset: usize) -> Span
{
    Span::new(Pos::new(start_line, start_offset), Pos::new(end_line, end_offset))
}


#[derive(Debug)]
pub enum ErrorType
{
    UnexpectedEOF,
    IOError(io::Error),
    UnexpectedChar(char),
    UnexpectedToken(Token),
    ExpectedIndent,
    ExpectedIdentifier,
    ExpectedStringLiteral,
    ExpectedOperator,
    InvalidOperator(String),
    InvalidUnaryOperator(Operator),
    InvalidBinaryOperator(Operator),
    InvalidFloatingPoint,
    InvalidInteger,
    TypeError(String),
    SelfNotAllowed,
    RedefinitionOfVariable(String),
    UnknownVariable(String),
}


#[derive(Debug)]
pub struct CompileError
{
    pos: Pos,
    error: ErrorType,
}

impl CompileError
{
    pub fn new(pos: Pos, error: ErrorType) -> CompileError
    {
        CompileError{
            pos: pos,
            error: error,
        }
    }
}

pub fn err<T: Sized>(pos: Pos, e: ErrorType) -> Result<T, CompileError>
{
    Err(CompileError::new(pos, e))
}

impl fmt::Display for CompileError
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error>
    {
        match self.error
        {
            ErrorType::UnexpectedEOF => write!(f, "{}: Unexpected end of file", self.pos),
            ErrorType::IOError(ref e) =>  write!(f, "{}: {}", self.pos, e.description()),
            ErrorType::UnexpectedChar(c) =>  write!(f, "{}: Unexpected character {}", self.pos, c),
            ErrorType::UnexpectedToken(ref tok) =>  write!(f, "{}: Unexpected token {:?}", self.pos, tok),
            ErrorType::InvalidOperator(ref s) =>  write!(f, "{}: Invalid operator {}", self.pos, s),
            ErrorType::InvalidUnaryOperator(op) => write!(f, "{}: Invalid operator {}", self.pos, op),
            ErrorType::InvalidBinaryOperator(op) =>  write!(f, "{}: Invalid operator {}", self.pos, op),
            ErrorType::ExpectedIdentifier =>  write!(f, "{}: Expected identifier", self.pos),
            ErrorType::ExpectedIndent =>  write!(f, "{}: Expected indentation", self.pos),
            ErrorType::ExpectedStringLiteral =>  write!(f, "{}: Expected string literal", self.pos),
            ErrorType::ExpectedOperator => write!(f, "{}: Expected operator", self.pos),
            ErrorType::SelfNotAllowed => write!(f, "{}: A self argument is only allowed as the first argument of a member function", self.pos),
            ErrorType::InvalidFloatingPoint => write!(f, "{}: Invalid floating point number", self.pos),
            ErrorType::InvalidInteger => write!(f, "{}: Invalid integer", self.pos),
            ErrorType::TypeError(ref s) => write!(f, "{}: Wrong type: {}", self.pos, s),
            ErrorType::RedefinitionOfVariable(ref v) =>  write!(f, "{}: Attempting to redefine variable {}", self.pos, v),
            ErrorType::UnknownVariable(ref n) => write!(f, "{}: Unknown variable '{}'", self.pos, n),
        }
    }
}

impl Error for CompileError
{
    fn description(&self) -> &str
    {
        "Compile error"

    }
}

impl From<io::Error> for CompileError
{
    fn from(e: io::Error) -> Self
    {
        CompileError{
            pos: Pos::new(0, 0),
            error: ErrorType::IOError(e),
        }
    }
}
