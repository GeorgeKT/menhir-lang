use std::error::Error;
use std::cmp;
use std::convert::From;
use std::io;
use std::fmt;
use ast::Type;

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


impl Default for Pos
{
    fn default() -> Self
    {
        Pos::zero()
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

    pub fn merge(a: &Span, b: &Span) -> Span
    {
        let s = if a.start < b.start {a.start} else {b.start};
        let e = if a.end >= b.end {a.end} else {b.end};
        Span::new(s, e)
    }
}

impl Default for Span
{
    fn default() -> Self
    {
        Span::single(Pos::default())
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ErrorCode
{
    UnexpectedEOF,
    IOError,
    UnexpectedChar,
    UnexpectedToken,
    ExpectedIdentifier,
    ExpectedIntLiteral,
    ExpectedOperator,
    InvalidOperator,
    InvalidUnaryOperator,
    InvalidBinaryOperator,
    InvalidFloatingPoint,
    InvalidInteger,
    TypeError,
    UnknownName,
    MissingType,
    CallingNonCallable,
    CodegenError,
    RedefinitionOfVariable,
    RedefinitionOfFunction,
    RedefinitionOfStruct,
    ExpectedConstExpr,
    GenericTypeSubstitutionError,
    ExpressionNotAllowedAtTopLevel,
    LambdaDoesNotMatch,
    WrongArgumentCount,
    UnknownType(String, Type), // Name and expected type
}


#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CompileError
{
    pub pos: Pos,
    pub error: ErrorCode,
    pub msg: String,
}

impl CompileError
{
    pub fn new(pos: Pos, error: ErrorCode, msg: String) -> CompileError
    {
        CompileError{
            pos: pos,
            error: error,
            msg: msg,
        }
    }
}

pub type CompileResult<T> = Result<T, CompileError>;

pub fn err<T: Sized>(pos: Pos, e: ErrorCode, msg: String) -> CompileResult<T>
{
    Err(CompileError::new(pos, e, msg))
}

pub fn unknown_name(pos: Pos, name: &str) -> CompileError
{
    CompileError::new(pos, ErrorCode::UnknownName, format!("Unable to resolve name {}", name))
}

impl fmt::Display for CompileError
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error>
    {
        write!(f, "{}: {}", self.pos, self.msg)
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
            error: ErrorCode::IOError,
            msg: format!("IO Error: {}", e),
        }
    }
}
