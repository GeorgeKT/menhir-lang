use std::error::Error;
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
}

impl fmt::Display for Pos
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error>
    {
        write!(f, "{}:{}", self.line, self.offset)
    }
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
    InvalidOperator(String),
    InvalidUnaryOperator(Operator),
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
            ErrorType::InvalidUnaryOperator(op) =>  write!(f, "{}: Invalid operator {}", self.pos, op),
            ErrorType::ExpectedIdentifier =>  write!(f, "{}: Expected identifier", self.pos),
            ErrorType::ExpectedIndent =>  write!(f, "{}: Expected indentation", self.pos),
            ErrorType::ExpectedStringLiteral =>  write!(f, "{}: Expected string literal", self.pos),
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
