use std::error::Error;
use std::convert::From;
use std::io;
use std::fmt;

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
pub struct CompileError
{
    pos: Pos,
    message: String,
}

impl CompileError
{
    pub fn new(pos: Pos, msg: String) -> CompileError
    {
        CompileError{
            pos: pos,
            message: msg
        }
    }
}

impl fmt::Display for CompileError
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error>
    {
        write!(f, "{} : {}", self.pos, self.message)
    }
}

impl Error for CompileError
{
    fn description(&self) -> &str
    {
        &self.message
    }
}

impl From<io::Error> for CompileError
{
    fn from(e: io::Error) -> Self
    {
        CompileError{
            pos: Pos::new(0, 0),
            message: format!("IO Error: {}", e),
        }
    }
}
