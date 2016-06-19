use std::error::Error;
use std::convert::From;
use std::io;
use std::fmt;

#[derive(Debug)]
pub struct CompileError
{
    line: usize,
    offset: usize,
    message: String,
}

impl CompileError
{
    pub fn new(line: usize, offset: usize, msg: &str) -> CompileError
    {
        CompileError{
            line: line,
            offset: offset,
            message: msg.to_owned()
        }
    }
}

impl fmt::Display for CompileError
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error>
    {
        write!(f, "Line {}, offset {}: {}", self.line, self.offset, self.message)
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
            line: 0,
            offset: 0,
            message: format!("IO Error: {}", e),
        }
    }
}
