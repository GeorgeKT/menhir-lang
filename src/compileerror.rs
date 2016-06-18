use std::error::Error;
use std::fmt;

#[derive(Debug)]
pub struct CompileError
{
    line: u32,
    offset: u32,
    message: String,
}

impl CompileError
{
    pub fn new(line: u32, offset: u32, msg: &str) -> CompileError
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
