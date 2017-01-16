mod compiler;
mod debugger;
mod function;
mod instruction;
mod interpreter;
mod value;
mod valueref;
#[cfg(test)]
mod tests;

use std::fmt;
use std::rc::Rc;
use std::collections::HashMap;

use self::function::ByteCodeFunction;

#[derive(Debug, Eq, PartialEq)]
pub struct ExecutionError(pub String);

impl fmt::Display for ExecutionError
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error>
    {
        write!(f, "{}", self.0)
    }
}


pub struct ByteCodeModule
{
    pub name: String,
    pub functions: HashMap<String, Rc<ByteCodeFunction>>,
    pub exit_function: Rc<ByteCodeFunction>,
}

impl fmt::Display for ByteCodeModule
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error>
    {
        for func in self.functions.values() {
            func.fmt(f)?;
            writeln!(f, " ")?;
        }
        Ok(())
    }
}

pub use self::compiler::{compile_to_byte_code, START_CODE_FUNCTION};
pub use self::interpreter::run_byte_code;
pub use self::debugger::debug_byte_code;
