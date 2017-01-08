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

pub use self::compiler::*;
pub use self::debugger::*;
pub use self::function::*;
pub use self::instruction::*;
pub use self::interpreter::*;
pub use self::value::*;
pub use self::valueref::*;

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
