mod compiler;
mod function;
mod instruction;
mod interpreter;
#[cfg(test)]
mod tests;

use std::fmt;
pub use self::compiler::*;
pub use self::function::*;
pub use self::instruction::*;
pub use self::interpreter::*;

pub struct ByteCodeModule
{
    pub name: String,
    pub functions: Vec<ByteCodeFunction>,
}

impl fmt::Display for ByteCodeModule
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error>
    {
        for func in &self.functions {
            func.fmt(f)?;
            writeln!(f, " ")?;
        }
        Ok(())
    }
}
