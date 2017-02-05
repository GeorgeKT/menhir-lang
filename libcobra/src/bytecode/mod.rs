mod compiler;
mod debugger;
mod function;
mod instruction;
mod interpreter;
mod optimizer;
mod value;
mod valueref;
#[cfg(test)]
mod tests;

use std::io;
use std::fmt;
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


#[derive(Serialize, Deserialize, Debug)]
pub struct ByteCodeModule
{
    pub name: String,
    functions: HashMap<String, ByteCodeFunction>,
    exit_function: ByteCodeFunction,
}

impl ByteCodeModule
{
    fn get_function(&self, name: &str) -> Option<&ByteCodeFunction>
    {
        if name == self.exit_function.sig.name {
            Some(&self.exit_function)
        } else {
            self.functions.get(name)
        }
    }

    pub fn save<Output: io::Write>(&self, output: &mut Output) -> Result<(), String>
    {
        use serde_cbor::ser;
        ser::to_writer(output, self)
            .map_err(|err| format!("{}", err))
    }

    pub fn load<Input: io::Read>(input: &mut Input) -> Result<ByteCodeModule, String>
    {
        use serde_cbor::de;
        de::from_reader(input)
            .map_err(|err| format!("Cannot import bytecode: {}", err))
    }
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
pub use self::optimizer::{OptimizationLevel, optimize_module};
