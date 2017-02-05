mod compiler;
mod function;
mod instruction;
mod optimizer;

use std::io;
use std::fmt;
use std::collections::HashMap;
use std::io::Cursor;

use typechecker::{type_check_module};
use ast::{TreePrinter};
use parser::*;
use compileerror::CompileResult;

pub use self::instruction::*;
pub use self::function::*;
pub use self::compiler::{compile_to_byte_code, START_CODE_FUNCTION};
pub use self::optimizer::{OptimizationLevel, optimize_module};


#[derive(Serialize, Deserialize, Debug)]
pub struct ByteCodeModule
{
    pub name: String,
    functions: HashMap<String, ByteCodeFunction>,
    pub exit_function: ByteCodeFunction,
}

impl ByteCodeModule
{
    pub fn get_function(&self, name: &str) -> Option<&ByteCodeFunction>
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
            .map_err(|err| format!("Cannot load bytecode: {}", err))
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

pub fn generate_byte_code(prog: &str, dump: bool) -> CompileResult<ByteCodeModule>
{


    let mut cursor = Cursor::new(prog);
    let parser_options = ParserOptions::default();
    let mut md = parse_module(&parser_options, &mut cursor, "test", "")?;

    if dump {
        println!("Before type check");
        md.print(2);
        println!("-----------------");
    }

    type_check_module(&mut md)?;

    if dump {
        println!("After type check");
        md.print(2);
        println!("-----------------");
    }

    let bc_mod = compile_to_byte_code(&md);
    if dump {
        println!("ByteCode:");
        println!("{}", bc_mod);
        println!("-----------------");
    }

    Ok(bc_mod)
}

