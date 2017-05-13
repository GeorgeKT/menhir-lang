mod compiler;
mod consteval;
mod function;
mod instruction;
mod optimizer;

use std::fmt;
use std::collections::HashMap;

pub use self::instruction::*;
pub use self::function::*;
pub use self::compiler::{compile_to_byte_code};
pub use self::optimizer::{OptimizationLevel, optimize_module};

#[derive(Debug)]
pub struct ByteCodeModule
{
    pub name: String,
    pub functions: HashMap<String, ByteCodeFunction>,
    pub globals: HashMap<String, Constant>,
    pub exit_function: ByteCodeFunction,
}

impl ByteCodeModule
{
    pub fn main_function_name(&self) -> String
    {
        format!("{}::main", self.name)
    }

    pub fn get_function(&self, name: &str) -> Option<&ByteCodeFunction>
    {
        if name == self.exit_function.sig.name {
            Some(&self.exit_function)
        } else {
            self.functions.get(name)
        }
    }
}

impl fmt::Display for ByteCodeModule
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error>
    {
        for (name, value) in &self.globals {
            writeln!(f, "glob {} = {}", name, value)?;
        }

        writeln!(f, " ")?;

        for func in self.functions.values() {
            func.fmt(f)?;
            writeln!(f, " ")?;
        }
        Ok(())
    }
}

#[cfg(test)]
pub mod test
{
    use compileerror::CompileResult;
    use std::io::Cursor;
    use parser::{ParserOptions, parse_module};
    use bytecode::{ByteCodeModule, compile_to_byte_code};
    use typechecker::type_check_module;
    use ast::{TreePrinter, IntSize};
    use target::Target;

    pub fn generate_byte_code(prog: &str, dump: bool) -> CompileResult<ByteCodeModule>
    {
        let target = Target::new(IntSize::I32, "");
        let mut cursor = Cursor::new(prog);
        let parser_options = ParserOptions::default();
        let mut md = parse_module(&parser_options, &mut cursor, "test", "", &target)?;

        if dump {
            println!("Before type check");
            md.print(2);
            println!("-----------------");
        }

        type_check_module(&mut md, &target)?;

        if dump {
            println!("After type check");
            md.print(2);
            println!("-----------------");
        }

        let bc_mod = compile_to_byte_code(&md, &target)?;
        if dump {
            println!("ByteCode:");
            println!("{}", bc_mod);
            println!("-----------------");
        }

        Ok(bc_mod)
    }
}


