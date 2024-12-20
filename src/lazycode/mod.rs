use std::{collections::BTreeMap, fmt};

use crate::ast::External;

pub use self::compiler::compile_to_byte_code;
pub use self::function::ByteCodeFunction;
pub use self::instruction::Instruction;
pub use self::operand::{ByteCodeProperty, CallArg, Constant, Operand};
pub use self::optimizer::*;
pub use self::patterns::OPTIONAL_DATA_IDX;
pub use self::scope::{Scope, ScopeNode};
use self::stack::StackPtr;

mod compiler;
mod consteval;
mod function;
mod instruction;
mod operand;
mod optimizer;
mod patterns;
mod scope;
mod stack;

#[derive(Debug)]
pub struct Global {
    pub value: Constant,
    pub thread_local: bool,
}

#[derive(Debug)]
pub struct ByteCodeModule {
    pub name: String,
    pub functions: BTreeMap<String, ByteCodeFunction>,
    pub imported_functions: Vec<ByteCodeFunction>,
    pub external_vars: BTreeMap<String, External>,
    pub globals: BTreeMap<String, Global>,
    pub stack: StackPtr,
}

impl ByteCodeModule {
    pub fn main_function_name(&self) -> String {
        format!("{}::main", self.name)
    }

    pub fn get_function(&self, name: &str) -> Option<&ByteCodeFunction> {
        self.functions.get(name)
    }
}

impl fmt::Display for ByteCodeModule {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        for (name, global) in &self.globals {
            if global.thread_local {
                writeln!(f, "thread_local glob {} = {}", name, global.value)?;
            } else {
                writeln!(f, "glob {} = {}", name, global.value)?;
            }
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
pub mod test {
    use crate::ast::{IntSize, TreePrinter};
    use crate::compileerror::CompileResult;
    use crate::lazycode::{compile_to_byte_code, ByteCodeModule};
    use crate::parser::parse_str;
    use crate::target::Target;

    pub fn generate_byte_code(prog: &str, dump: bool) -> CompileResult<ByteCodeModule> {
        let target = Target::new(IntSize::I32, "");
        let mut pkg = parse_str(prog, "test", &target)?;

        if dump {
            println!("Before type check");
            pkg.print(0);
            println!("-----------------");
        }

        pkg.type_check(&target)?;

        if dump {
            println!("After type check");
            pkg.print(0);
            println!("-----------------");
        }

        let bc_mod = compile_to_byte_code(&pkg, &target)?;
        if dump {
            println!("ByteCode:");
            println!("{}", bc_mod);
            println!("-----------------");
        }

        Ok(bc_mod)
    }
}
