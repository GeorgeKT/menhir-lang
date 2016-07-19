mod statements;
mod expressions;
mod lexer;
mod tokens;
mod tokenqueue;
mod tests;

use std::io::Read;
use std::fs;
use std::path::Path;
use std::ffi::OsStr;

use ast::{Module};
use compileerror::{CompileResult};
use parser::statements::{parse_block};

pub use self::lexer::{Lexer};
pub use self::tokenqueue::{TokenQueue};
pub use self::expressions::{parse_expression};
pub use self::tokens::{Operator, Token, TokenKind};

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
pub enum ParseMode
{
    Block,      // allow every statement
    Module,     // only allow toplevel statements (funcs, vars, imports, structs ...)
}


pub fn parse_module<Input: Read>(input: &mut Input, name: &str, mode: ParseMode) -> CompileResult<Module>
{
    let mut tq = try!(Lexer::new().read(input));
    let block = try!(parse_block(&mut tq, 0, mode));
    Ok(Module::new(name, block))
}

pub fn parse_file(file_path: &str, mode: ParseMode) -> CompileResult<Module>
{
    let mut file = try!(fs::File::open(file_path));
    let path = Path::new(file_path);
    let module_name: &OsStr = path.file_stem().expect("Invalid filename");
    parse_module(&mut file, module_name.to_str().expect("Invalid UTF8 filename"), mode)
}
