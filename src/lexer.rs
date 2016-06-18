use std::io::Read;

use ast::Program;
use compileerror::CompileError;



pub fn lex<Input: Read>(input: &mut Input) -> Result<Program, CompileError>
{
    Err(CompileError::new(1, 1, "Not yet implemented"))
}
