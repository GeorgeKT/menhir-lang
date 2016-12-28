mod compiler;
mod function;
mod instruction;
mod interpreter;

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

/*
fn add_set(func: &mut ByteCodeFunction, expr: ByteCodeExpression, dst: &Var)
{
    func.add(set_instr(dst, expr));
}



fn bind(func: &mut ByteCodeFunction, name: &str, var: &Var)
{
    func.add(bind_instr(name, var))
}





fn add_array_len(func: &mut ByteCodeFunction, array: Var, dst: &Var)
{
    let expr = ByteCodeExpression::Property(array, Property::Len);
    add_set(func, expr, &dst);
}

fn make_array_len(func: &mut ByteCodeFunction, array: Var) -> Var
{
    let var = func.new_var(Type::Int);
    add_array_len(func, array, &var);
    var
}







*/
