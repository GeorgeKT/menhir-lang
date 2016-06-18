
pub struct Import
{
    path: String,
}

pub struct Variable
{
    name: String,
    is_const: bool,
}

pub struct Argument
{
    name: String,
    arg_type: String,
}

pub struct Function
{
    name: String,
    return_type: String,
    args: Vec<Argument>,
}

pub enum Statement
{
    Import(Import),
    VariableDeclaration(Variable),
    FunctionDeclaration(Function),
}

pub struct Program
{
    statements: Vec<Statement>,
}

impl Program
{
    pub fn dump(&self)
    {

    }
}
