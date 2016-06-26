
use compileerror::Pos;
use tokens::Operator;

#[derive(Debug)]
pub struct Import
{
    path: String,
    pos: Pos,
}

impl Import
{
    pub fn new(path: String, pos: Pos) -> Import
    {
        Import{
            path: path,
            pos: pos,
        }
    }
}

#[derive(Debug)]
pub enum Expression
{
    Number(String),
    StringLiteral(String),
    UnaryOp((Operator, Box<Expression>)),
    BinaryOp((Operator, Box<Expression>, Box<Expression>)),
    Call((String, Vec<Expression>)),
    NameRef(String),
}

#[derive(Debug)]
pub struct Variable
{
    pub name: String,
    pub typ: Option<Type>,
    pub is_const: bool,
    pub init: Expression,
}

impl Variable
{
    pub fn new(name: String, typ: Option<Type>, is_const: bool, init: Expression) -> Variable
    {
        Variable{
            name: name,
            typ: typ,
            is_const: is_const,
            init: init,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Type
{
    Void,
    Primitive(String),
}

#[derive(Debug)]
pub struct Argument
{
    pub name: String,
    pub arg_type: Type,
    pub constant: bool,
}

impl Argument
{
    pub fn new(name: String, typ: Type, constant: bool) -> Argument
    {
        Argument{
            name: name,
            arg_type: typ,
            constant: constant,
        }
    }
}

#[derive(Debug)]
pub struct Function
{
    pub name: String,
    pub return_type: Type,
    pub args: Vec<Argument>,
    pub block: Block,
}

impl Function
{
    pub fn new(name: String, ret: Type, args: Vec<Argument>, block: Block) -> Function
    {
        Function{
            name: name,
            return_type: ret,
            args: args,
            block: block,
        }
    }
}

#[derive(Debug)]
pub enum Statement
{
    Import(Import),
    VariableDeclaration(Vec<Variable>),
    FunctionDeclaration(Function),
}

#[derive(Debug)]
pub struct Block
{
    statements: Vec<Statement>,
}

impl Block
{
    pub fn new(s: Vec<Statement>) -> Block
    {
        Block{
            statements: s,
        }
    }

}

#[derive(Debug)]
pub struct Program
{
    block: Block,
}

impl Program
{
    pub fn new(block: Block) -> Program
    {
        Program{
            block: block,
        }
    }

    #[allow(dead_code)]
    pub fn dump(&self)
    {
        println!("{:?}", self);
    }
}
