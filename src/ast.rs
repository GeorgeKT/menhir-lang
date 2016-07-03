
use compileerror::Pos;
use tokens::Operator;

#[derive(Debug, Eq, PartialEq)]
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

#[derive(Debug, Eq, PartialEq)]
pub struct Call
{
    pub name: String,
    pub args: Vec<Expression>,
}

impl Call
{
    pub fn new(name: String, args: Vec<Expression>) -> Call
    {
        Call{
            name: name,
            args: args,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Expression
{
    Number(String),
    StringLiteral(String),
    UnaryOp((Operator, Box<Expression>)),
    BinaryOp((Operator, Box<Expression>, Box<Expression>)),
    Call(Call),
    NameRef(String),
}

#[derive(Debug, Eq, PartialEq)]
pub struct Variable
{
    pub name: String,
    pub typ: Option<Type>,
    pub is_const: bool,
    pub public: bool,
    pub init: Expression,
}

impl Variable
{
    pub fn new(name: String, typ: Option<Type>, is_const: bool, public: bool, init: Expression) -> Variable
    {
        Variable{
            name: name,
            typ: typ,
            is_const: is_const,
            public: public,
            init: init,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Type
{
    Void,
    Primitive(String),
    Struct(String),
    Union(String),
}

#[derive(Debug, Eq, PartialEq)]
pub struct Argument
{
    pub name: String,
    pub typ: Type,
    pub constant: bool,
}

impl Argument
{
    pub fn new(name: String, typ: Type, constant: bool) -> Argument
    {
        Argument{
            name: name,
            typ: typ,
            constant: constant,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Function
{
    pub name: String,
    pub return_type: Type,
    pub args: Vec<Argument>,
    pub public: bool,
    pub block: Block,
}

impl Function
{
    pub fn new(name: String, ret: Type, args: Vec<Argument>, public: bool, block: Block) -> Function
    {
        Function{
            name: name,
            return_type: ret,
            args: args,
            public: public,
            block: block,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct While
{
    pub cond: Expression,
    pub block: Block,
}

impl While
{
    pub fn new(cond: Expression, block: Block) -> While
    {
        While{
            cond: cond,
            block: block,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum ElsePart
{
    Empty,
    Block(Block),
    If(Box<If>),
}

#[derive(Debug, Eq, PartialEq)]
pub struct If
{
    pub cond: Expression,
    pub if_block: Block,
    pub else_part: ElsePart,
}

impl If
{
    pub fn new(cond: Expression, if_block: Block, ep: ElsePart) -> If
    {
        If{
            cond: cond,
            if_block: if_block,
            else_part: ep,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Return
{
    pub expr: Expression,
}

impl Return
{
    pub fn new(expr: Expression) -> Return
    {
        Return{
            expr: expr,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Struct
{
    pub name: String,
    pub variables: Vec<Variable>,
    pub functions: Vec<Function>,
    pub public: bool,
}

impl Struct
{
    pub fn new(name: String, public: bool) -> Struct
    {
        Struct{
            name: name,
            variables: Vec::new(),
            functions: Vec::new(),
            public: public,
        }
    }
}


#[derive(Debug, Eq, PartialEq)]
pub struct UnionCase
{
    pub name: String,
    pub vars: Vec<Argument>,
}

impl UnionCase
{
    pub fn new(name: String) -> UnionCase
    {
        UnionCase{
            name: name,
            vars: Vec::new(),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Union
{
    pub name: String,
    pub public: bool,
    pub cases: Vec<UnionCase>,
    pub functions: Vec<Function>,
}

impl Union
{
    pub fn new(name: String, public: bool) -> Union
    {
        Union{
            name: name,
            public: public,
            cases: Vec::new(),
            functions: Vec::new(),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct MatchCase
{
    pub name: String,
    pub bindings: Vec<String>,
    pub block: Block,
}

impl MatchCase
{
    pub fn new(name: String, bindings: Vec<String>, block: Block) -> MatchCase
    {
        MatchCase{
            name: name,
            bindings: bindings,
            block: block,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Match
{
    pub expr: Expression,
    pub cases: Vec<MatchCase>,
}

impl Match
{
    pub fn new(expr: Expression) -> Match
    {
        Match{
            expr: expr,
            cases: Vec::new(),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Statement
{
    Import(Import),
    Variable(Vec<Variable>),
    Function(Function),
    While(While),
    If(If),
    Return(Return),
    Call(Call),
    Struct(Struct),
    Union(Union),
    Match(Match),
}

#[derive(Debug, Eq, PartialEq)]
pub struct Block
{
    pub statements: Vec<Statement>,
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

#[derive(Debug, Eq, PartialEq)]
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
