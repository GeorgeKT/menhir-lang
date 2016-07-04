use std::fmt;
use compileerror::Pos;
use tokens::Operator;

fn prefix(level: usize) -> String
{
    let mut s = String::with_capacity(level);
    for _ in 0..level {
        s.push(' ')
    }
    s
}

pub trait TreePrinter
{
    fn print(&self, level: usize);
}

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

impl TreePrinter for Import
{
    fn print(&self, level: usize)
    {
        println!("{}import {}", prefix(level), self.path);
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

impl TreePrinter for Call
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        println!("{}call {}", p, self.name);
        for a in &self.args {
            a.print(level + 1);
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Expression
{
    Number(String),
    StringLiteral(String),
    UnaryOp((Operator, Box<Expression>)),
    PostFixUnaryOp((Operator, Box<Expression>)), // post increment and decrement
    BinaryOp((Operator, Box<Expression>, Box<Expression>)),
    Enclosed(Box<Expression>), // Expression enclosed between parens
    Call(Call),
    NameRef(String),
}

impl Expression
{
    pub fn precedence(&self) -> usize
    {
        match *self
        {
            Expression::BinaryOp((op, _, _)) => op.precedence(),
            _ => 0,
        }
    }
}

impl TreePrinter for Expression
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        match *self
        {
            Expression::Number(ref s) => {
                println!("{}number {}", p, s);
            },
            Expression::StringLiteral(ref s) => {
                println!("{}string {}", p, s);
            },
            Expression::UnaryOp((ref op, ref e)) => {
                println!("{}unary {}", p, op);
                e.print(level + 1)
            },
            Expression::PostFixUnaryOp((ref op, ref e)) => {
                println!("{}postfix unary {}", p, op);
                e.print(level + 1)
            },
            Expression::BinaryOp((ref op, ref left, ref right)) => {
                println!("{}binary {}", p, op);
                left.print(level + 1);
                right.print(level + 1)
            },
            Expression::Enclosed(ref e) => {
                println!("{}enclosed", p);
                e.print(level + 1);
            },
            Expression::Call(ref c) => c.print(level),
            Expression::NameRef(ref s) => {
                println!("{}name {}", p, s);
            }
        }
    }
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

impl TreePrinter for Variable
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        println!("{}var {}: {:?} (public: {}, constant: {}) =", p, self.name, self.typ, self.public, self.is_const);
        self.init.print(level + 1);
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

impl fmt::Display for Type
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error>
    {
        match *self
        {
            Type::Void => write!(f, "void"),
            Type::Primitive(ref t) => write!(f, "primitive {}", t),
            Type::Struct(ref s) => write!(f, "struct {}", s),
            Type::Union(ref u) => write!(f, "union {}", u),
        }
    }
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

impl TreePrinter for Argument
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        println!("{}{}: {} (constant {})", p, self.name, self.typ, self.constant);
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

impl TreePrinter for Function
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        println!("{}function {} (public: {})", p, self.name, self.public);
        println!("{} return_type: {}", p, self.return_type);
        println!("{} args:", p);
        for a in &self.args {
            a.print(level + 2);
        }
        self.block.print(level + 1)
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

impl TreePrinter for While
{
    fn print(&self, level: usize)
    {
        println!("{}while", prefix(level));
        self.cond.print(level + 1);
        self.block.print(level + 1);
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum ElsePart
{
    Empty,
    Block(Block),
    If(Box<If>),
}

impl TreePrinter for ElsePart
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        match *self
        {
            ElsePart::Empty => {
                println!("{}no else", p);
            },
            ElsePart::Block(ref b) => {
                println!("{}else", p);
                b.print(level + 1);
            },
            ElsePart::If(ref i) => i.print(level),
        }
    }
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

impl TreePrinter for If
{
    fn print(&self, level: usize)
    {
        println!("{}if", prefix(level));
        self.cond.print(level + 1);
        self.if_block.print(level + 1);
        self.else_part.print(level + 1);
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

impl TreePrinter for Return
{
    fn print(&self, level: usize)
    {
        println!("{}return", prefix(level));
        self.expr.print(level + 1)
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

impl TreePrinter for Struct
{
    fn print(&self, level: usize)
    {
        println!("{}struct {} (public: {})", prefix(level), self.name, self.public);
        for v in &self.variables {
            v.print(level + 1);
        }

        for fun in &self.functions {
            fun.print(level + 1);
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

impl TreePrinter for UnionCase
{
    fn print(&self, level: usize)
    {
        println!("{}case {}", prefix(level), self.name);
        for v in &self.vars {
            v.print(level + 1);
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

impl TreePrinter for Union
{
    fn print(&self, level: usize)
    {
        println!("{}union {} (public: {})", prefix(level), self.name, self.public);
        for c in &self.cases {
            c.print(level + 1);
        }

        for fun in &self.functions {
            fun.print(level + 1);
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

impl TreePrinter for MatchCase
{
    fn print(&self, level: usize)
    {
        println!("{}case {} {:?}", prefix(level), self.name, self.bindings);
        self.block.print(level)
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

impl TreePrinter for Match
{
    fn print(&self, level: usize)
    {
        println!("{}match", prefix(level));
        self.expr.print(level + 1);
        for c in &self.cases {
            c.print(level + 1);
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
    Struct(Struct),
    Union(Union),
    Match(Match),
    Expression(Expression),
}

impl TreePrinter for Statement
{
    fn print(&self, level: usize)
    {
        match *self
        {
            Statement::Import(ref i) => i.print(level),
            Statement::Variable(ref vars) => {
                for v in vars {
                    v.print(level);
                }
            },
            Statement::Function(ref fun) => fun.print(level),
            Statement::While(ref w) => w.print(level),
            Statement::If(ref i) => i.print(level),
            Statement::Return(ref r) => r.print(level),
            Statement::Struct(ref s) => s.print(level),
            Statement::Union(ref u) => u.print(level),
            Statement::Match(ref m) => m.print(level),
            Statement::Expression(ref e) => e.print(level),
        }
    }
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

impl TreePrinter for Block
{
    fn print(&self, level: usize)
    {
        println!("{}block: ", prefix(level));
        for s in &self.statements
        {
            s.print(level + 1);
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
}

impl TreePrinter for Program
{
    fn print(&self, level: usize)
    {
        self.block.print(level + 1)
    }
}
