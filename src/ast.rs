use std::fmt;
use compileerror::{Span, Pos};
use parser::Operator;

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
    span: Span,
}

impl Import
{
    pub fn new(path: String, span: Span) -> Import
    {
        Import{
            path: path,
            span: span,
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
    pub span: Span,
}

impl Call
{
    pub fn new(name: String, args: Vec<Expression>, span: Span) -> Call
    {
        Call{
            name: name,
            args: args,
            span: span,
        }
    }
}

impl TreePrinter for Call
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        println!("{}call {} {}", p, self.name, self.span);
        for a in &self.args {
            a.print(level + 1);
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Expression
{
    Number(Span, String),
    StringLiteral(Span, String),
    UnaryOp(Span, Operator, Box<Expression>),
    PostFixUnaryOp(Span, Operator, Box<Expression>), // post increment and decrement
    BinaryOp(Span, Operator, Box<Expression>, Box<Expression>),
    Enclosed(Span, Box<Expression>), // Expression enclosed between parens
    Call(Call),
    NameRef(Span, String),
    Assignment(Span, Operator, String, Box<Expression>),
}

impl Expression
{
    pub fn precedence(&self) -> usize
    {
        match *self
        {
            Expression::BinaryOp(_, op, _, _) => op.precedence(),
            _ => 0,
        }
    }

    pub fn span(&self) -> Span
    {
        match *self
        {
            Expression::Number(span, _) => span,
            Expression::StringLiteral(span, _) => span,
            Expression::UnaryOp(span, _, _) => span,
            Expression::PostFixUnaryOp(span, _, _) => span,
            Expression::BinaryOp(span, _, _, _) => span,
            Expression::Enclosed(span, _) => span,
            Expression::Call(ref c) => c.span,
            Expression::NameRef(span, _) => span,
            Expression::Assignment(span, _, _, _) => span,
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
            Expression::Number(ref span, ref s) => {
                println!("{}number {} ({})", p, s, span);
            },
            Expression::StringLiteral(ref span, ref s) => {
                println!("{}string \"{}\" ({})", p, s, span);
            },
            Expression::UnaryOp(ref span, ref op, ref e) => {
                println!("{}unary {} ({})", p, op, span);
                e.print(level + 1)
            },
            Expression::PostFixUnaryOp(ref span, ref op, ref e) => {
                println!("{}postfix unary {} ({})", p, op, span);
                e.print(level + 1)
            },
            Expression::BinaryOp(ref span, ref op, ref left, ref right) => {
                println!("{}binary {} ({})", p, op, span);
                left.print(level + 1);
                right.print(level + 1)
            },
            Expression::Enclosed(ref span, ref e) => {
                println!("{}enclosed ({})", p, span);
                e.print(level + 1);
            },
            Expression::Call(ref c) => c.print(level),
            Expression::NameRef(ref span, ref s) => {
                println!("{}name {} ({})", p, s, span);
            },
            Expression::Assignment(ref span, ref op, ref target, ref e) => {
                println!("{}{} {} ({})", p, target, op, span);
                e.print(level + 1);
            },
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Variable
{
    pub name: String,
    pub typ: Type,
    pub is_const: bool,
    pub public: bool,
    pub init: Expression,
    pub span: Span,
}

impl Variable
{
    pub fn new(name: String, typ: Type, is_const: bool, public: bool, init: Expression, span: Span) -> Variable
    {
        Variable{
            name: name,
            typ: typ,
            is_const: is_const,
            public: public,
            init: init,
            span: span,
        }
    }
}

impl TreePrinter for Variable
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        println!("{}var {}: {:?} (public: {}, constant: {}, span: {}) =",
            p, self.name, self.typ, self.public, self.is_const, self.span);
        self.init.print(level + 1);
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Type
{
    Void,
    Unknown,
    Primitive(Pos, String),
    Struct(Pos, String),
    Union(Pos, String),
    Pointer(Pos, Box<Type>),
}

impl fmt::Display for Type
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error>
    {
        match *self
        {
            Type::Void => write!(f, "void"),
            Type::Unknown => write!(f, "unknown"),
            Type::Primitive(_, ref t) => write!(f, "primitive {}", t),
            Type::Struct(_, ref s) => write!(f, "struct {}", s),
            Type::Union(_, ref u) => write!(f, "union {}", u),
            Type::Pointer(_, ref st) => write!(f, "pointer to {}", st),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Argument
{
    pub name: String,
    pub typ: Type,
    pub constant: bool,
    pub span: Span,
}

impl Argument
{
    pub fn new(name: String, typ: Type, constant: bool, span: Span) -> Argument
    {
        Argument{
            name: name,
            typ: typ,
            constant: constant,
            span: span,
        }
    }
}

impl TreePrinter for Argument
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        println!("{}{}: {} (constant: {}, span; {})", p, self.name, self.typ, self.constant, self.span);
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct FunctionSignature
{
    pub name: String,
    pub return_type: Type,
    pub args: Vec<Argument>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Function
{
    pub sig: FunctionSignature,
    pub public: bool,
    pub block: Block,
    pub span: Span,
}

impl Function
{
    pub fn new(sig: FunctionSignature, public: bool, block: Block, span: Span) -> Function
    {
        Function{
            sig: sig,
            public: public,
            block: block,
            span: span,
        }
    }
}

impl TreePrinter for Function
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        println!("{}function {} (public: {}, span: {})", p, self.sig.name, self.public, self.span);
        println!("{} return_type: {}", p, self.sig.return_type);
        println!("{} args:", p);
        for a in &self.sig.args {
            a.print(level + 2);
        }
        self.block.print(level + 1)
    }
}


#[derive(Debug, Eq, PartialEq)]
pub struct ExternalFunction
{
    pub sig: FunctionSignature,
    pub span: Span,
}

impl ExternalFunction
{
    pub fn new(sig: FunctionSignature, span: Span) -> ExternalFunction
    {
        ExternalFunction{
            sig: sig,
            span: span,
        }
    }
}

impl TreePrinter for ExternalFunction
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        println!("{}external function {} (span: {})", p, self.sig.name, self.span);
        println!("{} return_type: {}", p, self.sig.return_type);
        println!("{} args:", p);
        for a in &self.sig.args {
            a.print(level + 2);
        }
    }
}


#[derive(Debug, Eq, PartialEq)]
pub struct While
{
    pub cond: Expression,
    pub block: Block,
    pub span: Span,
}

impl While
{
    pub fn new(cond: Expression, block: Block, span: Span) -> While
    {
        While{
            cond: cond,
            block: block,
            span: span,
        }
    }
}

impl TreePrinter for While
{
    fn print(&self, level: usize)
    {
        println!("{}while {}", prefix(level), self.span);
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
    pub span: Span,
}

impl If
{
    pub fn new(cond: Expression, if_block: Block, ep: ElsePart, span: Span) -> If
    {
        If{
            cond: cond,
            if_block: if_block,
            else_part: ep,
            span: span,
        }
    }
}

impl TreePrinter for If
{
    fn print(&self, level: usize)
    {
        println!("{}if {}", prefix(level), self.span);
        self.cond.print(level + 1);
        self.if_block.print(level + 1);
        self.else_part.print(level + 1);
    }
}


#[derive(Debug, Eq, PartialEq)]
pub struct Return
{
    pub expr: Expression,
    pub span: Span,
}

impl Return
{
    pub fn new(expr: Expression, span: Span) -> Return
    {
        Return{
            expr: expr,
            span: span,
        }
    }
}

impl TreePrinter for Return
{
    fn print(&self, level: usize)
    {
        println!("{}return {}", prefix(level), self.span);
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
    pub span: Span,
}

impl Struct
{
    pub fn new(name: String, public: bool, span: Span) -> Struct
    {
        Struct{
            name: name,
            variables: Vec::new(),
            functions: Vec::new(),
            public: public,
            span: span,
        }
    }
}

impl TreePrinter for Struct
{
    fn print(&self, level: usize)
    {
        println!("{}struct {} (public: {}, span: {})", prefix(level), self.name, self.public, self.span);
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
    pub span: Span,
}

impl UnionCase
{
    pub fn new(name: String, span: Span) -> UnionCase
    {
        UnionCase{
            name: name,
            vars: Vec::new(),
            span: span,
        }
    }
}

impl TreePrinter for UnionCase
{
    fn print(&self, level: usize)
    {
        println!("{}case {} (span: {})", prefix(level), self.name, self.span);
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
    pub span: Span,
}

impl Union
{
    pub fn new(name: String, public: bool, span: Span) -> Union
    {
        Union{
            name: name,
            public: public,
            cases: Vec::new(),
            functions: Vec::new(),
            span: span,
        }
    }
}

impl TreePrinter for Union
{
    fn print(&self, level: usize)
    {
        println!("{}union {} (public: {}, span: {})", prefix(level), self.name, self.public, self.span);
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
    pub span: Span,
}

impl MatchCase
{
    pub fn new(name: String, bindings: Vec<String>, block: Block, span: Span) -> MatchCase
    {
        MatchCase{
            name: name,
            bindings: bindings,
            block: block,
            span: span,
        }
    }
}

impl TreePrinter for MatchCase
{
    fn print(&self, level: usize)
    {
        println!("{}case {} {:?} (span: {})", prefix(level), self.name, self.bindings, self.span);
        self.block.print(level)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Match
{
    pub expr: Expression,
    pub cases: Vec<MatchCase>,
    pub span: Span,
}

impl Match
{
    pub fn new(expr: Expression, span: Span) -> Match
    {
        Match{
            expr: expr,
            cases: Vec::new(),
            span: span,
        }
    }
}

impl TreePrinter for Match
{
    fn print(&self, level: usize)
    {
        println!("{}match (span: {})", prefix(level), self.span);
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
    ExternalFunction(ExternalFunction),
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
            Statement::ExternalFunction(ref fun) => fun.print(level),
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
    pub name: String,
    pub block: Block,
}

impl Program
{
    pub fn new(name: &str, block: Block) -> Program
    {
        Program{
            name: name.into(),
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
