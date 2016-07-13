use std::fmt;
use compileerror::{Span};
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
pub struct ModuleName
{
    pub parts: Vec<String>,
    pub span: Span,
}

impl ModuleName
{
    pub fn new(parts: Vec<String>, span: Span) -> ModuleName {
        ModuleName{
            parts: parts,
            span: span,
        }
    }
    pub fn to_string(&self) -> String
    {
        self.parts.join("::")
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Import
{
    pub modules: Vec<ModuleName>,
    pub span: Span,
}

impl Import
{
    pub fn new(modules: Vec<ModuleName>, span: Span) -> Import
    {
        Import{
            modules: modules,
            span: span,
        }
    }
}

impl TreePrinter for Import
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        println!("{}import ({})", p, self.span);
        for m in &self.modules {
            println!("{} {} ({})", p, m.to_string(), m.span);
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
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

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct UnaryOp
{
    pub operator: Operator,
    pub expression: Box<Expression>,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct BinaryOp
{
    pub operator: Operator,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Assignment
{
    pub operator: Operator,
    pub target: Box<Expression>,
    pub expression: Box<Expression>,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct NameRef
{
    pub name: String,
    pub span: Span,
}

impl NameRef
{
    pub fn new(name: String, span: Span) -> NameRef
    {
        NameRef{
            name: name,
            span: span,
        }
    }
}

impl TreePrinter for NameRef
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        println!("{}name {} ({})", p, self.name, self.span);
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ObjectConstruction
{
    pub object_type: String,
    pub args: Vec<Expression>,
    pub span: Span,
}


#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Member
{
    Call(Call),
    Nested(Box<MemberAccess>),
    Var(NameRef),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct MemberAccess
{
    pub name: String,
    pub member: Member,
    pub span: Span,
}

impl MemberAccess
{
    pub fn new(name: &str, member: Member, span: Span) -> MemberAccess
    {
        MemberAccess{
            name: name.into(),
            member: member,
            span: span,
        }
    }
}

impl TreePrinter for MemberAccess
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        println!("{}member access {} ({})", p, self.name, self.span);
        match self.member
        {
            Member::Call(ref c) => c.print(level + 1),
            Member::Nested(ref n) => n.print(level + 1),
            Member::Var(ref n) => n.print(level + 1),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ArrayLiteral
{
    pub elements: Vec<Expression>,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Expression
{
    IntLiteral(Span, u64),
    FloatLiteral(Span, String), // Keep as string until we generate code, so we can compare it
    StringLiteral(Span, String),
    ArrayLiteral(ArrayLiteral),
    UnaryOp(UnaryOp),
    PostFixUnaryOp(UnaryOp), // post increment and decrement
    BinaryOp(BinaryOp),
    Enclosed(Span, Box<Expression>), // Expression enclosed between parens
    Call(Call),
    NameRef(NameRef),
    Assignment(Assignment),
    ObjectConstruction(ObjectConstruction),
    MemberAccess(MemberAccess),
}

pub fn array_lit(e: Vec<Expression>, span: Span) -> Expression
{
    Expression::ArrayLiteral(ArrayLiteral{
        elements: e,
        span: span,
    })
}

pub fn bin_op(op: Operator, left: Expression, right: Expression, span: Span) -> Expression
{
    Expression::BinaryOp(BinaryOp{
        operator: op,
        left: Box::new(left),
        right: Box::new(right),
        span: span,
    })
}

pub fn bin_op2(op: Operator, left: Expression, right: Box<Expression>, span: Span) -> Expression
{
    Expression::BinaryOp(BinaryOp{
        operator: op,
        left: Box::new(left),
        right: right,
        span: span,
    })
}


pub fn assignment(op: Operator, target: Expression, expression: Expression, span: Span) -> Expression
{
    Expression::Assignment(Assignment{
        operator: op,
        target: Box::new(target),
        expression: Box::new(expression),
        span: span,
    })
}

#[cfg(test)]
pub fn name_ref(name: &str, span: Span) -> Expression
{
    Expression::NameRef(NameRef{
        name: name.into(),
        span: span,
    })
}

pub fn object_construction(object_type: String, args: Vec<Expression>, span: Span) -> Expression
{
    Expression::ObjectConstruction(ObjectConstruction{
        object_type: object_type,
        args: args,
        span: span,
    })
}

pub fn unary_op(operator: Operator, expression: Expression, span: Span) -> Expression
{
    Expression::UnaryOp(UnaryOp{
        operator: operator,
        expression: Box::new(expression),
        span: span,
    })
}

pub fn pf_unary_op(operator: Operator, expression: Expression, span: Span) -> Expression
{
    Expression::PostFixUnaryOp(UnaryOp{
        operator: operator,
        expression: Box::new(expression),
        span: span,
    })
}

#[cfg(test)]
pub fn member_access(name: &str, member: Member, span: Span) -> Expression
{
    Expression::MemberAccess(MemberAccess::new(name, member, span))
}

impl Expression
{
    pub fn precedence(&self) -> usize
    {
        match *self
        {
            Expression::BinaryOp(ref op) => op.operator.precedence(),
            _ => 0,
        }
    }

    pub fn span(&self) -> Span
    {
        match *self
        {
            Expression::IntLiteral(span, _) => span,
            Expression::FloatLiteral(span, _) => span,
            Expression::StringLiteral(span, _) => span,
            Expression::ArrayLiteral(ref a) => a.span,
            Expression::UnaryOp(ref op) => op.span,
            Expression::PostFixUnaryOp(ref op) => op.span,
            Expression::BinaryOp(ref op) => op.span,
            Expression::Enclosed(span, _) => span,
            Expression::Call(ref c) => c.span,
            Expression::NameRef(ref nr) => nr.span,
            Expression::Assignment(ref a) => a.span,
            Expression::ObjectConstruction(ref oc) => oc.span,
            Expression::MemberAccess(ref ma) => ma.span,
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
            Expression::IntLiteral(ref span, integer) => {
                println!("{}int {} ({})", p, integer, span);
            },
            Expression::FloatLiteral(ref span, ref s) => {
                println!("{}float {} ({})", p, s, span);
            },
            Expression::StringLiteral(ref span, ref s) => {
                println!("{}string \"{}\" ({})", p, s, span);
            },
            Expression::ArrayLiteral(ref a) => {
                println!("{}array ({})", p, a.span);
                for e in &a.elements {
                    e.print(level + 1);
                }
            },
            Expression::UnaryOp(ref op) => {
                println!("{}unary {} ({})", p, op.operator, op.span);
                op.expression.print(level + 1)
            },
            Expression::PostFixUnaryOp(ref op) => {
                println!("{}postfix unary {} ({})", p, op.operator, op.span);
                op.expression.print(level + 1)
            },
            Expression::BinaryOp(ref op) => {
                println!("{}binary {} ({})", p, op.operator, op.span);
                op.left.print(level + 1);
                op.right.print(level + 1)
            },
            Expression::Enclosed(ref span, ref e) => {
                println!("{}enclosed ({})", p, span);
                e.print(level + 1);
            },
            Expression::Call(ref c) => c.print(level),
            Expression::NameRef(ref nr) => nr.print(level),
            Expression::Assignment(ref a) => {
                println!("{}assign {} ({})", p, a.operator, a.span);
                a.target.print(level + 1);
                a.expression.print(level + 1);
            },
            Expression::ObjectConstruction(ref oc) => {
                println!("{}construct {} ({})", p, oc.object_type, oc.span);
                for p in &oc.args {
                    p.print(level + 1);
                }
            },
            Expression::MemberAccess(ref ma) => ma.print(level),
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
    Primitive(String),
    Complex(String),
    Pointer(Box<Type>),
}

impl Type
{
    pub fn ptr(t: Type) -> Type
    {
        Type::Pointer(Box::new(t))
    }
}

impl fmt::Display for Type
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error>
    {
        match *self
        {
            Type::Void => write!(f, "void"),
            Type::Unknown => write!(f, "unknown"),
            Type::Primitive(ref t) => write!(f, "{}", t),
            Type::Complex(ref s) => write!(f, "{}", s),
            Type::Pointer(ref st) => write!(f, "pointer to {}", st),
        }
    }
}


#[derive(Debug, Eq, PartialEq, Clone)]
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

#[derive(Debug, Eq, PartialEq, Clone)]
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
pub struct Module
{
    pub name: String,
    pub block: Block,
}

impl Module
{
    pub fn new(name: &str, block: Block) -> Module
    {
        Module{
            name: name.into(),
            block: block,
        }
    }
}

impl TreePrinter for Module
{
    fn print(&self, level: usize)
    {
        self.block.print(level)
    }
}
