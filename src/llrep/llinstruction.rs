use std::fmt;
use itertools::free::join;
use ast::{Type, Literal};
use llrep::llfunction::LLVar;

#[derive(Debug, Clone)]
pub enum LLLiteral
{
    Int(u64),
    Float(String),
    Char(u8),
    String(String),
    Bool(bool),
    Array(Vec<LLVar>),
}

impl fmt::Display for LLLiteral
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error>
    {
        match *self
        {
            LLLiteral::Int(v) => write!(f, "int {}", v),
            LLLiteral::Float(ref v) => write!(f, "float {}", v),
            LLLiteral::Char(v) => write!(f, "char {}", v),
            LLLiteral::String(ref v) => write!(f, "string {}", v),
            LLLiteral::Bool(v) => write!(f, "bool {}", v),
            LLLiteral::Array(ref elements) => write!(f, "[{}]", join(elements.iter(), ", ")),
        }
    }
}

#[derive(Debug, Clone)]
pub enum LLExpr
{
    Literal(LLLiteral),
    Ref(LLVar),
    Add(LLVar, LLVar),
    Sub(LLVar, LLVar),
    Mul(LLVar, LLVar),
    Div(LLVar, LLVar),
    Mod(LLVar, LLVar),
    And(LLVar, LLVar),
    Or(LLVar, LLVar),
    LT(LLVar, LLVar),
    LTE(LLVar, LLVar),
    GT(LLVar, LLVar),
    GTE(LLVar, LLVar),
    EQ(LLVar, LLVar),
    NEQ(LLVar, LLVar),
    USub(LLVar),
    Not(LLVar),
    Load(String),
    Call{name: String, args: Vec<LLVar>},
}

impl fmt::Display for LLExpr
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error>
    {
        match *self
        {
            LLExpr::Literal(ref l) => l.fmt(f),
            LLExpr::Ref(ref v) => write!(f, "ref {}", v),
            LLExpr::Add(ref a, ref b) => write!(f, "{} + {}", a, b),
            LLExpr::Sub(ref a, ref b) => write!(f, "{} - {}", a, b),
            LLExpr::Mul(ref a, ref b) => write!(f, "{} * {}", a, b),
            LLExpr::Div(ref a, ref b) => write!(f, "{} / {}", a, b),
            LLExpr::Mod(ref a, ref b) => write!(f, "{} % {}", a, b),
            LLExpr::And(ref a, ref b) => write!(f, "{} && {}", a, b),
            LLExpr::Or(ref a, ref b) => write!(f, "{} || {}", a, b),
            LLExpr::LT(ref a, ref b) => write!(f, "{} < {}", a, b),
            LLExpr::LTE(ref a, ref b) => write!(f, "{} <= {}", a, b),
            LLExpr::GT(ref a, ref b) => write!(f, "{} > {}", a, b),
            LLExpr::GTE(ref a, ref b) => write!(f, "{} >= {}", a, b),
            LLExpr::EQ(ref a, ref b) => write!(f, "{} == {}", a, b),
            LLExpr::NEQ(ref a, ref b) => write!(f, "{} != {}", a, b),
            LLExpr::USub(ref v) => write!(f, "- {}", v),
            LLExpr::Not(ref v) => write!(f, "! {}", v),
            LLExpr::Load(ref name) => write!(f, "load {}", name),
            LLExpr::Call{ref name, ref args} => write!(f, "{}({})", name, join(args.iter(), ", ")),
        }
    }
}



#[derive(Debug, Clone)]
pub enum LLInstruction
{
    //StackAlloc{var: LLVar, typ: Type},
    //SetArrayElement{var: LLVar, index: LLExpr, value: LLExpr},
    //SetStructElement{var: LLVar, member_index: usize, value: LLExpr},
    StartScope,
    EndScope{ret_var: LLVar},
    Bind{name: String, var: LLVar},
    Set{var: LLVar, expr: LLExpr},
    Return(LLVar),
    ReturnVoid,
}

impl LLInstruction
{
    pub fn set(var: LLVar, e: LLExpr) -> LLInstruction
    {
        LLInstruction::Set{
            var: var,
            expr: e,
        }
    }

    pub fn ret(var: LLVar) -> LLInstruction
    {
        LLInstruction::Return(var)
    }

    pub fn bind(name: &str, var: LLVar) -> LLInstruction
    {
        LLInstruction::Bind{
            name: name.into(),
            var: var,
        }
    }
}

impl fmt::Display for LLInstruction
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error>
    {
        match *self
        {
            LLInstruction::StartScope => writeln!(f, "  scope start"),
            LLInstruction::EndScope{ref ret_var} => writeln!(f, "  scope end (ret: {})", ret_var.name),
            LLInstruction::Bind{ref name, ref var} => writeln!(f, "  bind {} = {}", name, var.name),
            LLInstruction::Set{ref var, ref expr} => writeln!(f, "  set {} = {}", var, expr),
            LLInstruction::Return(ref var) => writeln!(f, "  ret {}", var),
            LLInstruction::ReturnVoid => writeln!(f, "  ret void"),
        }
    }
}
