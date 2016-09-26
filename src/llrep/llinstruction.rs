use std::fmt;
use itertools::free::join;
use ast::{Type, Literal, ArrayProperty};
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
    StructMember{obj: LLVar, index: usize},
    ArrayProperty{array: LLVar, property: ArrayProperty},
}

impl LLExpr
{
    pub fn replace_by_ret(&mut self, bad_name: &str)
    {
        match *self
        {
            LLExpr::Literal(_) => {},
            LLExpr::Add(ref mut a, ref mut b) => {
                a.replace_by_ret(bad_name);
                b.replace_by_ret(bad_name);
            },
            LLExpr::Sub(ref mut a, ref mut b) => {
                a.replace_by_ret(bad_name);
                b.replace_by_ret(bad_name);
            },
            LLExpr::Mul(ref mut a, ref mut b) => {
                a.replace_by_ret(bad_name);
                b.replace_by_ret(bad_name);
            },
            LLExpr::Div(ref mut a, ref mut b) => {
                a.replace_by_ret(bad_name);
                b.replace_by_ret(bad_name);
            },
            LLExpr::Mod(ref mut a, ref mut b) => {
                a.replace_by_ret(bad_name);
                b.replace_by_ret(bad_name);
            },
            LLExpr::And(ref mut a, ref mut b) => {
                a.replace_by_ret(bad_name);
                b.replace_by_ret(bad_name);
            },
            LLExpr::Or(ref mut a, ref mut b) => {
                a.replace_by_ret(bad_name);
                b.replace_by_ret(bad_name);
            },
            LLExpr::LT(ref mut a, ref mut b) => {
                a.replace_by_ret(bad_name);
                b.replace_by_ret(bad_name);
            },
            LLExpr::LTE(ref mut a, ref mut b) => {
                a.replace_by_ret(bad_name);
                b.replace_by_ret(bad_name);
            },
            LLExpr::GT(ref mut a, ref mut b) => {
                a.replace_by_ret(bad_name);
                b.replace_by_ret(bad_name);
            },
            LLExpr::GTE(ref mut a, ref mut b) => {
                a.replace_by_ret(bad_name);
                b.replace_by_ret(bad_name);
            },
            LLExpr::EQ(ref mut a, ref mut b) => {
                a.replace_by_ret(bad_name);
                b.replace_by_ret(bad_name);
            },
            LLExpr::NEQ(ref mut a, ref mut b) => {
                a.replace_by_ret(bad_name);
                b.replace_by_ret(bad_name);
            },
            LLExpr::USub(ref mut v) => {
                v.replace_by_ret(bad_name);
            },
            LLExpr::Not(ref mut v) => {
                v.replace_by_ret(bad_name);
            },
            LLExpr::Load(_) => {},
            LLExpr::Call{ref name, ref mut args} => {
                for arg in args.iter_mut() {
                    arg.replace_by_ret(bad_name);
                }
            },
            LLExpr::StructMember{ref mut obj, index} => {
                obj.replace_by_ret(bad_name);
            },
            LLExpr::ArrayProperty{ref mut array, ref property} => {
                array.replace_by_ret(bad_name);
            },
        }
    }
}

impl fmt::Display for LLExpr
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error>
    {
        match *self
        {
            LLExpr::Literal(ref l) => l.fmt(f),
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
            LLExpr::StructMember{ref obj, index} => write!(f, "{}.{}", obj, index),
            LLExpr::ArrayProperty{ref array, ref property} => write!(f, "{}.{:?}", array, property),
        }
    }
}



#[derive(Debug, Clone)]
pub enum LLInstruction
{
    //SetArrayElement{var: LLVar, index: LLExpr, value: LLExpr},
    StackAlloc(LLVar),
    SetStructMember{obj: LLVar, member_index: usize, value: LLVar},
    StartScope,
    EndScope{ret_var: LLVar},
    Bind{name: String, var: LLVar},
    Set{var: LLVar, expr: LLExpr},
    Return(LLVar),
    ReturnVoid,
    NOP,
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

    pub fn set_struct_member(obj: LLVar, index: usize, e: LLVar) -> LLInstruction
    {
        LLInstruction::SetStructMember{
            obj: obj,
            member_index: index,
            value: e,
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

    pub fn replace_by_ret(&mut self, bad_name: &str)
    {
        match *self
        {
            LLInstruction::SetStructMember{ref mut obj, member_index, ref mut value} => {
                obj.replace_by_ret(bad_name);
                value.replace_by_ret(bad_name);
            },

            LLInstruction::EndScope{ref mut ret_var} => {
                ret_var.replace_by_ret(bad_name);
            },

            LLInstruction::Bind{ref name, ref mut var} => {
                var.replace_by_ret(bad_name);
            },

            LLInstruction::Set{ref mut var, ref mut expr} => {
                var.replace_by_ret(bad_name);
                expr.replace_by_ret(bad_name);
            },

            LLInstruction::Return(ref mut var) => {
                var.replace_by_ret(bad_name);
            },

            LLInstruction::ReturnVoid => {},
            LLInstruction::StartScope => {},
            LLInstruction::NOP => {},
            LLInstruction::StackAlloc(ref var) => {
                if var.name == bad_name {
                    panic!("Internal Compiler Error: StackAlloc cannot do replace_by_ret");
                }
            },
        }
    }
}

impl fmt::Display for LLInstruction
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error>
    {
        match *self
        {
            LLInstruction::StackAlloc(ref var) => {
                writeln!(f, "  stack alloc {}", var)
            },
            LLInstruction::SetStructMember{ref obj, ref member_index, ref value} => {
                writeln!(f, "  set {}.{} = {}", obj, member_index, value)
            },
            LLInstruction::StartScope => {
                writeln!(f, "  scope start")
            },
            LLInstruction::EndScope{ref ret_var} => {
                writeln!(f, "  scope end (ret: {})", ret_var.name)
            },
            LLInstruction::Bind{ref name, ref var} => {
                writeln!(f, "  bind {} = {}", name, var.name)
            },
            LLInstruction::Set{ref var, ref expr} => {
                writeln!(f, "  set {} = {}", var, expr)
            },
            LLInstruction::Return(ref var) => {
                writeln!(f, "  ret {}", var)
            },
            LLInstruction::ReturnVoid => {
                writeln!(f, "  ret void")
            },
            LLInstruction::NOP => {
                writeln!(f, "  nop")
            },
        }
    }
}
