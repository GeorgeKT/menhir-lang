use std::fmt;
use itertools::free::join;
use ast::{ArrayProperty};
use llrep::llfunction::{LLBasicBlockRef, LLVar};

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
    Call(String, Vec<LLVar>),
    StructMember(LLVar, usize),
    SumTypeIndex(LLVar),
    SumTypeStruct(LLVar, usize),
    ArrayProperty(LLVar, ArrayProperty),
    ArrayHead(LLVar),
    ArrayTail(LLVar),
}

impl LLExpr
{
    pub fn rename(&mut self, bad_name: &str, new_name: &str)
    {
        match *self
        {
            LLExpr::Literal(_) => {},
            LLExpr::Add(ref mut a, ref mut b) => {
                a.rename_if_equals(bad_name, new_name);
                b.rename_if_equals(bad_name, new_name);
            },
            LLExpr::Sub(ref mut a, ref mut b) => {
                a.rename_if_equals(bad_name, new_name);
                b.rename_if_equals(bad_name, new_name);
            },
            LLExpr::Mul(ref mut a, ref mut b) => {
                a.rename_if_equals(bad_name, new_name);
                b.rename_if_equals(bad_name, new_name);
            },
            LLExpr::Div(ref mut a, ref mut b) => {
                a.rename_if_equals(bad_name, new_name);
                b.rename_if_equals(bad_name, new_name);
            },
            LLExpr::Mod(ref mut a, ref mut b) => {
                a.rename_if_equals(bad_name, new_name);
                b.rename_if_equals(bad_name, new_name);
            },
            LLExpr::And(ref mut a, ref mut b) => {
                a.rename_if_equals(bad_name, new_name);
                b.rename_if_equals(bad_name, new_name);
            },
            LLExpr::Or(ref mut a, ref mut b) => {
                a.rename_if_equals(bad_name, new_name);
                b.rename_if_equals(bad_name, new_name);
            },
            LLExpr::LT(ref mut a, ref mut b) => {
                a.rename_if_equals(bad_name, new_name);
                b.rename_if_equals(bad_name, new_name);
            },
            LLExpr::LTE(ref mut a, ref mut b) => {
                a.rename_if_equals(bad_name, new_name);
                b.rename_if_equals(bad_name, new_name);
            },
            LLExpr::GT(ref mut a, ref mut b) => {
                a.rename_if_equals(bad_name, new_name);
                b.rename_if_equals(bad_name, new_name);
            },
            LLExpr::GTE(ref mut a, ref mut b) => {
                a.rename_if_equals(bad_name, new_name);
                b.rename_if_equals(bad_name, new_name);
            },
            LLExpr::EQ(ref mut a, ref mut b) => {
                a.rename_if_equals(bad_name, new_name);
                b.rename_if_equals(bad_name, new_name);
            },
            LLExpr::NEQ(ref mut a, ref mut b) => {
                a.rename_if_equals(bad_name, new_name);
                b.rename_if_equals(bad_name, new_name);
            },
            LLExpr::USub(ref mut v) => {
                v.rename_if_equals(bad_name, new_name);
            },
            LLExpr::Not(ref mut v) => {
                v.rename_if_equals(bad_name, new_name);
            },
            LLExpr::Call(_, ref mut args) => {
                for arg in args.iter_mut() {
                    arg.rename_if_equals(bad_name, new_name);
                }
            },
            LLExpr::StructMember(ref mut obj, _) => {
                obj.rename_if_equals(bad_name, new_name);
            },
            LLExpr::SumTypeIndex(ref mut obj) => {
                obj.rename_if_equals(bad_name, new_name);
            },
            LLExpr::SumTypeStruct(ref mut obj, _) => {
                obj.rename_if_equals(bad_name, new_name);
            },
            LLExpr::ArrayProperty(ref mut array, _) => {
                array.rename_if_equals(bad_name, new_name);
            },
            LLExpr::ArrayHead(ref mut array) => {
                array.rename_if_equals(bad_name, new_name);
            },
            LLExpr::ArrayTail(ref mut array) => {
                array.rename_if_equals(bad_name, new_name);
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
            LLExpr::Call(ref name, ref args) => write!(f, "{}({})", name, join(args.iter(), ", ")),
            LLExpr::StructMember(ref obj, index) => write!(f, "{}.{}", obj, index),
            LLExpr::SumTypeIndex(ref obj) => write!(f, "sum type index {}", obj),
            LLExpr::SumTypeStruct(ref obj, index) => write!(f, "{}.{}", obj, index),
            LLExpr::ArrayProperty(ref array, ref property) => write!(f, "{}.{:?}", array, property),
            LLExpr::ArrayHead(ref array) => write!(f, "head {}", array),
            LLExpr::ArrayTail(ref array) => write!(f, "tail {}", array),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LLSetStructMember
{
    pub obj: LLVar,
    pub member_index: usize,
    pub value: LLVar
}

#[derive(Debug, Clone)]
pub struct LLBind
{
    pub name: String,
    pub var: LLVar,
}

#[derive(Debug, Clone)]
pub struct LLSet
{
    pub var: LLVar,
    pub expr: LLExpr
}

#[derive(Debug, Clone)]
pub struct LLUpdate
{
    pub dst: LLVar,
    pub src: LLVar,
}

#[derive(Debug, Clone)]
pub struct LLBranchIf
{
    pub cond: LLVar,
    pub on_true: LLBasicBlockRef,
    pub on_false: LLBasicBlockRef,
}

#[derive(Debug, Clone)]
pub enum LLInstruction
{
    //SetArrayElement{var: LLVar, index: LLExpr, value: LLExpr},
    StackAlloc(LLVar),
    SetStructMember(LLSetStructMember),
    StartScope,
    EndScope(LLVar),
    Bind(LLBind),
    Set(LLSet),
    Return(LLVar),
    ReturnVoid,
    Branch(LLBasicBlockRef),
    BranchIf(LLBranchIf),
    NOP,
}

pub fn set_instr(var: LLVar, e: LLExpr) -> LLInstruction
{
    LLInstruction::Set(LLSet{
        var: var,
        expr: e,
    })
}

pub fn set_struct_member_instr(obj: LLVar, index: usize, e: LLVar) -> LLInstruction
{
    LLInstruction::SetStructMember(LLSetStructMember{
        obj: obj,
        member_index: index,
        value: e,
    })
}

pub fn ret_instr(var: LLVar) -> LLInstruction
{
    LLInstruction::Return(var)
}

pub fn bind_instr(name: &str, var: LLVar) -> LLInstruction
{
    LLInstruction::Bind(LLBind{
        name: name.into(),
        var: var,
    })
}

pub fn branch_if_instr(cond: LLVar, on_true: LLBasicBlockRef, on_false: LLBasicBlockRef) -> LLInstruction
{
    LLInstruction::BranchIf(LLBranchIf{
        cond: cond,
        on_true: on_true,
        on_false: on_false,
    })
}

impl LLInstruction
{
    pub fn rename(&mut self, bad_name: &str, new_name: &str)
    {
        match *self
        {
            LLInstruction::SetStructMember(ref mut s)=> {
                s.obj.rename_if_equals(bad_name, new_name);
                s.value.rename_if_equals(bad_name, new_name);
            },

            LLInstruction::EndScope(ref mut ret_var) => {
                ret_var.rename_if_equals(bad_name, new_name);
            },

            LLInstruction::Bind(ref mut b) => {
                b.var.rename_if_equals(bad_name, new_name);
            },

            LLInstruction::Set(ref mut s) => {
                s.var.rename_if_equals(bad_name, new_name);
                s.expr.rename(bad_name, new_name);
            },

            LLInstruction::Return(ref mut var) => {
                var.rename_if_equals(bad_name, new_name);
            },

            LLInstruction::ReturnVoid => {},
            LLInstruction::StartScope => {},
            LLInstruction::NOP => {},
            LLInstruction::Branch(_) => {},
            LLInstruction::BranchIf(ref mut b) => {
                b.cond.rename_if_equals(bad_name, new_name);
            },
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
            LLInstruction::SetStructMember(ref s) => {
                writeln!(f, "  set {}.{} = {}", s.obj, s.member_index, s.value)
            },
            LLInstruction::StartScope => {
                writeln!(f, "  scope start")
            },
            LLInstruction::EndScope(ref ret_var) => {
                writeln!(f, "  scope end (ret: {})", ret_var.name)
            },
            LLInstruction::Bind(ref b) => {
                writeln!(f, "  bind {} = {}", b.name, b.var.name)
            },
            LLInstruction::Set(ref s) => {
                writeln!(f, "  set {} = {}", s.var, s.expr)
            },
            LLInstruction::Return(ref var) => {
                writeln!(f, "  ret {}", var)
            },
            LLInstruction::ReturnVoid => {
                writeln!(f, "  ret void")
            },
            LLInstruction::Branch(ref name) => {
                writeln!(f, "  br {}", name)
            },
            LLInstruction::BranchIf(ref b) => {
                writeln!(f, "  brif {} ? {} : {} ", b.cond, b.on_true, b.on_false)
            },
            LLInstruction::NOP => {
                writeln!(f, "  nop")
            },
        }
    }
}
