use std::fmt;
use itertools::free::join;
use ast::{ArrayProperty, Type};
use parser::Operator;
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
    UnaryOp(Operator, LLVar),
    BinaryOp(Operator, LLVar, LLVar),
    Call(String, Vec<LLVar>),
    StructMember(LLVar, usize),
    SumTypeIndex(LLVar),
    SumTypeStruct(LLVar, usize),
    SumTypeCase(usize),
    ArrayProperty(LLVar, ArrayProperty),
    ArrayHead(LLVar),
    ArrayTail(LLVar),
    Ref(LLVar),
    Func(String),
    HeapAlloc(Type),
}

impl fmt::Display for LLExpr
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error>
    {
        match *self
        {
            LLExpr::Literal(ref l) => l.fmt(f),
            LLExpr::UnaryOp(op, ref v) => write!(f, "{} {}", op, v),
            LLExpr::BinaryOp(op, ref a, ref b) => write!(f, "{} {} {}", a, op, b),
            LLExpr::Call(ref name, ref args) => write!(f, "{}({})", name, join(args.iter(), ", ")),
            LLExpr::StructMember(ref obj, index) => write!(f, "{}.{}", obj, index),
            LLExpr::SumTypeIndex(ref obj) => write!(f, "sum type index {}", obj),
            LLExpr::SumTypeStruct(ref obj, index) => write!(f, "{}.{}", obj, index),
            LLExpr::SumTypeCase(index) => write!(f, "sum type case {}", index),
            LLExpr::ArrayProperty(ref array, ref property) => write!(f, "{}.{:?}", array, property),
            LLExpr::ArrayHead(ref array) => write!(f, "head {}", array),
            LLExpr::ArrayTail(ref array) => write!(f, "tail {}", array),
            LLExpr::Ref(ref obj) => write!(f, "ref {}", obj),
            LLExpr::Func(ref func) => write!(f, "func {}", func),
            LLExpr::HeapAlloc(ref typ) => write!(f, "heap_alloc {}", typ),
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
    Alloc(LLVar),
    SetStructMember(LLSetStructMember),
    StartScope,
    EndScope,
    Bind(LLBind),
    Set(LLSet),
    Return(LLVar),
    ReturnVoid,
    Branch(LLBasicBlockRef),
    BranchIf(LLBranchIf),
    IncRef(LLVar),
    DecRef(LLVar),
}

pub fn set_instr(var: &LLVar, e: LLExpr) -> LLInstruction
{
    LLInstruction::Set(LLSet{
        var: var.clone(),
        expr: e,
    })
}

pub fn set_struct_member_instr(obj: &LLVar, index: usize, e: &LLVar) -> LLInstruction
{
    LLInstruction::SetStructMember(LLSetStructMember{
        obj: obj.clone(),
        member_index: index,
        value: e.clone(),
    })
}

pub fn ret_instr(var: &LLVar) -> LLInstruction
{
    LLInstruction::Return(var.clone())
}

pub fn bind_instr(name: &str, var: &LLVar) -> LLInstruction
{
    LLInstruction::Bind(LLBind{
        name: name.into(),
        var: var.clone(),
    })
}

pub fn branch_if_instr(cond: &LLVar, on_true: LLBasicBlockRef, on_false: LLBasicBlockRef) -> LLInstruction
{
    LLInstruction::BranchIf(LLBranchIf{
        cond: cond.clone(),
        on_true: on_true,
        on_false: on_false,
    })
}

impl fmt::Display for LLInstruction
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error>
    {
        match *self
        {
            LLInstruction::Alloc(ref var) => {
                writeln!(f, "  alloc {}", var)
            },
            LLInstruction::SetStructMember(ref s) => {
                writeln!(f, "  set {}.{} = {}", s.obj, s.member_index, s.value)
            },
            LLInstruction::StartScope => {
                writeln!(f, "  scope start")
            },
            LLInstruction::EndScope => {
                writeln!(f, "  scope end")
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
            LLInstruction::IncRef(ref v) => {
                writeln!(f, "  incref {}", v.name)
            },
            LLInstruction::DecRef(ref v) => {
                writeln!(f, "  decref {}", v.name)
            },
        }
    }
}
