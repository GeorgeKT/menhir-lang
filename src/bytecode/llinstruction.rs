use std::fmt;
use itertools::free::join;
use ast::{ArrayProperty, Type};
use parser::Operator;
use bytecode::llfunction::{BasicBlockRef, Var};

#[derive(Debug, Clone)]
pub enum ByteCodeLiteral
{
    Int(u64),
    Float(String),
    Char(u8),
    String(String),
    Bool(bool),
    Array(Vec<Var>),
}

impl fmt::Display for ByteCodeLiteral
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error>
    {
        match *self
        {
            ByteCodeLiteral::Int(v) => write!(f, "int {}", v),
            ByteCodeLiteral::Float(ref v) => write!(f, "float {}", v),
            ByteCodeLiteral::Char(v) => write!(f, "char {}", v),
            ByteCodeLiteral::String(ref v) => write!(f, "string {}", v),
            ByteCodeLiteral::Bool(v) => write!(f, "bool {}", v),
            ByteCodeLiteral::Array(ref elements) => write!(f, "[{}]", join(elements.iter(), ", ")),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ByteCodeExpression
{
    Literal(ByteCodeLiteral),
    UnaryOp(Operator, Var),
    BinaryOp(Operator, Var, Var),
    Call(String, Vec<Var>),
    StructMember(Var, usize),
    SumTypeIndex(Var),
    SumTypeStruct(Var, usize),
    SumTypeCase(usize),
    ArrayProperty(Var, ArrayProperty),
    ArrayHead(Var),
    ArrayTail(Var),
    Ref(Var),
    Func(String),
    HeapAlloc(Type),
}

impl fmt::Display for ByteCodeExpression
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error>
    {
        match *self
        {
            ByteCodeExpression::Literal(ref l) => l.fmt(f),
            ByteCodeExpression::UnaryOp(op, ref v) => write!(f, "{} {}", op, v),
            ByteCodeExpression::BinaryOp(op, ref a, ref b) => write!(f, "{} {} {}", a, op, b),
            ByteCodeExpression::Call(ref name, ref args) => write!(f, "{}({})", name, join(args.iter(), ", ")),
            ByteCodeExpression::StructMember(ref obj, index) => write!(f, "{}.{}", obj, index),
            ByteCodeExpression::SumTypeIndex(ref obj) => write!(f, "sum type index {}", obj),
            ByteCodeExpression::SumTypeStruct(ref obj, index) => write!(f, "{}.{}", obj, index),
            ByteCodeExpression::SumTypeCase(index) => write!(f, "sum type case {}", index),
            ByteCodeExpression::ArrayProperty(ref array, ref property) => write!(f, "{}.{:?}", array, property),
            ByteCodeExpression::ArrayHead(ref array) => write!(f, "head {}", array),
            ByteCodeExpression::ArrayTail(ref array) => write!(f, "tail {}", array),
            ByteCodeExpression::Ref(ref obj) => write!(f, "ref {}", obj),
            ByteCodeExpression::Func(ref func) => write!(f, "func {}", func),
            ByteCodeExpression::HeapAlloc(ref typ) => write!(f, "heap_alloc {}", typ),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LLSetStructMember
{
    pub obj: Var,
    pub member_index: usize,
    pub value: Var
}

#[derive(Debug, Clone)]
pub struct LLBind
{
    pub name: String,
    pub var: Var,
}

#[derive(Debug, Clone)]
pub struct LLSet
{
    pub var: Var,
    pub expr: ByteCodeExpression
}

#[derive(Debug, Clone)]
pub struct LLBranchIf
{
    pub cond: Var,
    pub on_true: BasicBlockRef,
    pub on_false: BasicBlockRef,
}

#[derive(Debug, Clone)]
pub enum LLInstruction
{
    //SetArrayElement{var: Var, index: ByteCodeExpression, value: ByteCodeExpression},
    Alloc(Var),
    SetStructMember(LLSetStructMember),
    StartScope,
    EndScope,
    Bind(LLBind),
    Set(LLSet),
    Return(Var),
    ReturnVoid,
    Branch(BasicBlockRef),
    BranchIf(LLBranchIf),
    IncRef(Var),
    DecRef(Var),
}

pub fn set_instr(var: &Var, e: ByteCodeExpression) -> LLInstruction
{
    LLInstruction::Set(LLSet{
        var: var.clone(),
        expr: e,
    })
}

pub fn set_struct_member_instr(obj: &Var, index: usize, e: &Var) -> LLInstruction
{
    LLInstruction::SetStructMember(LLSetStructMember{
        obj: obj.clone(),
        member_index: index,
        value: e.clone(),
    })
}

pub fn ret_instr(var: &Var) -> LLInstruction
{
    LLInstruction::Return(var.clone())
}

pub fn bind_instr(name: &str, var: &Var) -> LLInstruction
{
    LLInstruction::Bind(LLBind{
        name: name.into(),
        var: var.clone(),
    })
}

pub fn branch_if_instr(cond: &Var, on_true: BasicBlockRef, on_false: BasicBlockRef) -> LLInstruction
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
