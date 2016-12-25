use std::fmt;
use itertools::free::join;
use ast::{ArrayProperty, Type};
use parser::Operator;
use bytecode::function::{BasicBlockRef, Var};

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
pub enum Instruction
{
    //SetArrayElement{var: Var, index: ByteCodeExpression, value: ByteCodeExpression},
    Alloc(Var),
    SetStructMember{obj: Var, member_index: usize, value: Var},
    StartScope,
    EndScope,
    Bind{name: String, var: Var},
    Set{var: Var, expr: ByteCodeExpression},
    Return(Var),
    ReturnVoid,
    Branch(BasicBlockRef),
    BranchIf{cond: Var, on_true: BasicBlockRef, on_false: BasicBlockRef},
    IncRef(Var),
    DecRef(Var),
}

pub fn set_instr(var: &Var, e: ByteCodeExpression) -> Instruction
{
    Instruction::Set{
        var: var.clone(),
        expr: e,
    }
}

pub fn set_struct_member_instr(obj: &Var, index: usize, e: &Var) -> Instruction
{
    Instruction::SetStructMember{
        obj: obj.clone(),
        member_index: index,
        value: e.clone(),
    }
}

pub fn ret_instr(var: &Var) -> Instruction
{
    Instruction::Return(var.clone())
}

pub fn bind_instr(name: &str, var: &Var) -> Instruction
{
    Instruction::Bind{
        name: name.into(),
        var: var.clone(),
    }
}

pub fn branch_if_instr(cond: &Var, on_true: BasicBlockRef, on_false: BasicBlockRef) -> Instruction
{
    Instruction::BranchIf{
        cond: cond.clone(),
        on_true: on_true,
        on_false: on_false,
    }
}

impl fmt::Display for Instruction
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error>
    {
        match *self
        {
            Instruction::Alloc(ref var) => {
                writeln!(f, "  alloc {}", var)
            },
            Instruction::SetStructMember{ref obj, member_index, ref value} => {
                writeln!(f, "  set {}.{} = {}", obj, member_index, value)
            },
            Instruction::StartScope => {
                writeln!(f, "  scope start")
            },
            Instruction::EndScope => {
                writeln!(f, "  scope end")
            },
            Instruction::Bind{ref name, ref var} => {
                writeln!(f, "  bind {} = {}", name, var.name)
            },
            Instruction::Set{ref var, ref expr} => {
                writeln!(f, "  set {} = {}", var, expr)
            },
            Instruction::Return(ref var) => {
                writeln!(f, "  ret {}", var)
            },
            Instruction::ReturnVoid => {
                writeln!(f, "  ret void")
            },
            Instruction::Branch(ref name) => {
                writeln!(f, "  br {}", name)
            },
            Instruction::BranchIf{ref cond, ref on_true, ref on_false} => {
                writeln!(f, "  brif {} ? {} : {} ", cond, on_true, on_false)
            },
            Instruction::IncRef(ref v) => {
                writeln!(f, "  incref {}", v.name)
            },
            Instruction::DecRef(ref v) => {
                writeln!(f, "  decref {}", v.name)
            },
        }
    }
}
