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
pub enum ByteCodeProperty
{
    ArrayProperty(ArrayProperty),
    SumTypeIndex,
}

impl fmt::Display for ByteCodeProperty
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error>
    {
        match *self
        {
            ByteCodeProperty::ArrayProperty(ref ap) => write!(f, "array property {}", ap),
            ByteCodeProperty::SumTypeIndex => write!(f, "sum type index"),
        }
    }
}


#[derive(Debug, Clone)]
pub enum Instruction
{
    Store{dst: Var, src: Var},
    StoreLit{dst: Var, lit: ByteCodeLiteral},
    Load{dst: Var, src: Var},
    LoadMember{dst: Var, obj: Var, member_index: usize},
    LoadProperty{dst: Var, obj: Var, prop: ByteCodeProperty},
    UnaryOp{dst: Var, op: Operator, src: Var},
    BinaryOp{dst: Var, op: Operator, left: Var, right: Var},
    Call{dst: Var, func: String, args: Vec<Var>},
    StackAlloc(Var),
    HeapAlloc(Var),
    StartScope,
    EndScope,
    Return(Var),
    ReturnVoid,
    Branch(BasicBlockRef),
    BranchIf{cond: Var, on_true: BasicBlockRef, on_false: BasicBlockRef},
    Delete(Var),
}

pub fn store_instr(dst: &Var, src: &Var) -> Instruction
{
    Instruction::Store{
        dst: dst.clone(),
        src: src.clone(),
    }
}

pub fn store_lit_instr(dst: &Var, lit: ByteCodeLiteral) -> Instruction
{
    Instruction::StoreLit{
        dst: dst.clone(),
        lit: lit,
    }
}

pub fn load_instr(dst: &Var, src: &Var) -> Instruction
{
    Instruction::Load{
        dst: dst.clone(),
        src: src.clone(),
    }
}

pub fn ret_instr(var: &Var) -> Instruction
{
    Instruction::Return(var.clone())
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
            Instruction::Store{ref dst, ref src} => {
                writeln!(f, "  str {} {}", dst, src)
            },

            Instruction::StoreLit{ref dst, ref lit} => {
                writeln!(f, "  strlit {} {}", dst, lit)
            },

            Instruction::Load{ref dst, ref src} => {
                writeln!(f, "  ldr {} {}", dst, src)
            },

            Instruction::LoadMember{ref dst, ref obj, member_index} => {
                writeln!(f, "  ldrm {} {}.{}", dst, obj, member_index)
            },

            Instruction::LoadProperty{ref dst, ref obj, ref prop} => {
                writeln!(f, "  ldrp {} {}.{}", dst, obj, prop)
            },

            Instruction::UnaryOp{ref dst, ref op, ref src} => {
                writeln!(f, "  uop {} {} {}", dst, op, src)
            },

            Instruction::BinaryOp{ref dst, ref op, ref left, ref right} => {
                writeln!(f, "  bop {} {} {} {}", dst, op, left, right)
            },

            Instruction::Call{ref dst, ref func, ref args} => {
                writeln!(f, "  call {} {} {}", dst, func, join(args.iter(), " "))
            },

            Instruction::StackAlloc(ref var) => {
                writeln!(f, "  salloc {}", var)
            },

            Instruction::HeapAlloc(ref var) => {
                writeln!(f, "  halloc {}", var)
            },

            Instruction::StartScope => {
                writeln!(f, "  scope start")
            },

            Instruction::EndScope => {
                writeln!(f, "  scope end")
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

            Instruction::Delete(ref var) => {
                writeln!(f, "  delete {}", var)
            },
        }
    }
}
