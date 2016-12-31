use std::fmt;
use itertools::free::join;
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
        }
    }
}

#[derive(Debug, Clone)]
pub enum ByteCodeProperty
{
    Len,
    SumTypeIndex,
}

impl fmt::Display for ByteCodeProperty
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error>
    {
        match *self
        {
            ByteCodeProperty::Len => write!(f, "len"),
            ByteCodeProperty::SumTypeIndex => write!(f, "sum type index"),
        }
    }
}


#[derive(Debug, Clone)]
pub enum Instruction
{
    Store{dst: Var, src: Var},
    StoreLit{dst: Var, lit: ByteCodeLiteral},
    StoreFunc{dst: Var, func: String},
    LoadMember{dst: Var, obj: Var, member_index: usize},
    GetProperty{dst: Var, obj: Var, prop: ByteCodeProperty},
    SetProperty{dst: Var, prop: ByteCodeProperty, val: usize},
    UnaryOp{dst: Var, op: Operator, src: Var},
    BinaryOp{dst: Var, op: Operator, left: Var, right: Var},
    Call{dst: Var, func: String, args: Vec<Var>},
    Slice{dst: Var, src: Var, start: Var, len: Var},
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

pub fn store_func_instr(dst: &Var, func: &str) -> Instruction
{
    Instruction::StoreFunc{
        dst: dst.clone(),
        func: func.into(),
    }
}

pub fn load_member_instr(dst: &Var, obj: &Var, member_index: usize) -> Instruction
{
    Instruction::LoadMember{
        dst: dst.clone(),
        obj: obj.clone(),
        member_index: member_index,
    }
}

pub fn ret_instr(var: &Var) -> Instruction
{
    Instruction::Return(var.clone())
}

pub fn unary_op_instr(dst: &Var, op: Operator, src: Var) -> Instruction
{
    Instruction::UnaryOp{
        dst: dst.clone(),
        op: op,
        src: src,
    }
}

pub fn binary_op_instr(dst: &Var, op: Operator, left: Var, right: Var) -> Instruction
{
    Instruction::BinaryOp{
        dst: dst.clone(),
        op: op,
        left: left,
        right: right,
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

pub fn call_instr(dst: &Var, func: &str, args: Vec<Var>) -> Instruction
{
    Instruction::Call{
        dst: dst.clone(),
        func: func.into(),
        args: args
    }
}

pub fn set_prop_instr(dst: &Var, prop: ByteCodeProperty, value: usize) -> Instruction
{
    Instruction::SetProperty{
        dst: dst.clone(),
        prop: prop,
        val: value,
    }
}

pub fn get_prop_instr(dst: &Var, obj: &Var, prop: ByteCodeProperty) -> Instruction
{
    Instruction::GetProperty{
        dst: dst.clone(),
        obj: obj.clone(),
        prop: prop,
    }
}

pub fn slice_instr(dst: &Var, src: &Var, start: Var, len: Var) -> Instruction
{
    Instruction::Slice{
        dst: dst.clone(),
        src: src.clone(),
        start: start,
        len: len,
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

            Instruction::StoreFunc{ref dst, ref func} => {
                writeln!(f, "  strfunc {} {}", dst, func)
            },

            Instruction::LoadMember{ref dst, ref obj, member_index} => {
                writeln!(f, "  loadm {} {}.{}", dst, obj, member_index)
            },

            Instruction::GetProperty{ref dst, ref obj, ref prop} => {
                writeln!(f, "  getp {} {}.{}", dst, obj, prop)
            },

            Instruction::SetProperty{ref dst, ref prop, ref val} => {
                writeln!(f, "  setp {} {} {}", dst, prop, val)
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

            Instruction::Slice{ref dst, ref src, ref start, ref len} => {
                writeln!(f, "  slice {} {} {} {}", dst, src, start, len)
            },
        }
    }
}
