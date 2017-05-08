use std::fmt;
use itertools::free::join;
use ast::{UnaryOperator, BinaryOperator, Type, IntSize, FloatSize, ptr_type, array_type};
use bytecode::function::{BasicBlockRef, Var};
use target::native_int_size;


#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
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
            ByteCodeProperty::SumTypeIndex => write!(f, "sum_type_index"),
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum Constant
{
    Int(i64, IntSize),
    UInt(u64, IntSize),
    Float(f64, FloatSize),
    Char(char),
    String(String),
    Bool(bool),
    Array(Vec<Constant>),
}

impl Constant
{
    pub fn get_type(&self) -> Type
    {
        match *self
        {
            Constant::Int(_, int_size) => Type::Int(int_size),
            Constant::UInt(_, int_size) => Type::UInt(int_size),
            Constant::Float(_, float_size) => Type::Float(float_size),
            Constant::Char(_) => Type::Char,
            Constant::String(_) => Type::String,
            Constant::Bool(_) => Type::Bool,
            Constant::Array(ref members) => {
                array_type(members[0].get_type(), members.len())
            }
        }
    }
}

impl fmt::Display for Constant
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error>
    {
        match *self
        {
            Constant::Int(v, int_size) => write!(f, "(int{} {})", int_size, v),
            Constant::UInt(v, int_size) => write!(f, "(uint{} {})", int_size, v),
            Constant::Float(v, float_size) => write!(f, "(float{} {})", float_size, v),
            Constant::Char(v) => write!(f, "(char {})", v),
            Constant::String(ref v) => write!(f, "(string {})", v),
            Constant::Bool(v) => write!(f, "(bool {})", v),
            Constant::Array(ref m) => write!(f, "[{}]", join(m.iter(), ", ")),
        }
    }
}


#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Operand
{
    Var(Var),
    AddressOf(Var),
    Const(Constant),
    Func(String),
}

impl Operand
{
    pub fn const_int(v: i64, int_size: IntSize) -> Operand
    {
        Operand::Const(Constant::Int(v, int_size))
    }

    pub fn const_uint(v: u64, int_size: IntSize) -> Operand
    {
        Operand::Const(Constant::UInt(v, int_size))
    }

    pub fn const_float(v: f64, float_size: FloatSize) -> Operand
    {
        Operand::Const(Constant::Float(v, float_size))
    }

    pub fn const_bool(v: bool) -> Operand
    {
        Operand::Const(Constant::Bool(v))
    }

    pub fn const_char(v: char) -> Operand
    {
        Operand::Const(Constant::Char(v))
    }

    pub fn const_string<S: Into<String>>(s: S) -> Operand
    {
        Operand::Const(Constant::String(s.into()))
    }

    pub fn get_type(&self) -> Type
    {
        match *self
        {
            Operand::Var(ref var) => var.typ.clone(),
            Operand::AddressOf(ref var) => ptr_type(var.typ.clone()),
            Operand::Const(ref c) => c.get_type(),
            Operand::Func(_) => Type::Unknown,
        }
    }
}

impl fmt::Display for Operand
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error>
    {
        match *self
        {
            Operand::Var(ref var) => write!(f, "{}", var),
            Operand::AddressOf(ref var) => write!(f, "&{}", var),
            Operand::Const(ref c) => write!(f, "{}", c),
            Operand::Func(ref func) => write!(f, "(func {})", func),
        }
    }
}

pub fn var_op(v: &Var) -> Operand
{
    Operand::Var(v.clone())
}

pub fn float_op(fstr: &str, float_size: FloatSize) -> Operand
{
    match fstr.parse::<f64>()
    {
        Ok(f) => Operand::const_float(f, float_size),
        Err(_) => panic!("Internal Compiler Error: {} is not a valid floating point number", fstr)
    }
}


#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Instruction
{
    Store{dst: Var, src: Operand},
    Load{dst: Var, ptr: Var},
    LoadMember{dst: Var, obj: Var, member_index: Operand},
    StoreMember{obj: Var, member_index: Operand, src: Operand},
    AddressOf{dst: Var, obj: Var},
    AddressOfMember{dst: Var, obj: Var, member_index: Operand},
    GetProperty{dst: Var, obj: Var, prop: ByteCodeProperty},
    SetProperty{obj: Var, prop: ByteCodeProperty, val: usize},
    UnaryOp{dst: Var, op: UnaryOperator, src: Operand},
    BinaryOp{dst: Var, op: BinaryOperator, left: Operand, right: Operand},
    Call{dst: Option<Var>, func: String, args: Vec<Operand>},
    Slice{dst: Var, src: Var, start: Operand, len: Operand},
    Cast{dst: Var, src: Operand},
    LoadOptionalFlag{dst: Var, obj: Var},
    StoreNil(Var),
    StackAlloc(Var),
    HeapAlloc(Var),
    StartScope,
    EndScope,
    Return(Operand),
    ReturnVoid,
    Branch(BasicBlockRef),
    BranchIf{cond: Operand, on_true: BasicBlockRef, on_false: BasicBlockRef},
    Delete(Var),
    Exit,
}

pub fn store_instr(dst: &Var, src: &Var) -> Instruction
{
    Instruction::Store{
        dst: dst.clone(),
        src: Operand::Var(src.clone()),
    }
}

pub fn store_operand_instr(dst: &Var, op: Operand) -> Instruction
{
    Instruction::Store{
        dst: dst.clone(),
        src: op,
    }
}

pub fn store_func_instr(dst: &Var, func: &str) -> Instruction
{
    Instruction::Store{
        dst: dst.clone(),
        src: Operand::Func(func.into()),
    }
}

pub fn load_instr(dst: &Var, ptr: &Var) -> Instruction
{
    Instruction::Load{
        dst: dst.clone(),
        ptr: ptr.clone()
    }
}

pub fn load_member_instr_with_var(dst: &Var, obj: &Var, member_index: &Var) -> Instruction
{
    Instruction::LoadMember{
        dst: dst.clone(),
        obj: obj.clone(),
        member_index: Operand::Var(member_index.clone()),
    }
}

pub fn load_member_instr(dst: &Var, obj: &Var, member_index: usize) -> Instruction
{
    Instruction::LoadMember{
        dst: dst.clone(),
        obj: obj.clone(),
        member_index: Operand::const_uint(member_index as u64, native_int_size()),
    }
}

pub fn store_member_instr(obj: &Var, member_index: usize, src: Var) -> Instruction
{
    Instruction::StoreMember{
        obj: obj.clone(),
        member_index: Operand::const_uint(member_index as u64, native_int_size()),
        src: Operand::Var(src),
    }
}

pub fn address_of_instr(dst: &Var, obj: &Var) -> Instruction
{
    Instruction::AddressOf{
        dst: dst.clone(),
        obj: obj.clone(),
    }
}

pub fn address_of_member_instr(dst: &Var, obj: &Var, member_index: usize) -> Instruction
{
    Instruction::AddressOfMember{
        dst: dst.clone(),
        obj: obj.clone(),
        member_index: Operand::const_uint(member_index as u64, native_int_size()),
    }
}


pub fn ret_instr(var: &Var) -> Instruction
{
    Instruction::Return(var_op(var))
}

pub fn unary_op_instr(dst: &Var, op: UnaryOperator, src: Operand) -> Instruction
{
    Instruction::UnaryOp{
        dst: dst.clone(),
        op: op,
        src: src,
    }
}

pub fn binary_op_instr(dst: &Var, op: BinaryOperator, left: Operand, right: Operand) -> Instruction
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
        cond: var_op(cond),
        on_true: on_true,
        on_false: on_false,
    }
}

pub fn call_instr(dst: &Var, func: &str, args: Vec<Operand>) -> Instruction
{
    Instruction::Call{
        dst: Some(dst.clone()),
        func: func.into(),
        args: args
    }
}

pub fn void_call_instr(func: &str, args: Vec<Operand>) -> Instruction
{
    Instruction::Call{
        dst: None,
        func: func.into(),
        args: args
    }
}

pub fn set_prop_instr(obj: &Var, prop: ByteCodeProperty, value: usize) -> Instruction
{
    Instruction::SetProperty{
        obj: obj.clone(),
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

pub fn slice_instr(dst: &Var, src: &Var, start: Operand, len: Operand) -> Instruction
{
    Instruction::Slice{
        dst: dst.clone(),
        src: src.clone(),
        start: start,
        len: len,
    }
}

pub fn cast_instr(dst: &Var, src: &Var) -> Instruction
{
    Instruction::Cast{
        dst: dst.clone(),
        src: var_op(src),
    }
}

pub fn load_optional_flag_instr(dst: &Var, obj: &Var) -> Instruction
{
    Instruction::LoadOptionalFlag{
        dst: dst.clone(),
        obj: obj.clone()
    }
}

impl fmt::Display for Instruction
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error>
    {
        match *self
        {
            Instruction::Exit => {
                writeln!(f, "  exit")
            },

            Instruction::Store{ref dst, ref src} => {
                writeln!(f, "  store {} {}", dst, src)
            },

            Instruction::Load{ref dst, ref ptr} => {
                writeln!(f, "  load {} {}", dst, ptr)
            },

            Instruction::LoadMember{ref dst, ref obj, ref member_index} => {
                writeln!(f, "  loadm {} {}.{}", dst, obj, member_index)
            },

            Instruction::StoreMember{ref obj, ref member_index, ref src} => {
                writeln!(f, "  storem {}.{} {}", obj, member_index, src)
            },

            Instruction::AddressOf{ref dst, ref obj} => {
                writeln!(f, "  addr {} {}", dst, obj)
            },

            Instruction::AddressOfMember{ref dst, ref obj, ref member_index} => {
                writeln!(f, "  addrm {} {}.{}", dst, obj, member_index)
            },

            Instruction::GetProperty{ref dst, ref obj, ref prop} => {
                writeln!(f, "  getp {} {}.{}", dst, obj, prop)
            },

            Instruction::SetProperty{ref obj, ref prop, ref val} => {
                writeln!(f, "  setp {} {} {}", obj, prop, val)
            },

            Instruction::UnaryOp{ref dst, ref op, ref src} => {
                writeln!(f, "  uop {} {} {}", dst, op, src)
            },

            Instruction::BinaryOp{ref dst, ref op, ref left, ref right} => {
                writeln!(f, "  bop {} {} {} {}", dst, op, left, right)
            },

            Instruction::Call{ref dst, ref func, ref args} => {
                match *dst {
                    Some(ref dst) => writeln!(f, "  call {} {} {}", dst, func, join(args.iter(), " ")),
                    None => writeln!(f, "  call {} {}", func, join(args.iter(), " ")),
                }
            },

            Instruction::Cast{ref dst, ref src} => {
                writeln!(f, "  cast {} {}", dst, src)
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

            Instruction::LoadOptionalFlag{ref dst, ref obj} => {
                writeln!(f, "  loadoptf {} {}", dst, obj)
            }

            Instruction::StoreNil(ref v) => {
                writeln!(f, "  storenil {}", v)
            }
        }
    }
}
