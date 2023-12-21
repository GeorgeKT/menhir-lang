use std::fmt;

use itertools::join;

use crate::ast::{array_type, ptr_type, BinaryOperator, FloatSize, IntSize, Type, UnaryOperator};

#[derive(Debug, Clone, Copy)]
pub enum ByteCodeProperty {
    Len,
    Data,
    SumTypeIndex,
}

impl fmt::Display for ByteCodeProperty {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match *self {
            ByteCodeProperty::Data => write!(f, "data"),
            ByteCodeProperty::Len => write!(f, "len"),
            ByteCodeProperty::SumTypeIndex => write!(f, "sum_type_index"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Constant {
    Int(i64, IntSize),
    UInt(u64, IntSize),
    Float(f64, FloatSize),
    Char(char),
    String(String),
    Bool(bool),
    Array(Vec<Constant>),
    NullPtr(Type),
}

impl Constant {
    pub fn get_type(&self) -> Type {
        match *self {
            Constant::Int(_, int_size) => Type::Int(int_size),
            Constant::UInt(_, int_size) => Type::UInt(int_size),
            Constant::Float(_, float_size) => Type::Float(float_size),
            Constant::Char(_) => Type::Char,
            Constant::String(_) => Type::String,
            Constant::Bool(_) => Type::Bool,
            Constant::NullPtr(ref typ) => ptr_type(typ.clone()),
            Constant::Array(ref members) => array_type(members[0].get_type(), members.len()),
        }
    }
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match *self {
            Constant::Int(v, int_size) => write!(f, "(int{} {})", int_size, v),
            Constant::UInt(v, int_size) => write!(f, "(uint{} {})", int_size, v),
            Constant::Float(v, float_size) => write!(f, "(float{} {})", float_size, v),
            Constant::Char(v) => write!(f, "(char {})", v),
            Constant::String(ref v) => write!(f, "(string \"{}\")", v),
            Constant::Bool(v) => write!(f, "(bool {})", v),
            Constant::Array(ref m) => write!(f, "[{}]", join(m.iter(), ", ")),
            Constant::NullPtr(_) => write!(f, "null"),
        }
    }
}

#[derive(Debug)]
pub struct CallArg {
    pub arg: Operand,
    pub mutable: bool,
    pub arg_type: Type,
}

pub fn call_arg(arg: Operand, mutable: bool, typ: Type) -> CallArg {
    CallArg {
        arg,
        mutable,
        arg_type: typ,
    }
}

#[derive(Debug)]
pub enum Operand {
    Var {
        name: String,
        typ: Type,
    },
    Constant {
        value: Constant,
    },
    Member {
        obj: Box<Operand>,
        idx: Box<Operand>,
        typ: Type,
    },
    MemberPtr {
        obj: Box<Operand>,
        idx: Box<Operand>,
        typ: Type,
    },
    AddressOf {
        obj: Box<Operand>,
    },
    Binary {
        op: BinaryOperator,
        left: Box<Operand>,
        right: Box<Operand>,
        typ: Type,
    },
    Unary {
        op: UnaryOperator,
        inner: Box<Operand>,
        typ: Type,
    },
    Struct {
        members: Vec<Operand>,
        typ: Type,
    },
    Array {
        members: Vec<Operand>,
        typ: Type,
    },
    Sum {
        variant: usize,
        inner: Option<Box<Operand>>,
        typ: Type,
    },
    Enum {
        variant: usize,
        typ: Type,
    },
    Dereference {
        inner: Box<Operand>,
        typ: Type,
    },
    SizeOf {
        typ: Type,
        int_size: IntSize,
    },
    Func {
        name: String,
        typ: Type,
    },
    Call {
        callee: Box<Operand>,
        args: Vec<CallArg>,
        typ: Type,
        rvo: bool,
    },
    Property {
        operand: Box<Operand>,
        property: ByteCodeProperty,
        typ: Type,
    },
    Slice {
        array: Box<Operand>,
        range: Box<Operand>,
        typ: Type,
    },
    Result {
        ok: bool,
        inner: Box<Operand>,
        typ: Type,
    },
    Optional {
        inner: Option<Box<Operand>>,
        typ: Type,
    },
    Cast {
        inner: Box<Operand>,
        typ: Type,
    },
    Null {
        typ: Type,
    },
    New {
        inner: Box<Operand>,
        typ: Type,
    },
    Range {
        start: Box<Operand>,
        end: Box<Operand>,
        typ: Type,
    },
    Void,
}

impl Operand {
    pub fn safe_clone(&self) -> Operand {
        match self {
            Operand::Var { name, typ } => Operand::Var {
                name: name.clone(),
                typ: typ.clone(),
            },
            Operand::Constant { value } => Operand::Constant { value: value.clone() },
            _ => panic!("ICE: only var and constant operands can be cloned"),
        }
    }

    pub fn cloneable(&self) -> bool {
        matches!(self, Operand::Var { .. } | Operand::Constant { .. })
    }

    pub fn const_int(v: i64, int_size: IntSize) -> Operand {
        Operand::Constant {
            value: Constant::Int(v, int_size),
        }
    }

    pub fn const_uint(v: u64, int_size: IntSize) -> Operand {
        Operand::Constant {
            value: Constant::UInt(v, int_size),
        }
    }

    pub fn const_float(v: f64, float_size: FloatSize) -> Operand {
        Operand::Constant {
            value: Constant::Float(v, float_size),
        }
    }

    pub fn const_bool(v: bool) -> Operand {
        Operand::Constant {
            value: Constant::Bool(v),
        }
    }

    pub fn const_char(v: char) -> Operand {
        Operand::Constant {
            value: Constant::Char(v),
        }
    }

    pub fn const_string<S: Into<String>>(s: S) -> Operand {
        Operand::Constant {
            value: Constant::String(s.into()),
        }
    }

    pub fn sti(op: Operand, int_size: IntSize) -> Operand {
        Operand::Property {
            operand: Box::new(op),
            property: ByteCodeProperty::SumTypeIndex,
            typ: Type::UInt(int_size),
        }
    }

    pub fn len(op: Operand, int_size: IntSize) -> Operand {
        Operand::Property {
            operand: Box::new(op),
            property: ByteCodeProperty::Len,
            typ: Type::UInt(int_size),
        }
    }

    pub fn binary(op: BinaryOperator, left: Operand, right: Operand, typ: Type) -> Operand {
        Operand::Binary {
            op,
            left: Box::new(left),
            right: Box::new(right),
            typ,
        }
    }

    pub fn member(obj: Operand, idx: Operand, typ: Type) -> Operand {
        Operand::Member {
            obj: Box::new(obj),
            idx: Box::new(idx),
            typ,
        }
    }

    pub fn member_ptr(obj: Operand, idx: Operand, typ: Type) -> Operand {
        Operand::MemberPtr {
            obj: Box::new(obj),
            idx: Box::new(idx),
            typ,
        }
    }

    pub fn slice(array: Operand, range: Operand, typ: Type) -> Operand {
        Operand::Slice {
            array: Box::new(array),
            range: Box::new(range),
            typ,
        }
    }

    pub fn is_call(&self) -> bool {
        if let Operand::Call { .. } = self {
            true
        } else {
            false
        }
    }

    pub fn get_type(&self) -> Type {
        match self {
            Operand::Var { typ, .. } => typ.clone(),
            Operand::Constant { value } => value.get_type(),
            Operand::Member { typ, .. } => typ.clone(),
            Operand::MemberPtr { typ, .. } => typ.clone(),
            Operand::AddressOf { obj } => ptr_type(obj.get_type()),
            Operand::Binary { typ, .. } => typ.clone(),
            Operand::Unary { typ, .. } => typ.clone(),
            Operand::Struct { typ, .. } => typ.clone(),
            Operand::Array { typ, .. } => typ.clone(),
            Operand::Sum { typ, .. } => typ.clone(),
            Operand::Enum { typ, .. } => typ.clone(),
            Operand::Dereference { typ, .. } => typ.clone(),
            Operand::SizeOf { int_size, .. } => Type::UInt(*int_size),
            Operand::Func { typ, .. } => typ.clone(),
            Operand::Call { typ, .. } => typ.clone(),
            Operand::Property { typ, .. } => typ.clone(),
            Operand::Slice { typ, .. } => typ.clone(),
            Operand::Result { typ, .. } => typ.clone(),
            Operand::Optional { typ, .. } => typ.clone(),
            Operand::Cast { typ, .. } => typ.clone(),
            Operand::Null { typ } => typ.clone(),
            Operand::New { typ, .. } => typ.clone(),
            Operand::Void => Type::Void,
            Operand::Range { typ, .. } => typ.clone(),
        }
    }

    pub fn visit<Func>(&self, f: &mut Func)
    where
        Func: FnMut(&Operand),
    {
        f(self);
        match self {
            Operand::Member { obj, idx, .. } => {
                obj.visit(f);
                idx.visit(f);
            }
            Operand::MemberPtr { obj, idx, .. } => {
                obj.visit(f);
                idx.visit(f);
            }
            Operand::AddressOf { obj } => obj.visit(f),
            Operand::Binary { left, right, .. } => {
                left.visit(f);
                right.visit(f);
            }
            Operand::Unary { inner, .. } => inner.visit(f),
            Operand::Struct { members, .. } => {
                for m in members {
                    m.visit(f);
                }
            }
            Operand::Array { members, .. } => {
                for m in members {
                    m.visit(f);
                }
            }
            Operand::Sum { inner: Some(i), .. } => i.visit(f),
            Operand::Dereference { inner, .. } => inner.visit(f),
            Operand::Call { callee, args, .. } => {
                callee.visit(f);
                for a in args {
                    a.arg.visit(f);
                }
            }
            Operand::Property { operand, .. } => operand.visit(f),
            Operand::Slice { array, range, .. } => {
                array.visit(f);
                range.visit(f);
            }
            Operand::Result { inner, .. } => inner.visit(f),
            Operand::Optional { inner: Some(i), .. } => i.visit(f),
            Operand::Cast { inner, .. } => inner.visit(f),
            Operand::New { inner, .. } => inner.visit(f),
            Operand::Range { start, end, .. } => {
                start.visit(f);
                end.visit(f);
            }
            Operand::Var { .. }
            | Operand::Constant { .. }
            | Operand::Enum { .. }
            | Operand::SizeOf { .. }
            | Operand::Func { .. }
            | Operand::Null { .. }
            | Operand::Sum { .. }
            | Operand::Optional { .. }
            | Operand::Void => (),
        }
    }
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Operand::Var { name, typ } => write!(f, "({name}: {typ})"),
            Operand::Constant { value } => write!(f, "{value}"),
            Operand::Member { obj, idx, .. } => write!(f, "{obj}.{idx}"),
            Operand::MemberPtr { obj, idx, .. } => write!(f, "&{obj}.{idx}"),
            Operand::AddressOf { obj } => write!(f, "&{obj}"),
            Operand::Binary { op, left, right, .. } => write!(f, "(bop {op} {left} {right})"),
            Operand::Unary { op, inner, .. } => write!(f, "(uop {op} {inner})"),
            Operand::Struct { members, .. } => {
                write!(f, "{{")?;
                for (idx, m) in members.iter().enumerate() {
                    write!(f, "{m}")?;
                    if idx != members.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "}}")
            }
            Operand::Array { members, .. } => {
                write!(f, "[")?;
                for (idx, m) in members.iter().enumerate() {
                    write!(f, "{m}")?;
                    if idx != members.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "]")
            }
            Operand::Sum { variant, inner, .. } => {
                if let Some(i) = inner {
                    write!(f, "(sum {variant} {i})")
                } else {
                    write!(f, "(sum {variant})")
                }
            }
            Operand::Enum { variant, .. } => write!(f, "(enum {variant})"),
            Operand::Dereference { inner, .. } => write!(f, "*{inner}"),
            Operand::SizeOf { typ, .. } => write!(f, "sizeof({typ})"),
            Operand::Func { name, .. } => write!(f, "(func {name})"),
            Operand::Call { callee, args, .. } => {
                write!(f, "(call {callee}(")?;
                for (idx, m) in args.iter().enumerate() {
                    write!(f, "{}", m.arg)?;
                    if idx != args.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "))")
            }
            Operand::Property { operand, property, .. } => write!(f, "({operand}.{property})"),
            Operand::Slice { array, range, .. } => write!(f, "(slice {array}[{range}])"),
            Operand::Result { ok, inner, .. } => write!(f, "({} {})", (if *ok { "ok" } else { "error" }), inner),
            Operand::Optional { inner, .. } => {
                if let Some(op) = inner {
                    write!(f, "?{op}")
                } else {
                    write!(f, "nil")
                }
            }
            Operand::Cast { inner, typ } => {
                write!(f, "(cast {inner} {typ})")
            }
            Operand::Null { .. } => write!(f, "null"),
            Operand::New { inner, .. } => write!(f, "(new {inner})"),
            Operand::Void => write!(f, "void"),
            Operand::Range { start, end, .. } => {
                write!(f, "({start}..{end})")?;
                Ok(())
            }
        }
    }
}
