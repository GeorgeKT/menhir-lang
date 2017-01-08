use std::fmt;
use std::rc::Rc;
use itertools::free::join;
use ast::Type;
use super::*;

#[derive(Debug, Clone)]
pub enum Value
{
    Uninitialized,
    Void,
    Int(i64),
    UInt(u64),
    Float(f64),
    Char(char),
    Bool(bool),
    String(String),
    Array(Vec<ValueRef>),
    Slice(Vec<ValueRef>),
    Func(Rc<ByteCodeFunction>),
    Struct(Vec<ValueRef>),
    Sum(usize, Box<ValueRef>),
    Enum(usize),
    Pointer(ValueRef),
}

impl fmt::Display for Value
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error>
    {
        match *self
        {
            Value::Uninitialized => write!(f, "uninitialized"),
            Value::Void => write!(f, "void"),
            Value::Int(v) => write!(f, "int {}", v),
            Value::UInt(v) => write!(f, "uint {}", v),
            Value::Float(v) => write!(f, "float {}", v),
            Value::Char(v) => write!(f, "char {}", v),
            Value::Bool(v) => write!(f, "bool {}", v),
            Value::Array(ref v) => write!(f, "[{}]", join(v.iter(), ", ")),
            Value::String(ref s) => write!(f, "\"{}\"", s),
            Value::Slice(ref v) => write!(f, "[{}]", join(v.iter(), ", ")),
            Value::Func(ref v) => write!(f, "func {}", v),
            Value::Struct(ref v) => write!(f, "{{{}}}", join(v.iter(), ", ")),
            Value::Sum(idx, ref v) => write!(f, "sum {} {}", idx, v),
            Value::Enum(idx) => write!(f, "enum {}", idx),
            Value::Pointer(ref v) => write!(f, "pointer {}", v),
        }
    }
}

impl Value
{
    pub fn to_exit_code(&self) -> i32
    {
        match *self
        {
            Value::Int(v) => v as i32,
            Value::Bool(v) => if v == true {0i32} else {1i32},
            _ => 1i32,
        }
    }

    pub fn from_literal(lit: &ByteCodeLiteral) -> Result<Value, ExecutionError>
    {
        match lit
        {
            &ByteCodeLiteral::Int(v) => Ok(Value::Int(v as i64)),
            &ByteCodeLiteral::Float(ref num) => {
                match num.parse::<f64>()
                {
                    Ok(f) => Ok(Value::Float(f)),
                    Err(_) => Err(ExecutionError(format!("{} is not a valid floating point number", num))),
                }
            },
            &ByteCodeLiteral::Char(v) => Ok(Value::Char(v as char)),
            &ByteCodeLiteral::String(ref s) => Ok(Value::String(s.clone())),
            &ByteCodeLiteral::Bool(v) => Ok(Value::Bool(v)),
        }
    }

    pub fn from_type(typ: &Type) -> Result<Value, ExecutionError>
    {
        match *typ
        {
            Type::Unknown | Type::Unresolved(_) | Type::Generic(_) => panic!("Types must be known before the interpreter can run"),
            Type::Void => Ok(Value::Void),
            Type::Int => Ok(Value::Int(0)),
            Type::UInt => Ok(Value::UInt(0)),
            Type::Float => Ok(Value::Float(0.0)),
            Type::Char => Ok(Value::Char('x')),
            Type::Bool => Ok(Value::Bool(false)),
            Type::String => Ok(Value::String(String::default())),
            Type::Array(ref at) => {
                let mut array = Vec::with_capacity(at.len);
                for _ in 0..at.len {
                    let element = Value::from_type(&at.element_type)?;
                    array.push(ValueRef::new(element));
                }
                Ok(Value::Array(array))
            },
            Type::Slice(_) => Ok(Value::Slice(Vec::new())),
            Type::Func(_) => Ok(Value::Void), // Use void, seeing that we can't fill in the function pointer yet
            Type::Struct(ref st) => {
                let mut members = Vec::new();
                for m in &st.members {
                    let member = Value::from_type(&m.typ)?;
                    members.push(ValueRef::new(member));
                }
                Ok(Value::Struct(members))
            },
            Type::Enum(_) => Ok(Value::Enum(0)),
            Type::Sum(ref st) => {
                let first_case = Value::from_type(&st.cases[0].typ)?;
                Ok(Value::Sum(0, Box::new(ValueRef::new(first_case))))
            },
            Type::Pointer(ref inner) => {
                let inner = Value::from_type(inner)?;
                Ok(Value::Pointer(ValueRef::new(inner)))
            },

        }
    }

    pub fn get_property(&self, prop: ByteCodeProperty) -> Result<Value, ExecutionError>
    {
        match (self, prop)
        {
            (&Value::Array(ref a), ByteCodeProperty::Len) => Ok(Value::Int(a.len() as i64)),
            (&Value::Slice(ref a), ByteCodeProperty::Len) => Ok(Value::Int(a.len() as i64)),
            (&Value::Sum(idx, _), ByteCodeProperty::SumTypeIndex) => Ok(Value::Int(idx as i64)),
            (&Value::Enum(idx), ByteCodeProperty::SumTypeIndex) => Ok(Value::Int(idx as i64)),
            _  => Err(ExecutionError(format!("Unknown property {}", prop))),
        }
    }
}
