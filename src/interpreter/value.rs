use std::fmt;
use itertools::free::join;
use ast::Type;
use super::ExecutionResult;
use super::valueref::ValueRef;
use bytecode::*;

#[derive(Debug, Clone)]
pub enum Value
{
    Uninitialized,
    Void,
    Nil,
    Int(isize),
    UInt(usize),
    Float(f64),
    Char(char),
    Bool(bool),
    String(String),
    Array(Vec<ValueRef>),
    Slice(Vec<ValueRef>),
    Func(String),
    Struct(Vec<ValueRef>),
    Sum(usize, Box<ValueRef>),
    Enum(usize),
    Pointer(ValueRef),
    Optional(ValueRef),
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
            Value::Optional(ref v) => write!(f, "optional {}", v),
            Value::Nil => write!(f, "nil"),
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
            Value::UInt(v) => v as i32,
            Value::Bool(v) => if v {0i32} else {1i32},
            _ => 1i32,
        }
    }

    pub fn from_const(cst: &Constant) -> Value
    {
        match *cst
        {
            Constant::Int(v) => Value::Int(v),
            Constant::UInt(v) => Value::UInt(v),
            Constant::Float(v) => Value::Float(v),
            Constant::Char(v) => Value::Char(v as char),
            Constant::String(ref s) => Value::String(s.clone()),
            Constant::Bool(v) => Value::Bool(v),
            Constant::Array(ref array_data) => {
                let mut array_data_values = Vec::with_capacity(array_data.len());
                for ad in array_data {
                    array_data_values.push(ValueRef::new(Value::from_const(ad)));
                }
                Value::Array(array_data_values)
            }
        }
    }

    pub fn from_type(typ: &Type) -> ExecutionResult<Value>
    {
        match *typ
        {
            Type::Unknown | Type::Unresolved(_) | Type::Generic(_) => panic!("Types must be known before the interpreter can run"),
            Type::Interface(_) => panic!("NYI"),
            Type::SelfType => panic!("Self type must be resolved at this point"),
            Type::Void |
            Type::Func(_) => Ok(Value::Void), // Use void, seeing that we can't fill in the function pointer yet
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
            Type::Optional(ref _inner) => {
                Ok(Value::Optional(ValueRef::new(Value::Nil)))
            },
        }
    }

    pub fn get_property(&self, prop: ByteCodeProperty) -> ExecutionResult<Value>
    {
        match (self, prop)
        {
            (&Value::Array(ref a), ByteCodeProperty::Len) |
            (&Value::Slice(ref a), ByteCodeProperty::Len) => Ok(Value::UInt(a.len())),
            (&Value::String(ref s), ByteCodeProperty::Len) => Ok(Value::UInt(s.len())),
            (&Value::Sum(idx, _), ByteCodeProperty::SumTypeIndex) |
            (&Value::Enum(idx), ByteCodeProperty::SumTypeIndex) => Ok(Value::UInt(idx)),
            _  => Err(format!("Unknown property {}", prop)),
        }
    }

    pub fn get_member_ptr(&self, member_index: usize) -> ExecutionResult<Value>
    {
        match *self
        {
            Value::Array(ref arr) => {
                if member_index < arr.len() {
                    Ok(Value::Pointer(arr[member_index].to_ptr()))
                } else {
                    Err(format!("Array index {} out of bounds", member_index))
                }
            },

            Value::Slice(ref slice) => {
                if member_index < slice.len() {
                    Ok(Value::Pointer(slice[member_index].to_ptr()))
                } else {
                    Err(format!("Slice index {} out of bounds", member_index))
                }
            },

            Value::Struct(ref members) => {
                if member_index < members.len() {
                    Ok(Value::Pointer(members[member_index].to_ptr()))
                } else {
                    Err(format!("Struct member index {} out of bounds", member_index))
                }
            },

            Value::Sum(idx, ref inner) => {
                if member_index == idx {
                    Ok(Value::Pointer(inner.to_ptr()))
                } else {
                    Err(format!("Wrong sum type index {}", member_index))
                }
            },

            Value::Pointer(ref inner) => {
                inner.apply(|v: &Value| {
                    v.get_member_ptr(member_index)
                })
            },

            _ => Err(format!("Load member not supported on {}", self))
        }
    }

    pub fn update_member(&mut self, member_index: usize, value: Value) -> ExecutionResult<()>
    {
        let mut target = match *self
        {
            Value::Array(ref mut arr) => {
                if member_index < arr.len() {
                    &mut arr[member_index]
                } else {
                    return Err(format!("Array index {} out of bounds", member_index))
                }
            },

            Value::Slice(ref mut slice) => {
                if member_index < slice.len() {
                    &mut slice[member_index]
                } else {
                    return Err(format!("Slice index {} out of bounds", member_index))
                }
            },

            Value::Struct(ref mut members) => {
                if member_index < members.len() {
                    &mut members[member_index]
                } else {
                    return Err(format!("Struct member index {} out of bounds", member_index))
                }
            },

            Value::Sum(idx, ref mut inner) => {
                if member_index == idx {
                    inner
                } else {
                    return Err(format!("Wrong sum type index {}", member_index))
                }
            },

            Value::Pointer(ref mut inner) => {
                return inner.apply_mut(|v: &mut Value| v.update_member(member_index, value));
            },

            _ => return Err(format!("Update member not supported on {}", self))
        };

        target.apply_mut(|v: &mut Value| {
            *v = value;
            Ok(())
        })
    }

    pub fn load(&self) -> ExecutionResult<Value>
    {
        if let Value::Pointer(ref inner) = *self {
            Ok(inner.clone_value()?)
        } else {
            Err(format!("Load can only happen on a pointer"))
        }
    }

    pub fn is_nil(&self) -> bool
    {
        if let Value::Nil = *self {
            true
        } else {
            false
        }
    }
}
