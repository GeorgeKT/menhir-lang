use std::rc::Rc;
use llvm::core::*;
use llvm::prelude::*;

use libcobra::ast::*;

pub struct ArrayValue
{
    value: LLVMValueRef,
    array_type: Rc<ArrayType>,
}

pub struct SliceValue
{
    value: LLVMValueRef,
    slice_type: Rc<SliceType>,
}

pub struct StructValue
{
    value: LLVMValueRef,
    struct_type: Rc<StructType>,
}

pub struct SumValue
{
    value: LLVMValueRef,
    sum_type: Rc<SumType>,
}

pub struct OptionalValue
{
    value: LLVMValueRef,
    inner_type: Rc<Type>,
}

pub enum ValueRef
{
    Const(LLVMValueRef, Type),
    Ptr(LLVMValueRef, Rc<Type>),
    Array(ArrayValue),
    Slice(SliceValue),
    String(LLVMValueRef),
    Struct(StructValue),
    Sum(SumValue),
    Optional(OptionalValue),
    Func(LLVMValueRef),
}


impl ValueRef
{
    pub fn new(value: LLVMValueRef, typ: &Type) -> ValueRef
    {
        match *typ
        {
            Type::Enum(_) | Type::Void | Type::Int | Type::UInt | Type::Float |
            Type::Char | Type::Bool => ValueRef::Const(value, typ.clone()),

            Type::Pointer(ref inner) => ValueRef::Ptr(value, inner.clone()),

            Type::Array(ref at) =>
                ValueRef::Array(ArrayValue{
                    value: value,
                    array_type: at.clone(),
                }),

            Type::Slice(ref st) =>
                ValueRef::Slice(SliceValue{
                    value: value,
                    slice_type: st.clone(),
                }),

            Type::Struct(ref st) =>
                ValueRef::Struct(StructValue{
                    value: value,
                    struct_type: st.clone(),
                }),

            Type::Sum(ref st) =>
                ValueRef::Sum(SumValue{
                    value: value,
                    sum_type: st.clone(),
                }),

            Type::Optional(ref ot) =>
                ValueRef::Optional(OptionalValue{
                    value: value,
                    inner_type: ot.clone(),
                }),

            Type::String => ValueRef::String(value),
            Type::Func(_) => ValueRef::Func(value),

            Type::Nil | Type::Unknown | Type::Generic(_) | Type::Unresolved(_) | Type::SelfType |
            Type::Interface(_) => panic!("Type {} not allowed at this point", typ),
        }
    }
}
