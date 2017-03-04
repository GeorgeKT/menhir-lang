use std::rc::Rc;
use std::ops::Deref;
use llvm::core::*;
use llvm::prelude::*;

use libcobra::ast::*;
use libcobra::bytecode::ByteCodeProperty;
use super::instructions::{const_uint, const_int};

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
    Void(LLVMValueRef),
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
    pub fn new(value: LLVMValueRef, typ: &Type, allocated: bool) -> ValueRef
    {
        match *typ
        {
            Type::Void => ValueRef::Void(value),

            Type::Enum(_) |
            Type::Int |
            Type::UInt |
            Type::Float |
            Type::Char |
            Type::Bool => {
                if allocated {
                    ValueRef::Ptr(value, Rc::new(typ.clone()))
                } else {
                    ValueRef::Const(value, typ.clone())
                }
            }

            Type::Pointer(ref inner) => {
                if allocated {
                    ValueRef::Ptr(value, Rc::new(inner.deref().clone()))
                } else {
                    ValueRef::Ptr(value, inner.clone())
                }
            }

            Type::Array(ref at) => {
                ValueRef::Array(ArrayValue {
                    value: value,
                    array_type: at.clone(),
                })
            }

            Type::Slice(ref st) => {
                ValueRef::Slice(SliceValue{
                    value: value,
                    slice_type: st.clone(),
                })
            }

            Type::Struct(ref st) => {
                ValueRef::Struct(StructValue {
                    value: value,
                    struct_type: st.clone(),
                })
            }

            Type::Sum(ref st) => {
                ValueRef::Sum(SumValue{
                    value: value,
                    sum_type: st.clone(),
                })
            }

            Type::Optional(ref ot) => {
                ValueRef::Optional(OptionalValue{
                    value: value,
                    inner_type: ot.clone(),
                })
            }

            Type::String => ValueRef::String(value),
            Type::Func(_) => ValueRef::Func(value),

            Type::Nil | Type::Unknown | Type::Generic(_) | Type::Unresolved(_) | Type::SelfType |
            Type::Interface(_) => panic!("Type {} not allowed at this point", typ),
        }
    }

    pub fn store(&self, builder: LLVMBuilderRef, vr: LLVMValueRef)
    {
        match *self
        {
            ValueRef::Ptr(v, _) => unsafe {
                LLVMBuildStore(builder, vr, v);
            },

            ValueRef::Void(_) => (),

            _ => panic!("Store not allowed"),
        }
    }

    pub fn load(&self, builder: LLVMBuilderRef) -> LLVMValueRef
    {
        match *self
        {
            ValueRef::Ptr(v, _) => unsafe {
                LLVMBuildLoad(builder, v, cstr!("load"))
            },

            ValueRef::Const(v, _) => v,
            ValueRef::Void(v) => v,
            _ => panic!("Load not allowed"),
        }
    }

    pub fn get_member_ptr(&self, ctx: LLVMContextRef, builder: LLVMBuilderRef, index: LLVMValueRef) -> LLVMValueRef
    {
        match *self
        {
            ValueRef::Array(ref av) => unsafe {
                let mut indices = vec![const_int(ctx, 0), index];
                LLVMBuildGEP(builder, av.value, indices.as_mut_ptr(), 2, cstr!("member"))
            },

            _ => panic!("Load member not allowed"),
        }
    }

    pub fn store_member(&self, ctx: LLVMContextRef, builder: LLVMBuilderRef, index: LLVMValueRef, value: LLVMValueRef)
    {
        match *self
        {
            ValueRef::Array(ref av) => unsafe {
                let mut indices = vec![const_int(ctx, 0), index];
                let member = LLVMBuildGEP(builder, av.value, indices.as_mut_ptr(), 2, cstr!("member"));
                LLVMBuildStore(builder, value, member);
            },

            _ => panic!("Load member not allowed"),
        }
    }


    pub fn get_property(&self, ctx: LLVMContextRef, prop: ByteCodeProperty) -> LLVMValueRef
    {
        match (self, prop)
        {
            (&ValueRef::Array(ref av), ByteCodeProperty::Len) => unsafe {
                const_uint(ctx, av.array_type.len)
            },

            _ => panic!("Get property not allowed")
        }
    }
}
