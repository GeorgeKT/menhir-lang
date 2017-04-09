use libc::c_uint;
use llvm::core::*;
use llvm::prelude::*;

use ast::*;
use bytecode::{ByteCodeProperty, Operand};
use super::context::Context;
use super::instructions::{const_uint, const_int, const_bool, copy, get_operand};

#[derive(Clone)]
pub struct ValueRef
{
    pub value: LLVMValueRef,
    pub typ: Type,
}


impl ValueRef
{
    pub fn new(value: LLVMValueRef, typ: &Type, allocated: bool) -> ValueRef
    {
        match *typ
        {
            Type::Unknown | Type::Generic(_) | Type::Unresolved(_) | Type::SelfType |
            Type::Interface(_) => panic!("Type {} not allowed at this point", typ),

            Type::Enum(_) |
            Type::Int |
            Type::UInt |
            Type::Float |
            Type::Char |
            Type::Void |
            Type::Bool => ValueRef{
                value: value,
                typ: if allocated {
                    ptr_type(typ.clone())
                } else {
                    typ.clone()
                }
            },


            Type::Array(_) |
            Type::Slice(_) |
            Type::Struct(_) |
            Type::Sum(_) |
            Type::Func(_) |
            Type::Optional(_) |
            Type::String |
            Type::Pointer(_) => ValueRef {
                value: value,
                typ: typ.clone(),
            },
        }
    }

    pub unsafe fn store(&self, ctx: &Context, val: &ValueRef)
    {
        match self.typ
        {
            Type::Optional(ref inner) => {
                let is_nil_ptr = LLVMBuildStructGEP(ctx.builder, self.value, 0, cstr!("is_nil_ptr"));
                LLVMBuildStore(ctx.builder, const_bool(ctx, false), is_nil_ptr);
                let inner_ptr = LLVMBuildStructGEP(ctx.builder, self.value, 1, cstr!("inner_ptr"));
                if inner.pass_by_value() {
                    LLVMBuildStore(ctx.builder, val.load(ctx.builder), inner_ptr);
                } else {
                    copy(ctx, inner_ptr, val.value, ctx.resolve_type(inner))
                }
            },

            Type::Pointer(ref inner) => {
                if inner.pass_by_value() {
                    LLVMBuildStore(ctx.builder, val.load(ctx.builder), self.value);
                } else {
                    copy(ctx, self.value, val.value, ctx.resolve_type(inner))
                }
            }

            _ => {
                if self.typ.pass_by_value() {
                    LLVMBuildStore(ctx.builder, val.load(ctx.builder), self.value);
                } else {
                    copy(ctx, self.value, val.value, ctx.resolve_type(&self.typ))
                }
            }
        }

    }

    pub fn load(&self, builder: LLVMBuilderRef) -> LLVMValueRef
    {
        match self.typ
        {
            Type::Pointer(_) => unsafe {
                LLVMBuildLoad(builder, self.value, cstr!("load"))
            },

            Type::Optional(ref inner_type) => unsafe {
                let inner_ptr = LLVMBuildStructGEP(builder, self.value, 1, cstr!("inner_ptr"));
                if inner_type.pass_by_value() {
                    LLVMBuildLoad(builder, inner_ptr, cstr!("inner"))
                } else {
                    inner_ptr
                }
            },

            _ => self.value
        }
    }

    pub fn address_of(&self) -> ValueRef
    {
        match self.typ
        {
            Type::Array(_) |
            Type::Slice(_) |
            Type::Struct(_) |
            Type::Sum(_) |
            Type::Func(_) |
            Type::Optional(_) |
            Type::String |
            Type::Pointer(_) => self.clone(),

            _ => panic!("Address of not allowed on value of type {}", self.typ)
        }
    }

    pub fn is_nil(&self, ctx: &Context) -> ValueRef
    {
        match self.typ
        {
            Type::Optional(_) => unsafe {
                let is_nil_ptr = LLVMBuildStructGEP(ctx.builder, self.value, 0, cstr!("is_nil_ptr"));
                ValueRef::new(
                    LLVMBuildLoad(ctx.builder, is_nil_ptr, cstr!("is_nil")),
                    &Type::Bool,
                    false
                )
            },

            _ => unsafe{
                ValueRef::new(const_bool(ctx, false), &Type::Bool, false)
            }
        }
    }

    pub fn store_nil(&self, ctx: &Context)
    {
        match self.typ
        {
            Type::Optional(_) => unsafe {
                let is_nil_ptr = LLVMBuildStructGEP(ctx.builder, self.value, 0, cstr!("is_nil_ptr"));
                LLVMBuildStore(ctx.builder, const_bool(ctx, true), is_nil_ptr);
            },

            _ => panic!("storenil only allowed on optional type"),
        }
    }

    pub fn get_member_ptr(&self, ctx: &Context, index: &Operand) -> ValueRef
    {
        let zero_val = unsafe{const_int(ctx, 0)};
        match self.typ
        {
            Type::Array(ref at) => unsafe {
                let index = get_operand(ctx, index).load(ctx.builder);
                let mut indices = vec![zero_val, index];
                ValueRef::new(
                    LLVMBuildGEP(ctx.builder, self.value, indices.as_mut_ptr(), 2, cstr!("member")),
                    &at.element_type,
                    true
                )
            },

            Type::Slice(ref st) => unsafe {
                let index = get_operand(ctx, index).load(ctx.builder);
                let data_ptr = LLVMBuildLoad(ctx.builder, self.slice_data_ptr(ctx), cstr!("data_ptr"));
                let mut indices = vec![index];
                ValueRef::new(
                    LLVMBuildGEP(ctx.builder, data_ptr, indices.as_mut_ptr(), 1, cstr!("member")),
                    &st.element_type,
                    true
                )
            },

            Type::Struct(ref st) => unsafe {
                let index = match *index {
                    Operand::Int(v) => v as usize,
                    Operand::UInt(v) => v as usize,
                    _ => panic!("Struct member access has to be through an integer"),
                };

                ValueRef::new(
                    LLVMBuildStructGEP(ctx.builder, self.value, index as c_uint, cstr!("member")),
                    &st.members[index].typ,
                    true
                )
            },

            _ => panic!("Load member not allowed"),
        }
    }

    pub fn store_member(&self, ctx: &Context, index: &Operand, value: &ValueRef)
    {
        match self.typ
        {
            Type::Array(_) | Type::Struct(_) | Type::Slice(_) => unsafe {
                let member_ptr = self.get_member_ptr(ctx, index);
                member_ptr.store(ctx, value);
            },

            _ => panic!("Load member not allowed"),
        }
    }


    pub fn get_property(&self, ctx: &Context, prop: ByteCodeProperty) -> ValueRef
    {
        match (&self.typ, prop)
        {
            (&Type::Array(ref a), ByteCodeProperty::Len) => unsafe {
                ValueRef::new(
                    const_uint(ctx, a.len),
                    &Type::UInt,
                    false
                )
            },

            (&Type::Slice(_), ByteCodeProperty::Len) => unsafe {
                let len_ptr = self.slice_len_ptr(ctx);
                ValueRef::new(
                    LLVMBuildLoad(ctx.builder, len_ptr, cstr!("len")),
                    &Type::UInt,
                    false
                )
            },

            _ => panic!("Get property not allowed")
        }
    }

    unsafe fn slice_data_ptr(&self, ctx: &Context) -> LLVMValueRef
    {
        LLVMBuildStructGEP(ctx.builder, self.value, 0, cstr!("slice_data_ptr"))
    }

    unsafe fn slice_len_ptr(&self, ctx: &Context) -> LLVMValueRef
    {
        LLVMBuildStructGEP(ctx.builder, self.value, 1, cstr!("slice_len_ptr"))
    }

    pub unsafe fn create_slice(&self, ctx: &Context, array: &ValueRef, start: &Operand, len: &Operand)
    {
        match array.typ
        {
            Type::Array(_) | Type::Slice(_) => {
                let data_ptr = self.slice_data_ptr(ctx);
                let len_ptr = self.slice_len_ptr(ctx);
                let member_ptr = array.get_member_ptr(ctx, start);
                LLVMBuildStore(ctx.builder, member_ptr.value, data_ptr);
                LLVMBuildStore(ctx.builder, get_operand(ctx, len).load(ctx.builder), len_ptr);
            }

            _ =>  panic!("Expecting an array type, not a {}", self.typ),
        }
    }
}
