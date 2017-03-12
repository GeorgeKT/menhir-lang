use llvm::core::*;
use llvm::prelude::*;

use ast::*;
use bytecode::ByteCodeProperty;
use super::context::Context;
use super::instructions::{const_uint, const_int, copy};

pub struct ValueRef
{
    value: LLVMValueRef,
    typ: Type,
}


impl ValueRef
{
    pub fn new(value: LLVMValueRef, typ: &Type, allocated: bool) -> ValueRef
    {
        match *typ
        {
            Type::Nil | Type::Unknown | Type::Generic(_) | Type::Unresolved(_) | Type::SelfType |
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

    pub unsafe fn store(&self, ctx: &Context, vr: LLVMValueRef)
    {
        if self.typ.pass_by_value() {
            LLVMBuildStore(ctx.builder, vr, self.value);
        } else {
            copy(ctx, self.value, vr, ctx.resolve_type(&self.typ))
        }
    }

    pub fn load(&self, builder: LLVMBuilderRef) -> LLVMValueRef
    {
        match self.typ
        {
            Type::Pointer(_) => unsafe {
                LLVMBuildLoad(builder, self.value, cstr!("load"))
            },

            _ => self.value
        }
    }

    pub fn get_member_ptr(&self, ctx: &Context, index: LLVMValueRef) -> LLVMValueRef
    {
        let zero_val = unsafe{const_int(ctx, 0)};
        match self.typ
        {
            Type::Array(_) | Type::Struct(_) => unsafe {
                let mut indices = vec![zero_val, index];
                LLVMBuildGEP(ctx.builder, self.value, indices.as_mut_ptr(), 2, cstr!("member"))
            },

            Type::Slice(_) => unsafe {
                let data_ptr = LLVMBuildLoad(ctx.builder, self.slice_data_ptr(ctx), cstr!("data_ptr"));
                let mut indices = vec![index];
                LLVMBuildGEP(ctx.builder, data_ptr, indices.as_mut_ptr(), 1, cstr!("member"))
            },

            _ => panic!("Load member not allowed"),
        }
    }

    pub fn store_member(&self, ctx: &Context, index: LLVMValueRef, value: LLVMValueRef)
    {
        match self.typ
        {
            Type::Array(_) | Type::Struct(_) | Type::Slice(_) => unsafe {
                let member_ptr = self.get_member_ptr(ctx, index);
                LLVMBuildStore(ctx.builder, value, member_ptr);
            },

            _ => panic!("Load member not allowed"),
        }
    }


    pub fn get_property(&self, ctx: &Context, prop: ByteCodeProperty) -> LLVMValueRef
    {
        match (&self.typ, prop)
        {
            (&Type::Array(ref a), ByteCodeProperty::Len) => unsafe {
                const_uint(ctx, a.len)
            },

            (&Type::Slice(_), ByteCodeProperty::Len) => unsafe {
                let len_ptr = self.slice_len_ptr(ctx);
                LLVMBuildLoad(ctx.builder, len_ptr, cstr!("len"))
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

    pub unsafe fn slice_from_array(&self, ctx: &Context, array: LLVMValueRef, start: LLVMValueRef, len: LLVMValueRef)
    {
        let data_ptr = self.slice_data_ptr(ctx);
        let len_ptr = self.slice_len_ptr(ctx);
        let mut indices = vec![const_int(ctx, 0), start];
        let first_array_element_ptr = LLVMBuildGEP(ctx.builder, array, indices.as_mut_ptr(), 2, cstr!("first_element"));
        LLVMBuildStore(ctx.builder, first_array_element_ptr, data_ptr);
        LLVMBuildStore(ctx.builder, len, len_ptr);
    }
}
