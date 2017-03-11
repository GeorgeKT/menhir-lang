use llvm::core::*;
use llvm::prelude::*;

use ast::*;
use bytecode::ByteCodeProperty;
use super::context::Context;
use super::instructions::{const_uint, const_int, copy};
use super::function::pass_by_value;

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
        if pass_by_value(&self.typ) {
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

    pub fn get_member_ptr(&self, ctx: LLVMContextRef, builder: LLVMBuilderRef, index: LLVMValueRef) -> LLVMValueRef
    {
        match self.typ
        {
            Type::Array(_) | Type::Struct(_) | Type::Slice(_) => unsafe {
                let mut indices = vec![const_int(ctx, 0), index];
                LLVMBuildGEP(builder, self.value, indices.as_mut_ptr(), 2, cstr!("member"))
            },

            _ => panic!("Load member not allowed"),
        }
    }

    pub fn store_member(&self, ctx: LLVMContextRef, builder: LLVMBuilderRef, index: LLVMValueRef, value: LLVMValueRef)
    {
        match self.typ
        {
            Type::Array(_) | Type::Struct(_) | Type::Slice(_) => unsafe {
                let mut indices = vec![const_int(ctx, 0), index];
                let member = LLVMBuildGEP(builder, self.value, indices.as_mut_ptr(), 2, cstr!("member"));
                LLVMBuildStore(builder, value, member);
            },

            _ => panic!("Load member not allowed"),
        }
    }


    pub fn get_property(&self, ctx: LLVMContextRef, prop: ByteCodeProperty) -> LLVMValueRef
    {
        match (&self.typ, prop)
        {
            (&Type::Array(ref a), ByteCodeProperty::Len) => unsafe {
                const_uint(ctx, a.len)
            },

            _ => panic!("Get property not allowed")
        }
    }
}
