use std::os::raw::c_uint;
use std::ops::Deref;
use llvm::prelude::*;
use llvm::core::*;

use ast::Type;
use codegen::{Context, ValueRef, cstr};

#[derive(Debug, Clone)]
pub struct StructValue
{
    value: LLVMValueRef,
    member_types: Vec<Type>,
}

impl StructValue
{
    pub fn new(value: LLVMValueRef, member_types: Vec<Type>) -> StructValue
    {
        StructValue{
            value: value,
            member_types: member_types,
        }
    }

    pub unsafe fn alloc(ctx: &Context, llvm_type: LLVMTypeRef, member_types: Vec<Type>) -> StructValue
    {
        StructValue{
            value: ctx.alloc(llvm_type, "struct"),
            member_types: member_types,
        }
    }

    pub unsafe fn get_member_ptr(&self, ctx: &Context, idx: usize) -> ValueRef
    {
        let member = LLVMBuildStructGEP(ctx.builder, self.value, idx as c_uint, cstr("member"));
        match self.member_types[idx]
        {
            Type::Array(ref element_type, _) => ValueRef::array(member, element_type.deref().clone()),
            Type::Slice(ref element_type) => ValueRef::slice(member, element_type.deref().clone()),
            Type::Struct(_) => ValueRef::struct_value(member, self.member_types[idx].get_member_types()),
            _ => ValueRef::Ptr(member),
        }
    }

    pub fn get(&self) -> LLVMValueRef
    {
        self.value
    }
}
