use std::os::raw::c_uint;
use llvm::prelude::*;
use llvm::core::*;

use ast::Type;
use codegen::{Context, ValueRef};

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

    pub unsafe fn get_member_ptr(&self, ctx: &Context, idx: usize) -> ValueRef
    {
        let member = LLVMBuildStructGEP(ctx.builder, self.value, idx as c_uint, cstr!("member"));
        ValueRef::new(member, &self.member_types[idx])
    }

    pub fn get(&self) -> LLVMValueRef
    {
        self.value
    }
}
