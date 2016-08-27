use std::os::raw::c_uint;
use llvm::prelude::*;
use llvm::core::*;

use codegen::{Context, ValueRef, cstr};

#[derive(Debug, Clone)]
pub struct StructValue
{
    value: LLVMValueRef,
    typ: LLVMTypeRef,
}

impl StructValue
{
    pub unsafe fn alloc(ctx: &Context, struct_type: LLVMTypeRef) -> StructValue
    {
        StructValue{
            value: ctx.alloc(struct_type, "array"),
            typ: struct_type,
        }
    }

    pub unsafe fn get_member_ptr(&self, ctx: &Context, idx: usize) -> ValueRef
    {
        ValueRef::Ptr(LLVMBuildStructGEP(ctx.builder, self.value, idx as c_uint, cstr("member")))
    }

    pub fn get(&self) -> LLVMValueRef
    {
        self.value
    }
}
