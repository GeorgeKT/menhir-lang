use llvm::prelude::*;
use llvm::core::*;

use codegen::cstr;

#[derive(Debug, Clone)]
pub struct ValueRef
{
    ptr: LLVMValueRef,
    constant: bool,
    builder: LLVMBuilderRef,
}

impl ValueRef
{
    pub fn new(ptr: LLVMValueRef, constant: bool, builder: LLVMBuilderRef) -> ValueRef
    {
        ValueRef{
            ptr: ptr,
            constant: constant,
            builder: builder,
        }
    }

    pub fn load(&self) -> LLVMValueRef
    {
        unsafe {
            LLVMBuildLoad(self.builder, self.ptr, cstr("load"))   
        }
    }

    pub fn get(&self) -> LLVMValueRef
    {
        self.ptr
    }
}