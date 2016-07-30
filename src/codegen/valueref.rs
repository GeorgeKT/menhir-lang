use llvm::prelude::*;
use llvm::core::*;
use llvm::*;

use codegen::cstr;

#[derive(Debug, Clone)]
pub struct ValueRef
{
    ptr: LLVMValueRef,
    constant: bool,
    builder: LLVMBuilderRef,
}

pub fn is_same_kind(a: LLVMTypeKind, b: LLVMTypeKind) -> bool
{
    (a as usize) == (b as usize)
}

pub unsafe fn is_pointer(t: LLVMTypeRef) -> bool
{
    is_same_kind(LLVMGetTypeKind(t), LLVMTypeKind::LLVMPointerTypeKind)
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
            if is_pointer(LLVMTypeOf(self.ptr)) {
                LLVMBuildLoad(self.builder, self.ptr, cstr("load"))
            } else {
                self.ptr
            }            
        }
    }

    pub fn get(&self) -> LLVMValueRef
    {
        self.ptr
    }
}