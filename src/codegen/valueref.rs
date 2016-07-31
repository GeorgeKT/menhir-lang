use llvm::prelude::*;
use llvm::core::*;
use llvm::*;

use codegen::cstr;
use codegen::context::Context;
use compileerror::{Pos, CompileResult, ErrorCode, err};

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

    pub fn local(builder: LLVMBuilderRef, typ: LLVMTypeRef) -> ValueRef
    {
        unsafe {
            ValueRef{
                ptr: LLVMBuildAlloca(builder, typ, cstr("alloc")),
                constant: false,
                builder: builder,
            }
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

    pub unsafe fn store(&self, ctx: &Context, val: ValueRef, pos: Pos) -> CompileResult<ValueRef>
    {
        if !self.is_pointer() {
            return err(pos, ErrorCode::CodegenError, format!("Store must be called on pointer types"));
        }
        
        if ctx.in_global_context()
        {
            if val.is_constant_value()
            {
                // We need to initialize globals when we are in the global context
                LLVMSetInitializer(self.ptr, val.get());
                Ok(val)
            }
            else
            {
                return err(pos, ErrorCode::ExpectedConstExpr,
                    format!("Global variables and constants must be initialized with a constant expression"));
            }
        }
        else
        {
            if self.constant {
                return err(pos, ErrorCode::ConstantModification, format!("Attempting to modify a constant"));
            }

            Ok(ValueRef::new(LLVMBuildStore(self.builder, val.load(), self.ptr), self.constant, self.builder))
        }
    }

    pub fn get_value_type(&self) -> LLVMTypeRef
    {
        unsafe{LLVMTypeOf(self.ptr)}
    }

    pub fn get_element_type(&self) -> LLVMTypeRef
    {
        unsafe{
            if self.is_pointer() {
                LLVMGetElementType(LLVMTypeOf(self.ptr))
            } else {
                LLVMTypeOf(self.ptr)
            }
        }
    }

    pub fn is_constant_value(&self) -> bool
    {
        unsafe {
            LLVMIsConstant(self.ptr) != 0
        }
    }

    pub fn is_pointer(&self) -> bool
    {
        unsafe {
            is_pointer(LLVMTypeOf(self.ptr))
        }
    }
}