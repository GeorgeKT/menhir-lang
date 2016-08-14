use llvm::prelude::*;
use llvm::core::*;
use llvm::*;

use codegen::cstr;
use codegen::context::Context;
use codegen::expressions::const_int;
use codegen::slice::Slice;
use compileerror::{Pos, CompileResult, ErrorCode, err};



#[derive(Debug, Clone)]
pub enum ValueRef
{
    Const(LLVMValueRef),
    Ptr(LLVMValueRef),
    Global(LLVMValueRef),
    Array(LLVMValueRef),
    Slice(Slice),
}

pub fn is_same_kind(a: LLVMTypeKind, b: LLVMTypeKind) -> bool
{
    (a as usize) == (b as usize)
}

/*
pub unsafe fn is_pointer(t: LLVMTypeRef) -> bool
{
    is_same_kind(LLVMGetTypeKind(t), LLVMTypeKind::LLVMPointerTypeKind)
}
*/

impl ValueRef
{
    pub fn const_value(v: LLVMValueRef) -> ValueRef
    {
        ValueRef::Const(v)
    }

    pub fn alloc(builder: LLVMBuilderRef, typ: LLVMTypeRef) -> ValueRef
    {
        unsafe {
            let alloc = LLVMBuildAlloca(builder, typ, cstr("alloc"));
            if is_same_kind(LLVMGetTypeKind(typ), LLVMTypeKind::LLVMArrayTypeKind) {
                ValueRef::Array(alloc)
            } else {
                ValueRef::Ptr(alloc)
            }
        }
    }

    pub fn ptr(ptr: LLVMValueRef) -> ValueRef
    {
        ValueRef::Ptr(ptr)
    }

    pub fn global(ptr: LLVMValueRef) -> ValueRef
    {
        ValueRef::Global(ptr)
    }

    pub fn const_array(arr: LLVMValueRef) -> ValueRef
    {
        ValueRef::Array(arr)
    }

    pub fn alloc_array(builder: LLVMBuilderRef, element_type: LLVMTypeRef, len: usize) -> ValueRef
    {
        unsafe {
            let typ = LLVMArrayType(element_type, len as u32);
            ValueRef::Array(LLVMBuildAlloca(builder, typ, cstr("array_alloc")))
        }
    }

    pub fn slice(slice: Slice) -> ValueRef
    {
        ValueRef::Slice(slice)
    }

    pub unsafe fn load(&self, builder: LLVMBuilderRef) -> LLVMValueRef
    {
        match *self
        {
            ValueRef::Const(cv) => cv,
            ValueRef::Ptr(av) => LLVMBuildLoad(builder, av, cstr("load")),
            ValueRef::Global(ptr) => LLVMBuildLoad(builder, ptr, cstr("load")),
            ValueRef::Array(arr) => arr,
            ValueRef::Slice(ref slice) => slice.get(),
        }
    }

    pub fn get(&self) -> LLVMValueRef
    {
        match *self
        {
            ValueRef::Const(cv) => cv,
            ValueRef::Ptr(av) => av,
            ValueRef::Global(ptr) => ptr,
            ValueRef::Array(arr) => arr,
            ValueRef::Slice(ref slice) => slice.get(),
        }
    }

    pub unsafe fn store_direct(&self, ctx: &Context, val: LLVMValueRef, pos: Pos) -> CompileResult<()>
    {
        match *self
        {
            ValueRef::Const(_) => {
                err(pos, ErrorCode::CodegenError, format!("Store cannot be called on a const value"))
            },
            ValueRef::Array(_) => {
                err(pos, ErrorCode::CodegenError, format!("Cannot store an array"))
            },
            ValueRef::Ptr(av) => {
                LLVMBuildStore(ctx.builder, val, av);
                Ok(())
            },
            ValueRef::Global(ptr) => {
                LLVMSetInitializer(ptr, val);
                Ok(())
            },
            ValueRef::Slice(_) => {
                err(pos, ErrorCode::CodegenError, format!("Cannot store a slice"))
            },
        }
    }

    pub unsafe fn store(&self, ctx: &Context, val: ValueRef, pos: Pos) -> CompileResult<()>
    {
        match *self
        {
            ValueRef::Const(_) => {
                err(pos, ErrorCode::CodegenError, format!("Store cannot be called on a const value"))
            },
            ValueRef::Array(_) => {
                err(pos, ErrorCode::CodegenError, format!("Cannot store an array"))
            },
            ValueRef::Ptr(av) => {
                LLVMBuildStore(ctx.builder, val.load(ctx.builder), av);
                Ok(())
            },
            ValueRef::Global(ptr) => {
                LLVMSetInitializer(ptr, val.get());
                Ok(())
            },
            ValueRef::Slice(_) => {
                err(pos, ErrorCode::CodegenError, format!("Cannot store a slice"))
            },
        }
    }

    pub unsafe fn get_element_type(&self, pos: Pos) -> CompileResult<LLVMTypeRef>
    {
        match *self
        {
            ValueRef::Const(_) => {
                err(pos, ErrorCode::CodegenError, format!("Const values don't have an element type"))
            },
            ValueRef::Array(arr) => {
                Ok(LLVMGetElementType(LLVMGetElementType(LLVMTypeOf(arr)))) // arr is a pointer to an array
            },
            ValueRef::Ptr(ptr) => {
                Ok(LLVMGetElementType(LLVMTypeOf(ptr)))
            },
            ValueRef::Global(ptr) => {
                Ok(LLVMGetElementType(LLVMTypeOf(ptr)))
            },
            ValueRef::Slice(ref slice) => {
                Ok(slice.get_element_type())
            },
        }
    }




    pub unsafe fn get_array_element(&self, ctx: &Context, index: LLVMValueRef, pos: Pos) -> CompileResult<ValueRef>
    {
        match *self
        {
            ValueRef::Array(arr) => {
                let mut index_expr = vec![const_int(ctx, 0), index];
                Ok(ValueRef::ptr(
                    LLVMBuildGEP(ctx.builder, arr, index_expr.as_mut_ptr(), 2, cstr("el")),
                ))
            },
            _ => err(pos, ErrorCode::CodegenError, format!("Attempting to get an array element from a none array")),
        }

/*
         {
            let et = LLVMGetElementType(LLVMTypeOf(self.ptr));
            if is_same_kind(LLVMGetTypeKind(et), LLVMTypeKind::LLVMArrayTypeKind)
            {

            }
            else if is_same_kind(LLVMGetTypeKind(LLVMTypeOf(self.ptr)), LLVMTypeKind::LLVMArrayTypeKind)
            {
                let mut index_expr = vec![index];
                Ok(ValueRef::new(
                    LLVMBuildGEP(self.builder, self.ptr, index_expr.as_mut_ptr(), 1, cstr("el")),
                    self.constant,
                    self.builder,
                ))
            }

            else if is_same_kind(LLVMGetTypeKind(et), LLVMTypeKind::LLVMPointerTypeKind)
            {
                let elptr = LLVMBuildLoad(self.builder, self.ptr, cstr("elptr"));
                let mut index_expr = vec![index];
                Ok(ValueRef::new(
                    LLVMBuildGEP(self.builder, elptr, index_expr.as_mut_ptr(), 1, cstr("el")),
                    self.constant,
                    self.builder,
                ))
            }
            else
            {
                err(pos, ErrorCode::TypeError, format!("Attempting to index, something which is not indexable"))
            }
        }
            */
    }



}
