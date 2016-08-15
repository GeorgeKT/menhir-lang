use llvm::prelude::*;
use llvm::core::*;
use llvm::*;

use codegen::{Context, Array, Slice, Sequence, cstr};
use compileerror::{Pos, CompileResult, ErrorCode, err};



#[derive(Debug, Clone)]
pub enum ValueRef
{
    Const(LLVMValueRef),
    Ptr(LLVMValueRef),
    Global(LLVMValueRef),
    Array(Array),
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

    pub fn alloc(ctx: &Context, typ: LLVMTypeRef) -> ValueRef
    {
        unsafe {
            let alloc = ctx.alloc(typ, "alloc");
            if is_same_kind(LLVMGetTypeKind(typ), LLVMTypeKind::LLVMArrayTypeKind) {
                ValueRef::Array(Array::new(alloc))
            } else {
                ValueRef::Ptr(alloc)
            }
        }
    }

    pub unsafe fn const_array(arr: LLVMValueRef) -> ValueRef
    {
        ValueRef::Array(Array::new(arr))
    }

    pub unsafe fn alloc_array(ctx: &Context, element_type: LLVMTypeRef, len: usize) -> ValueRef
    {
        ValueRef::Array(Array::alloc(ctx, element_type, len))
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
            ValueRef::Array(ref arr) => arr.get(),
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
            ValueRef::Array(ref arr) => arr.get(),
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

/*
    pub unsafe fn get_element_type(&self, pos: Pos) -> CompileResult<LLVMTypeRef>
    {
        match *self
        {
            ValueRef::Const(_) => {
                err(pos, ErrorCode::CodegenError, format!("Const values don't have an element type"))
            },
            ValueRef::Array(ref arr) => {
                Ok(arr.get_element_type()) // arr is a pointer to an array
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
*/
    pub unsafe fn index(&self, ctx: &Context, index: LLVMValueRef, pos: Pos) -> CompileResult<ValueRef>
    {
        match *self
        {
            ValueRef::Array(ref arr) => Ok(arr.get_element(ctx, index)),
            ValueRef::Slice(ref slice) => Ok(slice.get_element(ctx, index)),
            _ => err(pos, ErrorCode::CodegenError, format!("Attempting to get an array element from a none array")),
        }
    }
}
