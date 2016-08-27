use llvm::prelude::*;
use llvm::core::*;
use llvm::*;

use codegen::{Context, Array, Slice, Sequence, StructValue, cstr};
use compileerror::{Pos, CompileResult, ErrorCode, err};



#[derive(Debug, Clone)]
pub enum ValueRef
{
    Const(LLVMValueRef),
    Ptr(LLVMValueRef),
    Global(LLVMValueRef),
    Array(Array),
    Slice(Slice),
    Struct(StructValue),
}

pub fn is_same_kind(a: LLVMTypeKind, b: LLVMTypeKind) -> bool
{
    (a as usize) == (b as usize)
}

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

    pub unsafe fn alloc_struct(ctx: &Context, struct_type: LLVMTypeRef) -> ValueRef
    {
        ValueRef::Struct(StructValue::alloc(ctx, struct_type))
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
            ValueRef::Struct(ref sv) => sv.get(),
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
            ValueRef::Struct(ref sv) => sv.get(),
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
            ValueRef::Struct(_) => {
                err(pos, ErrorCode::CodegenError, format!("Cannot store a struct"))
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
            ValueRef::Struct(_) => {
                err(pos, ErrorCode::CodegenError, format!("Cannot store a struct"))
            },
        }
    }

    pub unsafe fn index(&self, ctx: &Context, index: LLVMValueRef, pos: Pos) -> CompileResult<ValueRef>
    {
        match *self
        {
            ValueRef::Array(ref arr) => Ok(arr.get_element(ctx, index)),
            ValueRef::Slice(ref slice) => Ok(slice.get_element(ctx, index)),
            _ => err(pos, ErrorCode::CodegenError, format!("Attempting to get an array element from a none array")),
        }
    }

    pub unsafe fn member(&self, ctx: &Context, idx: usize, pos: Pos) -> CompileResult<ValueRef>
    {
        match *self
        {
            ValueRef::Struct(ref vr) => Ok(vr.get_member_ptr(ctx, idx)),
            _ => err(pos, ErrorCode::CodegenError, format!("Attempting to get a struct member from a none struct")),
        }
    }
}
