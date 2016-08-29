use llvm::prelude::*;
use llvm::core::*;

use ast::Type;
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

impl ValueRef
{
    pub fn const_value(v: LLVMValueRef) -> ValueRef
    {
        ValueRef::Const(v)
    }

    pub unsafe fn alloc(ctx: &Context, llvm_type: LLVMTypeRef, typ: &Type) -> ValueRef
    {
        let alloc = ctx.alloc(llvm_type, "alloc");
        match *typ
        {
            Type::Array(ref at) => {
                ValueRef::array(alloc, at.element_type.clone())
            },
            Type::Slice(ref st) => {
                ValueRef::slice(alloc, st.element_type.clone())
            },
            Type::Struct(ref st) => {
                ValueRef::struct_value(alloc, st.members.iter().map(|m| m.typ.clone()).collect())
            },
            _ => {
                ValueRef::Ptr(alloc)
            },
        }
    }

    pub unsafe fn array(arr: LLVMValueRef, element_type: Type) -> ValueRef
    {
        ValueRef::Array(Array::new(arr, element_type))
    }

    pub unsafe fn alloc_array(ctx: &Context, llvm_element_type: LLVMTypeRef, element_type: Type, len: usize) -> ValueRef
    {
        ValueRef::Array(Array::alloc(ctx, llvm_element_type, element_type, len))
    }

    pub unsafe fn alloc_struct(ctx: &Context, struct_type: LLVMTypeRef, member_types: Vec<Type>) -> ValueRef
    {
        ValueRef::Struct(StructValue::alloc(ctx, struct_type, member_types))
    }

    pub unsafe fn struct_value(sv: LLVMValueRef, member_types: Vec<Type>) -> ValueRef
    {
        ValueRef::Struct(StructValue::new(sv, member_types))
    }

    pub unsafe fn slice(slice: LLVMValueRef, element_type: Type) -> ValueRef
    {
        ValueRef::Slice(Slice::new(slice, element_type))
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
