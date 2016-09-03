use llvm::prelude::*;
use llvm::core::*;

use ast::Type;
use codegen::{Context, Array, Slice, Sequence, StructValue, SumTypeValue, cstr};
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
    Sum(SumTypeValue)
}

impl ValueRef
{
    pub unsafe fn alloc(ctx: &mut Context, typ: &Type) -> ValueRef
    {
        let llvm_type = ctx.resolve_type(typ);
        let alloc = ctx.alloc(llvm_type, "alloc");
        ValueRef::new(alloc, typ)
    }

    pub unsafe fn new(value: LLVMValueRef, typ: &Type) -> ValueRef
    {
        match *typ
        {
            Type::Array(ref at) => {
                ValueRef::Array(Array::new(value, at.element_type.clone()))
            },
            Type::Slice(ref st) => {
                ValueRef::Slice(Slice::new(value, st.element_type.clone()))
            },
            Type::Struct(ref st) => {
                ValueRef::Struct(StructValue::new(value, st.members.iter().map(|m| m.typ.clone()).collect()))
            },
            Type::Sum(ref st) => {
                ValueRef::Sum(SumTypeValue::new(value, st.clone()))
            },
            _ => {
                ValueRef::Ptr(value)
            },
        }
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
            ValueRef::Sum(ref s) => s.get(),
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
            ValueRef::Sum(ref s) => s.get(),
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
            ValueRef::Sum(_) => {
                err(pos, ErrorCode::CodegenError, format!("Cannot store a sum"))
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
            ValueRef::Sum(_) => {
                err(pos, ErrorCode::CodegenError, format!("Cannot store a sum"))
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

    pub unsafe fn case_struct(&self, ctx: &mut Context, idx: usize, pos: Pos) -> CompileResult<ValueRef>
    {
        match *self
        {
            ValueRef::Sum(ref st) => Ok(st.get_data_ptr(ctx, idx)),
            _ => err(pos, ErrorCode::CodegenError, format!("Attempting to get a sum type case member from a none sum type")),
        }
    }

    pub unsafe fn case_type(&self, ctx: &Context, pos: Pos) -> CompileResult<ValueRef>
    {
        match *self
        {
            ValueRef::Sum(ref st) => Ok(st.get_type_ptr(ctx)),
            _ => err(pos, ErrorCode::CodegenError, format!("Attempting to get a sum type case member from a none sum type")),
        }
    }
}
