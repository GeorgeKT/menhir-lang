use llvm::prelude::*;
use llvm::core::*;

use ast::Type;
use codegen::{Context, Array, StructValue, SumTypeValue};


#[derive(Debug, Clone)]
pub enum ValueRef
{
    Const(LLVMValueRef),
    Ptr(LLVMValueRef),
    Array(Array),
    Struct(StructValue),
    Sum(SumTypeValue),
}

impl ValueRef
{
    pub unsafe fn alloc(ctx: &Context, typ: &Type) -> ValueRef
    {
        let llvm_type = ctx.resolve_type(typ);
        let alloc = ctx.stack_alloc(llvm_type, "alloc");
        ValueRef::new(alloc, typ)
    }

    pub unsafe fn new(value: LLVMValueRef, typ: &Type) -> ValueRef
    {
        match *typ
        {
            Type::Array(ref at) => {
                ValueRef::Array(Array::new(value, at.element_type.clone()))
            },
            Type::Struct(ref st) => {
                ValueRef::Struct(
                    StructValue::new(
                        value,
                        st.members.iter().map(|m| m.typ.clone()).collect()
                    )
                )
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
            ValueRef::Ptr(av) => LLVMBuildLoad(builder, av, cstr!("load")),
            ValueRef::Array(ref arr) => arr.get(),
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
            ValueRef::Array(ref arr) => arr.get(),
            ValueRef::Struct(ref sv) => sv.get(),
            ValueRef::Sum(ref s) => s.get(),
        }
    }

    pub unsafe fn store_direct(&self, ctx: &Context, val: LLVMValueRef)
    {
        match *self
        {
            ValueRef::Ptr(av) => {
                LLVMBuildStore(ctx.builder, val, av);
            },
            _ => {
                panic!("Internal Compiler Error: Store not allowed")
            },
        }
    }

    pub unsafe fn store(&self, ctx: &Context, val: ValueRef)
    {
        match *self
        {
            ValueRef::Ptr(av) => {
                LLVMBuildStore(ctx.builder, val.load(ctx.builder), av);
            },
            _ => {
                panic!("Internal Compiler Error: Store not allowed")
            },
        }
    }

    pub unsafe fn member(&self, ctx: &Context, idx: usize) -> ValueRef
    {
        match *self
        {
            ValueRef::Struct(ref vr) => vr.get_member_ptr(ctx, idx),
            _ => panic!("Internal Compiler Error: Attempting to get a struct member from a non struct"),
        }
    }

    pub unsafe fn case_struct(&self, ctx: &Context, idx: usize) -> ValueRef
    {
        match *self
        {
            ValueRef::Sum(ref st) => st.get_data_ptr(ctx, idx),
            _ => panic!("Internal Compiler Error: Attempting to get a sum type case member from a non sum type"),
        }
    }

    pub unsafe fn case_type(&self, ctx: &Context) -> ValueRef
    {
        match *self
        {
            ValueRef::Sum(ref st) => st.get_type_ptr(ctx),
            _ => panic!("Internal Compiler Error: Attempting to get a sum type case member from a non sum type"),
        }
    }
}
