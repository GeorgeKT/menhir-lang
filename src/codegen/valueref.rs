use llvm::prelude::*;
use llvm::core::*;

use ast::{Type, ArrayProperty};
use codegen::{Context, Array, StructValue, SumTypeValue};


#[derive(Debug, Clone)]
pub enum ValueRef
{
    Const(LLVMValueRef),
    Ptr(LLVMValueRef),
    Array(Array),
    Struct(StructValue),
    Sum(SumTypeValue),
    HeapPtr(LLVMValueRef, Type),
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
            Type::Func(_) => {
                ValueRef::Const(value)
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
            ValueRef::HeapPtr(av, _) => LLVMBuildLoad(builder, av, cstr!("load")),
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
            ValueRef::HeapPtr(av, _) => av,
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
            ValueRef::HeapPtr(av, _) => {
                LLVMBuildStore(ctx.builder, val, av);
            },
            _ => {
                panic!("Internal Compiler Error: Store not allowed")
            },
        }
    }

    pub unsafe fn store(&self, ctx: &Context, val: &ValueRef)
    {
        self.store_direct(ctx, val.load(ctx.builder))
    }

    pub unsafe fn deref(&self, ctx: &Context) -> ValueRef
    {
        match *self
        {
            ValueRef::HeapPtr(_, ref typ) => {
                ValueRef::new(self.load(ctx.builder), typ)
            },
            _ => {
                self.clone()
            },
        }
    }

    pub unsafe fn member(&self, ctx: &Context, idx: usize) -> ValueRef
    {
        match *self
        {
            ValueRef::Struct(ref vr) => {
                vr.get_member_ptr(ctx, idx)
            },

            ValueRef::HeapPtr(_, Type::Struct(_)) => {
                self.deref(ctx).member(ctx, idx)
            },
            _ => panic!("Internal Compiler Error: Invalid member access"),
        }
    }

    pub unsafe fn array_property(&self, ctx: &Context, prop: ArrayProperty) -> ValueRef
    {
        match (self, prop)
        {
            (&ValueRef::Array(ref ar), ArrayProperty::Len) => {
                ar.get_length_ptr(ctx)
            },

            (&ValueRef::HeapPtr(_, Type::Array(_)), ArrayProperty::Len) => {
                self.deref(ctx).array_property(ctx, prop)
            },
            _ => panic!("Internal Compiler Error: Invalid array property access"),
        }
    }


    pub unsafe fn case_struct(&self, ctx: &Context, idx: usize) -> ValueRef
    {
        match *self
        {
            ValueRef::Sum(ref st) => st.get_data_ptr(ctx, idx),
            ValueRef::HeapPtr(_, Type::Sum(_)) => {
                let vr = self.deref(ctx);
                vr.case_struct(ctx, idx)
            },
            _ => panic!("Internal Compiler Error: Attempting to get a sum type case member from a non sum type"),
        }
    }

    pub unsafe fn case_type(&self, ctx: &Context) -> ValueRef
    {
        match *self
        {
            ValueRef::Sum(ref st) => st.get_type_ptr(ctx),
            ValueRef::HeapPtr(_, Type::Sum(_)) => {
                let vr = self.deref(ctx);
                vr.case_type(ctx)
            },
            _ => panic!("Internal Compiler Error: Attempting to get a sum type case member from a non sum type"),
        }
    }
}
