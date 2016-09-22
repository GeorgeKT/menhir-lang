use std::rc::Rc;

use llvm::prelude::*;
use llvm::core::*;

use ast::{Type, SumType};
use codegen::{Context, ValueRef};

#[derive(Debug, Clone)]
pub struct SumTypeValue
{
    value: LLVMValueRef,
    typ: Rc<SumType>,
}

impl SumTypeValue
{
    pub fn new(value: LLVMValueRef, typ: Rc<SumType>) -> SumTypeValue
    {
        SumTypeValue{
            value: value,
            typ: typ
        }
    }

    pub unsafe fn get_data_ptr(&self, ctx: &Context, idx: usize) -> ValueRef
    {
        let data_ptr = LLVMBuildStructGEP(ctx.builder, self.value, 1, cstr!("data_ptr"));
        let case_type = &self.typ.cases[idx];
        match case_type.typ
        {
            Type::Int => ValueRef::Ptr(data_ptr), // Doesn't really make sense, maybe panic here
            Type::Struct(_) => {
                let llvm_case_type = ctx.resolve_type(&case_type.typ);
                let casted_data_ptr = LLVMBuildBitCast(ctx.builder, data_ptr, LLVMPointerType(llvm_case_type, 0), cstr!("cast"));
                ValueRef::new(casted_data_ptr, &case_type.typ)
            },
            _ => panic!("SumTypeCase must be an Int or a Struct"),
        }
    }

    pub unsafe fn get_type_ptr(&self, ctx: &Context) -> ValueRef
    {
        ValueRef::Ptr(LLVMBuildStructGEP(ctx.builder, self.value, 0, cstr!("type_ptr")))
    }

    pub fn get(&self) -> LLVMValueRef
    {
        self.value
    }
}
