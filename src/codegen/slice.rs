use std::os::raw::c_uint;
use llvm::prelude::*;
use llvm::core::*;

use codegen::{Context, cstr, const_int, ValueRef};
use compileerror::{CompileResult, Pos};


pub unsafe fn new_slice_type(ctx: LLVMContextRef, element_type: LLVMTypeRef) -> LLVMTypeRef
{
    let mut members = vec![
        LLVMPointerType(element_type, 0),       // Pointer to slice data
        LLVMInt64TypeInContext(ctx),            // Length
        LLVMInt64TypeInContext(ctx),            // Offset
    ];

    LLVMStructType(members.as_mut_ptr(), members.len() as c_uint, 0)
}

#[derive(Debug, Clone)]
pub struct Slice
{
    builder: LLVMBuilderRef,
    element_type: LLVMTypeRef,
    slice: LLVMValueRef,
}

impl Slice
{
    pub unsafe fn from_array(ctx: &Context, slice_type: LLVMTypeRef, arr: &ValueRef, pos: Pos) -> CompileResult<Slice>
    {
        let slice = Slice{
            builder: ctx.builder,
            element_type: LLVMGetElementType(LLVMTypeOf(arr.get())),
            slice: LLVMBuildAlloca(ctx.builder, slice_type, cstr("slice")),
        };

        let array_len = LLVMGetArrayLength(LLVMTypeOf(arr.get()));
        let length_ptr = slice.get_length_ptr();
        try!(length_ptr.store_direct(ctx, const_int(ctx, array_len as u64), pos));

        let offset_ptr = slice.get_offset_ptr();
        try!(offset_ptr.store_direct(ctx, const_int(ctx, 0), pos));

        let first = try!(arr.get_array_element(ctx, const_int(ctx, 0), pos));
        let data_ptr = slice.get_data_ptr();
        try!(data_ptr.store_direct(ctx, first.get(), pos));
        Ok(slice)
    }

    pub fn get(&self) -> LLVMValueRef {self.slice}

    pub unsafe fn get_length_ptr(&self) -> ValueRef
    {
        ValueRef::ptr(LLVMBuildStructGEP(self.builder, self.slice, 1, cstr("length")))
    }

    pub unsafe fn get_offset_ptr(&self) -> ValueRef
    {
        ValueRef::ptr(LLVMBuildStructGEP(self.builder, self.slice, 2, cstr("offset")))
    }

    pub unsafe fn get_data_ptr(&self) -> ValueRef
    {
        ValueRef::ptr(LLVMBuildStructGEP(self.builder, self.slice, 0, cstr("data_ptr")))
    }

    pub unsafe fn get_element_type(&self) -> LLVMTypeRef
    {
        self.element_type
    }
}
