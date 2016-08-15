use std::os::raw::c_uint;
use llvm::prelude::*;
use llvm::core::*;

use codegen::{Context, cstr, const_int, ValueRef, Array, Sequence};
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
    slice_type: LLVMTypeRef,
    slice: LLVMValueRef,
}

impl Slice
{
    pub unsafe fn new(builder: LLVMBuilderRef, slice: LLVMValueRef) -> Slice
    {
        let slice_type = LLVMGetElementType(LLVMTypeOf(slice));
        let mut element_types = vec![slice_type; 3]; // Will be overwritten
        LLVMGetStructElementTypes(slice_type, element_types.as_mut_ptr());
        Slice{
            builder: builder,
            element_type: element_types[2],
            slice_type: slice_type,
            slice: slice,
        }
    }

    pub unsafe fn empty(ctx: &mut Context, pos: Pos) -> CompileResult<Slice>
    {
        let element_type = LLVMInt64TypeInContext(ctx.context);
        let slice_type = ctx.get_slice_type(element_type);
        let slice = Slice{
            builder: ctx.builder,
            element_type: element_type,
            slice_type: slice_type,
            slice: ctx.alloc(slice_type, "slice"),
        };

        let length_ptr = slice.get_length_ptr();
        try!(length_ptr.store_direct(ctx, const_int(ctx, 0), pos));

        let offset_ptr = slice.get_offset_ptr();
        try!(offset_ptr.store_direct(ctx, const_int(ctx, 0), pos));

        let data_ptr = slice.get_data_ptr();
        try!(data_ptr.store_direct(ctx, LLVMConstPointerNull(LLVMPointerType(element_type, 0)), pos));
        Ok(slice)
    }

    pub unsafe fn from_array(ctx: &Context, slice_type: LLVMTypeRef, arr: &Array, offset: u64, pos: Pos) -> CompileResult<Slice>
    {
        let slice = Slice{
            builder: ctx.builder,
            element_type: arr.get_element_type(),
            slice_type: slice_type,
            slice: ctx.alloc(slice_type, "slice"),
        };

        let array_len = arr.get_length();
        let length_ptr = slice.get_length_ptr();
        try!(length_ptr.store_direct(ctx, const_int(ctx, array_len as u64), pos));

        let offset_ptr = slice.get_offset_ptr();
        try!(offset_ptr.store_direct(ctx, const_int(ctx, offset as u64), pos));

        let first = arr.get_element(ctx, const_int(ctx, 0));
        let data_ptr = slice.get_data_ptr();
        try!(data_ptr.store_direct(ctx, first.get(), pos));
        Ok(slice)
    }

    pub fn get(&self) -> LLVMValueRef {self.slice}

    pub unsafe fn get_length_ptr(&self) -> ValueRef
    {
        ValueRef::Ptr(LLVMBuildStructGEP(self.builder, self.slice, 1, cstr("length")))
    }

    pub unsafe fn get_offset_ptr(&self) -> ValueRef
    {
        ValueRef::Ptr(LLVMBuildStructGEP(self.builder, self.slice, 2, cstr("offset")))
    }

    pub unsafe fn get_data_ptr(&self) -> ValueRef
    {
        ValueRef::Ptr(LLVMBuildStructGEP(self.builder, self.slice, 0, cstr("data_ptr")))
    }

/*
    pub fn get_element_type(&self) -> LLVMTypeRef
    {
        self.element_type
    }
*/
}

impl Sequence for Slice
{
    unsafe fn subslice(&self, ctx: &mut Context, offset: u64, pos: Pos) -> CompileResult<ValueRef>
    {
        let slice = Slice{
            builder: self.builder,
            element_type: self.element_type,
            slice_type: self.slice_type,
            slice: ctx.alloc(self.slice_type, "subslice"),
        };

        let slice_length_ptr = slice.get_length_ptr();
        let length_ptr = self.get_length_ptr();
        let new_length = LLVMBuildSub(ctx.builder, length_ptr.load(ctx.builder), const_int(ctx, offset), cstr("new_length"));
        try!(slice_length_ptr.store_direct(ctx, new_length, pos));

        let slice_offset_ptr = slice.get_offset_ptr();
        let offset_ptr = self.get_offset_ptr();
        let new_offset = LLVMBuildAdd(ctx.builder, offset_ptr.load(ctx.builder), const_int(ctx, offset), cstr("new_offset"));
        try!(slice_offset_ptr.store_direct(ctx, new_offset, pos));

        let slice_data_ptr = slice.get_data_ptr();
        try!(slice_data_ptr.store_direct(ctx, self.get_data_ptr().load(ctx.builder), pos));
        Ok(ValueRef::Slice(slice))
    }

    unsafe fn get_element(&self, ctx: &Context, idx: LLVMValueRef) -> ValueRef
    {
        let data_ptr = self.get_data_ptr();
        let offset_ptr = self.get_offset_ptr();
        let offset = offset_ptr.load(ctx.builder);
        let pos = LLVMBuildAdd(ctx.builder, offset, idx, cstr("add"));
        let elptr = data_ptr.load(self.builder);
        let mut index_expr = vec![pos];
        ValueRef::Ptr(LLVMBuildGEP(self.builder, elptr, index_expr.as_mut_ptr(), 1, cstr("el")))
    }

    unsafe fn gen_length(&self, ctx: &Context) -> ValueRef
    {
        ValueRef::const_value(self.get_length_ptr().load(ctx.builder))
    }
}
