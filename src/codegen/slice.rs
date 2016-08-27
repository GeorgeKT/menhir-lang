use std::os::raw::c_uint;
use std::ops::Deref;
use llvm::prelude::*;
use llvm::core::*;

use ast::Type;
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
    slice_type: LLVMTypeRef,
    slice: LLVMValueRef,
    element_type: Type,
}

impl Slice
{
    pub unsafe fn new(slice: LLVMValueRef, element_type: Type) -> Slice
    {
        let slice_type = LLVMGetElementType(LLVMTypeOf(slice));
        let mut element_types = vec![slice_type; 3]; // Will be overwritten
        LLVMGetStructElementTypes(slice_type, element_types.as_mut_ptr());
        Slice{
            element_type: element_type,
            slice_type: slice_type,
            slice: slice,
        }
    }

    pub unsafe fn empty(ctx: &mut Context, pos: Pos) -> CompileResult<Slice>
    {
        let element_type = LLVMInt64TypeInContext(ctx.context);
        let slice_type = ctx.get_slice_type(element_type);
        let slice = Slice{
            element_type: Type::Int,
            slice_type: slice_type,
            slice: ctx.alloc(slice_type, "slice"),
        };

        let length_ptr = slice.get_length_ptr(ctx);
        try!(length_ptr.store_direct(ctx, const_int(ctx, 0), pos));

        let offset_ptr = slice.get_offset_ptr(ctx);
        try!(offset_ptr.store_direct(ctx, const_int(ctx, 0), pos));

        let data_ptr = slice.get_data_ptr(ctx);
        try!(data_ptr.store_direct(ctx, LLVMConstPointerNull(LLVMPointerType(element_type, 0)), pos));
        Ok(slice)
    }

    pub unsafe fn from_array(ctx: &Context, slice_type: LLVMTypeRef, arr: &Array, offset: u64, pos: Pos) -> CompileResult<Slice>
    {
        let slice = Slice{
            element_type: arr.get_element_type(),
            slice_type: slice_type,
            slice: ctx.alloc(slice_type, "slice"),
        };

        let array_len = arr.get_length();
        let length_ptr = slice.get_length_ptr(ctx);
        try!(length_ptr.store_direct(ctx, const_int(ctx, array_len as u64), pos));

        let offset_ptr = slice.get_offset_ptr(ctx);
        try!(offset_ptr.store_direct(ctx, const_int(ctx, offset as u64), pos));

        let first = arr.get_element(ctx, const_int(ctx, 0));
        let data_ptr = slice.get_data_ptr(ctx);
        try!(data_ptr.store_direct(ctx, first.get(), pos));
        Ok(slice)
    }

    pub fn get(&self) -> LLVMValueRef {self.slice}

    pub unsafe fn get_length_ptr(&self, ctx: &Context) -> ValueRef
    {
        ValueRef::Ptr(LLVMBuildStructGEP(ctx.builder, self.slice, 1, cstr("length")))
    }

    pub unsafe fn get_offset_ptr(&self, ctx: &Context) -> ValueRef
    {
        ValueRef::Ptr(LLVMBuildStructGEP(ctx.builder, self.slice, 2, cstr("offset")))
    }

    pub unsafe fn get_data_ptr(&self, ctx: &Context) -> ValueRef
    {
        ValueRef::Ptr(LLVMBuildStructGEP(ctx.builder, self.slice, 0, cstr("data_ptr")))
    }
}

impl Sequence for Slice
{
    unsafe fn subslice(&self, ctx: &mut Context, offset: u64, pos: Pos) -> CompileResult<ValueRef>
    {
        let slice = Slice{
            slice_type: self.slice_type,
            slice: ctx.alloc(self.slice_type, "subslice"),
            element_type: self.element_type.clone(),
        };

        let slice_length_ptr = slice.get_length_ptr(ctx);
        let length_ptr = self.get_length_ptr(ctx);
        let new_length = LLVMBuildSub(ctx.builder, length_ptr.load(ctx.builder), const_int(ctx, offset), cstr("new_length"));
        try!(slice_length_ptr.store_direct(ctx, new_length, pos));

        let slice_offset_ptr = slice.get_offset_ptr(ctx);
        let offset_ptr = self.get_offset_ptr(ctx);
        let new_offset = LLVMBuildAdd(ctx.builder, offset_ptr.load(ctx.builder), const_int(ctx, offset), cstr("new_offset"));
        try!(slice_offset_ptr.store_direct(ctx, new_offset, pos));

        let slice_data_ptr = slice.get_data_ptr(ctx);
        try!(slice_data_ptr.store_direct(ctx, self.get_data_ptr(ctx).load(ctx.builder), pos));
        Ok(ValueRef::Slice(slice))
    }

    unsafe fn get_element(&self, ctx: &Context, idx: LLVMValueRef) -> ValueRef
    {
        let data_ptr = self.get_data_ptr(ctx);
        let offset_ptr = self.get_offset_ptr(ctx);
        let offset = offset_ptr.load(ctx.builder);
        let pos = LLVMBuildAdd(ctx.builder, offset, idx, cstr("add"));
        let elptr = data_ptr.load(ctx.builder);
        let mut index_expr = vec![pos];
        let element = LLVMBuildGEP(ctx.builder, elptr, index_expr.as_mut_ptr(), 1, cstr("el"));
        match self.element_type
        {
            Type::Slice(ref nested_element_type) => ValueRef::slice(element, nested_element_type.deref().clone()),
            Type::Array(ref nested_element_type, _) => ValueRef::array(element, nested_element_type.deref().clone()),
            Type::Struct(_) => ValueRef::struct_value(element, self.element_type.get_member_types()),
            _ => ValueRef::Ptr(element),
        }
    }

    unsafe fn gen_length(&self, ctx: &Context) -> ValueRef
    {
        ValueRef::const_value(self.get_length_ptr(ctx).load(ctx.builder))
    }
}
