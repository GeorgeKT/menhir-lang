use llvm::prelude::*;
use llvm::core::*;

use ast::{Type, array_type};
use codegen::{const_int, ValueRef, Context};

#[derive(Debug, Clone)]
pub struct Array
{
    array: LLVMValueRef,
    element_type: Type,
}

unsafe fn heap_alloc_array(ctx: &mut Context, element_type: &Type, len: LLVMValueRef) -> LLVMValueRef
{
    let llvm_element_type = ctx.resolve_type(&element_type);
    let array = ctx.heap_alloc_array(llvm_element_type, len, "array_data");
    ctx.add_dec_ref_target(array);
    array
}

unsafe fn get_array_element(ctx: &Context, array: LLVMValueRef, index: LLVMValueRef, element_type: &Type) -> ValueRef
{
    let mut index_expr = vec![index];
    let element = LLVMBuildGEP(ctx.builder, array, index_expr.as_mut_ptr(), 1, cstr!("el"));
    ValueRef::new(element, element_type)
}

impl Array
{
    pub unsafe fn new(arr: LLVMValueRef, element_type: Type) -> Array
    {
        Array{
            array: arr,
            element_type: element_type,
        }
    }

    pub unsafe fn empty(ctx: &Context) -> Array
    {
        let element_type = ctx.resolve_type(&Type::Int);
        let slice_type = ctx.resolve_type(&array_type(Type::Int));
        let slice = Array{
            element_type: Type::Int,
            array: ctx.stack_alloc(slice_type, "array"),
        };

        let length_ptr = slice.get_length_ptr(ctx);
        length_ptr.store_direct(ctx, const_int(ctx, 0));

        let offset_ptr = slice.get_offset_ptr(ctx);
        offset_ptr.store_direct(ctx, const_int(ctx, 0));

        let data_ptr = slice.get_data_ptr(ctx);
        data_ptr.store_direct(ctx, LLVMConstPointerNull(LLVMPointerType(element_type, 0)));
        slice
    }

    pub unsafe fn init(&mut self, ctx: &mut Context, len: usize)
    {
        // First allocate the storage
        let array_len =  const_int(ctx, len as u64);
        let array_data = heap_alloc_array(ctx, &self.element_type, array_len);

        let length_ptr = self.get_length_ptr(ctx);
        length_ptr.store_direct(ctx, const_int(ctx, len as u64));

        let offset_ptr = self.get_offset_ptr(ctx);
        offset_ptr.store_direct(ctx, const_int(ctx, 0));

        let first = get_array_element(ctx, array_data, const_int(ctx, 0), &self.element_type);
        let data_ptr = self.get_data_ptr(ctx);
        data_ptr.store_direct(ctx, first.get());
    }

    pub unsafe fn alloc(ctx: &mut Context, array_type: LLVMTypeRef, element_type: Type, len: usize) -> Array
    {
        let mut slice = Array{
            array: ctx.stack_alloc(array_type, "array"),
            element_type: element_type,
        };

        slice.init(ctx, len);
        slice
    }

    pub fn get(&self) -> LLVMValueRef {self.array}

    pub unsafe fn head(&self, ctx: &Context) -> ValueRef
    {
        self.get_element(ctx, const_int(ctx, 0))
    }

    pub unsafe fn tail(&self, ctx: &Context) -> ValueRef
    {
        self.subslice(ctx, 1)
    }

    pub unsafe fn get_length_ptr(&self, ctx: &Context) -> ValueRef
    {
        ValueRef::Ptr(LLVMBuildStructGEP(ctx.builder, self.array, 1, cstr!("length")))
    }

    pub unsafe fn get_offset_ptr(&self, ctx: &Context) -> ValueRef
    {
        ValueRef::Ptr(LLVMBuildStructGEP(ctx.builder, self.array, 2, cstr!("offset")))
    }

    pub unsafe fn get_data_ptr(&self, ctx: &Context) -> ValueRef
    {
        ValueRef::Ptr(LLVMBuildStructGEP(ctx.builder, self.array, 0, cstr!("data_ptr")))
    }

    pub unsafe fn subslice(&self, ctx: &Context, offset: u64) -> ValueRef
    {
        let slice_type = ctx.resolve_type(&array_type(self.element_type.clone()));
        let slice = Array{
            array: ctx.stack_alloc(slice_type, "subslice"),
            element_type: self.element_type.clone(),
        };

        let slice_length_ptr = slice.get_length_ptr(ctx);
        let length_ptr = self.get_length_ptr(ctx);
        let new_length = LLVMBuildSub(ctx.builder, length_ptr.load(ctx.builder), const_int(ctx, offset), cstr!("new_length"));
        slice_length_ptr.store_direct(ctx, new_length);

        let slice_offset_ptr = slice.get_offset_ptr(ctx);
        let offset_ptr = self.get_offset_ptr(ctx);
        let new_offset = LLVMBuildAdd(ctx.builder, offset_ptr.load(ctx.builder), const_int(ctx, offset), cstr!("new_offset"));
        slice_offset_ptr.store_direct(ctx, new_offset);

        let slice_data_ptr = slice.get_data_ptr(ctx);
        slice_data_ptr.store_direct(ctx, self.get_data_ptr(ctx).load(ctx.builder));
        ValueRef::Array(slice)
    }

    pub unsafe fn get_element(&self, ctx: &Context, idx: LLVMValueRef) -> ValueRef
    {
        let data_ptr = self.get_data_ptr(ctx);
        let array_ptr = data_ptr.load(ctx.builder);

        let offset_ptr = self.get_offset_ptr(ctx);
        let offset = offset_ptr.load(ctx.builder);
        let new_pos = LLVMBuildAdd(ctx.builder, offset, idx, cstr!("add"));

        let mut index_expr = vec![new_pos];
        let element = LLVMBuildGEP(ctx.builder, array_ptr, index_expr.as_mut_ptr(), 1, cstr!("el"));
        ValueRef::new(element, &self.element_type)
    }

    pub unsafe fn gen_length(&self, ctx: &Context) -> ValueRef
    {
        ValueRef::Const(self.get_length_ptr(ctx).load(ctx.builder))
    }
}
