use llvm::prelude::*;
use llvm::core::*;

use ast::{Type};
use codegen::{const_int, const_bool, ValueRef, Context};

#[derive(Debug, Clone)]
pub struct Array
{
    array: LLVMValueRef,
    pub element_type: Type,
}

unsafe fn heap_alloc_array(ctx: &Context, element_type: &Type, len: LLVMValueRef) -> LLVMValueRef
{
    let llvm_element_type = ctx.resolve_type(&element_type);
    let array = ctx.heap_alloc_array(llvm_element_type, len, "array_data");
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

    pub unsafe fn concat(ctx: &Context, left: &Array, right: &Array) -> Array
    {
        let concat_fn = ctx.get_builtin("concat");
        let size = ctx.size_of_type(&left.element_type);
        let mut args = vec![
            left.array,
            right.array,
            const_int(ctx, size as u64),
        ];

        let arr = LLVMBuildCall(ctx.builder, concat_fn.function, args.as_mut_ptr(), args.len() as u32, cstr!(""));
        Array::new(arr, left.element_type.clone())
    }

    pub unsafe fn init(&self, ctx: &Context, len: LLVMValueRef)
    {
        // First allocate the storage
        let array_data = heap_alloc_array(ctx, &self.element_type, len);

        self.set_length(ctx, len);
        self.set_offset(ctx, const_int(ctx, 0));
        self.set_heap_allocated_flag(ctx, const_bool(ctx, true));

        let first = get_array_element(ctx, array_data, const_int(ctx, 0), &self.element_type);
        self.set_data_ptr(ctx, first.get());
    }

    pub unsafe fn fill_with_string_literal(&self, ctx: &Context, glob: LLVMValueRef, len: usize)
    {
        self.set_length(ctx, const_int(ctx, len as u64));
        self.set_offset(ctx, const_int(ctx, 0));

        let data_ptr = self.get_data_ptr(ctx);

        let mut index_expr = vec![const_int(ctx, 0), const_int(ctx, 0)];
        let first_element = LLVMBuildGEP(ctx.builder, glob, index_expr.as_mut_ptr(), 2, cstr!("first_element"));
        data_ptr.store_direct(ctx, first_element);

        let heap_allocated_ptr = self.get_heap_allocated_flag_ptr(ctx);
        heap_allocated_ptr.store_direct(ctx, const_bool(ctx, false))
    }

    pub fn get(&self) -> LLVMValueRef {self.array}

    pub unsafe fn head(&self, ctx: &Context) -> ValueRef
    {
        self.get_element(ctx, const_int(ctx, 0))
    }

    pub unsafe fn tail(&self, ctx: &Context, dst: &Array)
    {
        self.subslice(ctx, 1, dst)
    }

    pub unsafe fn get_length_ptr(&self, ctx: &Context) -> ValueRef
    {
        ValueRef::Ptr(LLVMBuildStructGEP(ctx.builder, self.array, 1, cstr!("length")))
    }

    pub unsafe fn set_length(&self, ctx: &Context, len: LLVMValueRef)
    {
        let length_ptr = self.get_length_ptr(ctx);
        length_ptr.store_direct(ctx, len);
    }

    pub unsafe fn get_offset_ptr(&self, ctx: &Context) -> ValueRef
    {
        ValueRef::Ptr(LLVMBuildStructGEP(ctx.builder, self.array, 2, cstr!("offset")))
    }

    pub unsafe fn set_offset(&self, ctx: &Context, offset: LLVMValueRef)
    {
        let offset_ptr = self.get_offset_ptr(ctx);
        offset_ptr.store_direct(ctx, offset);
    }

    pub unsafe fn get_heap_allocated_flag_ptr(&self, ctx: &Context) -> ValueRef
    {
        ValueRef::Ptr(LLVMBuildStructGEP(ctx.builder, self.array, 3, cstr!("heap_allocated")))
    }

    pub unsafe fn set_heap_allocated_flag(&self, ctx: &Context, val: LLVMValueRef)
    {
        let ptr = self.get_heap_allocated_flag_ptr(ctx);
        ptr.store_direct(ctx, val);
    }

    pub unsafe fn get_data_ptr(&self, ctx: &Context) -> ValueRef
    {
        ValueRef::Ptr(LLVMBuildStructGEP(ctx.builder, self.array, 0, cstr!("data_ptr")))
    }

    pub unsafe fn set_data_ptr(&self, ctx: &Context, val: LLVMValueRef)
    {
        let data_ptr = self.get_data_ptr(ctx);
        data_ptr.store_direct(ctx, val);
    }

    pub unsafe fn subslice(&self, ctx: &Context, offset: u64, dst: &Array)
    {
        let length_ptr = self.get_length_ptr(ctx);
        let new_length = LLVMBuildSub(ctx.builder, length_ptr.load(ctx.builder), const_int(ctx, offset), cstr!("new_length"));
        dst.set_length(ctx, new_length);

        let offset_ptr = self.get_offset_ptr(ctx);
        let new_offset = LLVMBuildAdd(ctx.builder, offset_ptr.load(ctx.builder), const_int(ctx, offset), cstr!("new_offset"));
        dst.set_offset(ctx, new_offset);

        dst.set_data_ptr(ctx, self.get_data_ptr(ctx).load(ctx.builder));
        dst.set_heap_allocated_flag(ctx, self.get_heap_allocated_flag_ptr(ctx).load(ctx.builder));
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

    /*
    pub unsafe fn inc_ref(&self, ctx: &Context)
    {
        let arc_inc_ref = ctx.get_builtin("arc_inc_ref");
        let array_data = self.get_data_ptr(ctx);
        let void_ptr = LLVMBuildBitCast(ctx.builder, array_data.get(), ctx.resolve_type(&Type::VoidPtr), cstr!("cast_to_void_ptr"));
        let mut args = vec![
            void_ptr
        ];
        LLVMBuildCall(ctx.builder, arc_inc_ref.function, args.as_mut_ptr(), 1, cstr!(""));
    }


    pub unsafe fn dec_ref(&self, ctx: &Context)
    {
        let arc_dec_ref = ctx.get_builtin("arc_dec_ref");
        let array_data = self.get_data_ptr(ctx);
        let void_ptr = LLVMBuildBitCast(ctx.builder, array_data.get(), ctx.resolve_type(&Type::VoidPtr), cstr!("cast_to_void_ptr"));
        let mut args = vec![
            void_ptr
        ];
        LLVMBuildCall(ctx.builder, arc_dec_ref.function, args.as_mut_ptr(), 1, cstr!(""));
    }
        */
}