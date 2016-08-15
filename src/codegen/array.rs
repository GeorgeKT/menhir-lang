use llvm::prelude::*;
use llvm::core::*;

use codegen::{const_int, cstr, ValueRef, Context, Slice, Sequence};
use compileerror::{CompileResult, Pos};

#[derive(Debug, Clone)]
pub struct Array
{
    array: LLVMValueRef,
    element_type: LLVMTypeRef,
    len: usize,
}

impl Array
{
    pub unsafe fn new(arr: LLVMValueRef) -> Array
    {
        Array{
            array: arr,
            element_type: LLVMGetElementType(LLVMGetElementType(LLVMTypeOf(arr))),
            len: LLVMGetArrayLength(LLVMGetElementType(LLVMTypeOf(arr))) as usize,
        }
    }

    pub unsafe fn alloc(ctx: &Context, element_type: LLVMTypeRef, len: usize) -> Array
    {
        let typ = LLVMArrayType(element_type, len as u32);
        Array{
            array: ctx.alloc(typ, "array"),
            element_type: element_type,
            len: len,
        }
    }

    pub fn get(&self) -> LLVMValueRef {self.array}
    pub fn get_length(&self) -> usize {self.len}
    pub fn get_element_type(&self) -> LLVMTypeRef {self.element_type}
}

impl Sequence for Array
{
    unsafe fn gen_length(&self, ctx: &Context) -> ValueRef
    {
        ValueRef::const_value(const_int(ctx, self.len as u64))
    }

    unsafe fn get_element(&self, ctx: &Context, index: LLVMValueRef) -> ValueRef
    {
        let mut index_expr = vec![const_int(ctx, 0), index];
        ValueRef::Ptr(LLVMBuildGEP(ctx.builder, self.array, index_expr.as_mut_ptr(), 2, cstr("el")))
    }

    unsafe fn subslice(&self, ctx: &mut Context, offset: u64, pos: Pos) -> CompileResult<ValueRef>
    {
        let slice_type = ctx.get_slice_type(self.element_type);
        let s = try!(Slice::from_array(ctx, slice_type, self, offset, pos));
        Ok(ValueRef::Slice(s))
    }
}
