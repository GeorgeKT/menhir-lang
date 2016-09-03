use llvm::prelude::*;
use llvm::core::*;

use ast::Type;
use codegen::{const_int, cstr, ValueRef, Context, Slice, Sequence};
use compileerror::{CompileResult, Pos};

#[derive(Debug, Clone)]
pub struct Array
{
    array: LLVMValueRef,
    element_type: Type,
    len: usize,
}

impl Array
{
    pub unsafe fn new(arr: LLVMValueRef, element_type: Type) -> Array
    {
        Array{
            array: arr,
            element_type: element_type,
            len: LLVMGetArrayLength(LLVMGetElementType(LLVMTypeOf(arr))) as usize,
        }
    }

    pub fn get(&self) -> LLVMValueRef {self.array}
    pub fn get_length(&self) -> usize {self.len}
    pub fn get_element_type(&self) -> Type {self.element_type.clone()}
    pub unsafe fn get_llvm_element_type(&self) -> LLVMTypeRef {LLVMGetElementType(LLVMGetElementType(LLVMTypeOf(self.array)))}
}

impl Sequence for Array
{
    unsafe fn gen_length(&self, ctx: &Context) -> ValueRef
    {
        ValueRef::Const(const_int(ctx, self.len as u64))
    }

    unsafe fn get_element(&self, ctx: &Context, index: LLVMValueRef) -> ValueRef
    {
        let mut index_expr = vec![const_int(ctx, 0), index];
        let element = LLVMBuildGEP(ctx.builder, self.array, index_expr.as_mut_ptr(), 2, cstr("el"));
        ValueRef::new(element, &self.element_type)
    }

    unsafe fn subslice(&self, ctx: &mut Context, offset: u64, pos: Pos) -> CompileResult<ValueRef>
    {
        let slice_type = ctx.get_slice_type(self.get_llvm_element_type());
        let s = try!(Slice::from_array(ctx, slice_type, self, offset, pos));
        Ok(ValueRef::Slice(s))
    }
}
