use std::ptr;
use std::ffi::CStr;
use llvm::core::*;
use llvm::prelude::*;
use llvm::*;
use codegen::cstr;
use codegen::context::Context;
use codegen::expressions::const_int;
use codegen::builtin::get_slice_type_name;

pub fn is_same_kind(a: LLVMTypeKind, b: LLVMTypeKind) -> bool
{
    (a as usize) == (b as usize)
}


unsafe fn array_to_slice(ctx: &Context, from: LLVMValueRef, to: LLVMTypeRef) -> Option<LLVMValueRef>
{
    let from_type = LLVMTypeOf(from);
    if !is_same_kind(LLVMGetTypeKind(from_type), LLVMTypeKind::LLVMPointerTypeKind) ||
       !is_same_kind(LLVMGetTypeKind(LLVMGetElementType(from_type)), LLVMTypeKind::LLVMArrayTypeKind) {
        return None;
    }

    let array_element_type = LLVMGetElementType(LLVMGetElementType(from_type));
    // Slices are structs containing a length field and a pointer field
    if !is_same_kind(LLVMGetTypeKind(to), LLVMTypeKind::LLVMPointerTypeKind) ||
       !is_same_kind(LLVMGetTypeKind(LLVMGetElementType(to)), LLVMTypeKind::LLVMStructTypeKind) {
        return None
    }

    let sname = LLVMGetStructName(LLVMGetElementType(to));
    if sname == ptr::null() {
        return None;
    }

    let cname = CStr::from_ptr(sname);
    match cname.to_str()
    {
        Ok(cname_str) => {
            if get_slice_type_name(array_element_type) == cname_str
            {
                let ptr = LLVMBuildAlloca(ctx.builder, LLVMGetElementType(to), cstr("slice"));
                let len = LLVMGetArrayLength(LLVMGetElementType(from_type));
                LLVMBuildStore(ctx.builder, const_int(ctx.context, len as u64), LLVMBuildStructGEP(ctx.builder, ptr, 0, cstr("length")));
                let cast_to_ptr = LLVMBuildBitCast(ctx.builder, from, LLVMPointerType(array_element_type, 0), cstr("cast"));
                LLVMBuildStore(ctx.builder, cast_to_ptr, LLVMBuildStructGEP(ctx.builder, ptr, 1, cstr("data")));
                Some(ptr)
            }
            else
            {
                println!("array_to_slice: 4");
                None
            }
        },
        Err(_) => None,
    }
}

unsafe fn array_to_ptr(b: LLVMBuilderRef, from: LLVMValueRef, to: LLVMTypeRef) -> Option<LLVMValueRef>
{
    let from_type = LLVMTypeOf(from);
    let can_convert =
        is_same_kind(LLVMGetTypeKind(from_type), LLVMTypeKind::LLVMPointerTypeKind) &&
        is_same_kind(LLVMGetTypeKind(to), LLVMTypeKind::LLVMPointerTypeKind) &&
        is_same_kind(LLVMGetTypeKind(LLVMGetElementType(from_type)), LLVMTypeKind::LLVMArrayTypeKind) &&
        LLVMGetElementType(LLVMGetElementType(from_type)) == LLVMGetElementType(to);
    if can_convert {
        let cast = LLVMBuildBitCast(b, from, to, cstr("cast"));
        Some(cast)
    } else {
        None
    }
}

// Convert a value to a different type, if needed and possible
pub unsafe fn convert(ctx: &Context, from: LLVMValueRef, to: LLVMTypeRef) ->  Option<LLVMValueRef>
{
    let from_type = LLVMTypeOf(from);
    if from_type == to {
        return Some(from); // Same types, so no problem
    }

    let c = array_to_ptr(ctx.builder, from, to);
    if c.is_some() {
        return c;
    }

    let c = array_to_slice(ctx, from, to);
    if c.is_some() {
        return c;
    }

    None
}
