use llvm::prelude::*;
use llvm::core::*;
use llvm::{LLVMTypeKind, LLVMLinkage};
use codegen::{type_name, cstr};
use codegen::context::{Context};
use codegen::expressions::{const_int};
use codegen::conversions::{is_struct, is_array, is_pointer, convert, is_same_kind};
use compileerror::{Pos, CompileResult, ErrorCode, err, type_error};


#[derive(Debug, Clone)]
pub struct ValueRef
{
    ptr: LLVMValueRef,
    constant: bool,
    builder: LLVMBuilderRef,
}


impl ValueRef
{
    pub fn local(builder: LLVMBuilderRef, typ: LLVMTypeRef) -> ValueRef
    {
        unsafe {
            ValueRef{
                ptr: LLVMBuildAlloca(builder, typ, cstr("alloc")),
                constant: false,
                builder: builder,
            }
        }
    }

    pub fn global(ctx: &Context, typ: LLVMTypeRef, linkage: LLVMLinkage, constant: bool) -> ValueRef
    {
        unsafe {
            let glob = LLVMAddGlobal(ctx.get_module_ref(), typ, cstr("glob_var"));
            LLVMSetLinkage(glob, linkage);
            LLVMSetGlobalConstant(glob, if constant {1} else {0});

            ValueRef{
                ptr: glob,
                constant: constant,
                builder: ctx.builder,
            }
        }
    }

    pub fn new(ptr: LLVMValueRef, constant: bool, builder: LLVMBuilderRef) -> ValueRef
    {
        ValueRef{
            ptr: ptr,
            constant: constant,
            builder: builder,
        }
    }

    pub fn is_pointer(&self) -> bool
    {
        unsafe {
            is_pointer(LLVMTypeOf(self.ptr))
        }
    }

    pub fn load(&self) -> LLVMValueRef
    {
        unsafe {
            if self.is_pointer()
            {
                // Do not attempt to load arrays and structs, should be accessed by get_array_element and get_struct_element
                if !is_same_kind(LLVMGetTypeKind(LLVMGetElementType(LLVMTypeOf(self.ptr))), LLVMTypeKind::LLVMStructTypeKind) &&
                   !is_same_kind(LLVMGetTypeKind(LLVMGetElementType(LLVMTypeOf(self.ptr))), LLVMTypeKind::LLVMArrayTypeKind)
                {
                    LLVMBuildLoad(self.builder, self.ptr, cstr("load"))
                }
                else
                {
                    self.ptr
                }
            }
            else
            {
                self.ptr
            }
        }

    }

    pub unsafe fn store(&self, ctx: &Context, val: ValueRef, pos: Pos) -> CompileResult<ValueRef>
    {
        if !self.is_pointer() {
            return Err(type_error(pos, format!("Store must be called on pointer types")));
        }

        let dst_typ = self.get_element_type();
        let val_typ = val.get_element_type();
        if let Some(cv) = convert(ctx, val, dst_typ)
        {
            if ctx.in_global_context()
            {
                if cv.is_constant_value()
                {
                    // We need to initialize globals when we are in the global context
                    LLVMSetInitializer(self.ptr, cv.get());
                    Ok(cv)
                }
                else
                {
                    return err(pos, ErrorCode::ExpectedConstExpr,
                        format!("Global variables and constants must be initialized with a constant expression"));
                }
            }
            else
            {
                if self.constant {
                    return err(pos, ErrorCode::ConstantModification, format!("Attempting to modify a constant"));
                }

                Ok(ValueRef::new(LLVMBuildStore(self.builder, cv.load(), self.ptr), self.constant, self.builder))
            }
        }
        else
        {
            Err(type_error(pos, format!("Wrong type ({}, expected {})", type_name(val_typ), type_name(dst_typ))))
        }
    }

    pub fn get(&self) -> LLVMValueRef
    {
        self.ptr
    }

    pub fn is_const(&self) -> bool
    {
        self.constant
    }

    pub fn get_value_type(&self) -> LLVMTypeRef
    {
        unsafe{LLVMTypeOf(self.ptr)}
    }

    pub fn get_element_type(&self) -> LLVMTypeRef
    {
        unsafe{
            if self.is_pointer() {
                LLVMGetElementType(LLVMTypeOf(self.ptr))
            } else {
                LLVMTypeOf(self.ptr)
            }
        }
    }

    pub fn is_constant_value(&self) -> bool
    {
        unsafe {
            LLVMIsConstant(self.ptr) != 0
        }
    }

    pub fn get_struct_element(&self, index: u32, pos: Pos) -> CompileResult<ValueRef>
    {
        if !self.is_pointer() {
            return Err(type_error(pos, format!("Attempting to get a struct element of a value")));
        }

        unsafe{
            let et = LLVMGetElementType(LLVMTypeOf(self.ptr));
            if is_struct(et)
            {
                Ok(ValueRef::new(
                    LLVMBuildStructGEP(self.builder, self.ptr, index, cstr("el")),
                    self.constant,
                    self.builder
                ))
            }
            else if is_pointer(et) && is_struct(LLVMGetElementType(et))
            {
                Ok(ValueRef::new(
                    LLVMBuildStructGEP(self.builder, LLVMBuildLoad(self.builder, self.ptr, cstr("elptr")), index, cstr("el")),
                    self.constant,
                    self.builder
                ))
            }
            else
            {
                Err(type_error(pos, format!("Attempting to get a struct element of a none struct")))
            }
        }
    }

    pub fn get_array_element(&self, ctx: &Context, index: LLVMValueRef, pos: Pos) -> CompileResult<ValueRef>
    {
        if !self.is_pointer() {
            return Err(type_error(pos, format!("Attempting to index a value")));
        }

        unsafe {
            let et = LLVMGetElementType(LLVMTypeOf(self.ptr));
            if is_same_kind(LLVMGetTypeKind(et), LLVMTypeKind::LLVMArrayTypeKind)
            {
                let mut index_expr = vec![const_int(ctx.context, 0), index];
                Ok(ValueRef::new(
                    LLVMBuildGEP(self.builder, self.ptr, index_expr.as_mut_ptr(), 2, cstr("el")),
                    self.constant,
                    self.builder,
                ))
            }
            else if is_same_kind(LLVMGetTypeKind(LLVMTypeOf(self.ptr)), LLVMTypeKind::LLVMArrayTypeKind)
            {
                let mut index_expr = vec![index];
                Ok(ValueRef::new(
                    LLVMBuildGEP(self.builder, self.ptr, index_expr.as_mut_ptr(), 1, cstr("el")),
                    self.constant,
                    self.builder,
                ))
            }

            else if is_same_kind(LLVMGetTypeKind(et), LLVMTypeKind::LLVMPointerTypeKind)
            {
                let elptr = LLVMBuildLoad(self.builder, self.ptr, cstr("elptr"));
                let mut index_expr = vec![index];
                Ok(ValueRef::new(
                    LLVMBuildGEP(self.builder, elptr, index_expr.as_mut_ptr(), 1, cstr("el")),
                    self.constant,
                    self.builder,
                ))
            }
            else
            {
                Err(type_error(pos, format!("Attempting to index, something which is not indexable")))
            }
        }
    }

    // If it is a ptr to something, return something as a value, if it is a pointer to a pointer, return the new pointer as a ptr
    pub fn deref(&self, pos: Pos) -> CompileResult<ValueRef>
    {
        if !self.is_pointer() {
            return Err(type_error(pos, format!("Attempting to dereference a value")));
        }

        unsafe {
            let et = LLVMGetElementType(LLVMTypeOf(self.ptr));
            if is_same_kind(LLVMGetTypeKind(et), LLVMTypeKind::LLVMStructTypeKind) ||
               is_same_kind(LLVMGetTypeKind(et), LLVMTypeKind::LLVMPointerTypeKind)
            {
                Ok(ValueRef::new(self.load(), self.constant, self.builder))
            }
            else
            {
                Err(type_error(pos, format!("Attempting to deref something which is not a pointer")))
            }
        }
    }

    pub unsafe fn copy(&self, ctx: &mut Context, pos: Pos) -> CompileResult<ValueRef>
    {
        let typ = self.get_element_type();
        if is_struct(typ)
        {
            let c = ValueRef::local(ctx.builder, typ);
            for i in 0..LLVMCountStructElementTypes(typ)
            {
                let src_ptr = try!(self.get_struct_element(i as u32, pos));
                let dst_ptr = try!(c.get_struct_element(i as u32, pos));
                let cv = ValueRef::new(src_ptr.load(), true, ctx.builder);
                try!(dst_ptr.store(ctx, cv, pos));
            }

            Ok(c)
        }
        else if is_array(typ)
        {
            let c = ValueRef::local(ctx.builder, typ);
            for i in 0..LLVMGetArrayLength(typ)
            {
                let index = const_int(ctx.context, i as u64);
                let src_ptr = try!(self.get_array_element(ctx, index, pos));
                let dst_ptr = try!(c.get_array_element(ctx, index, pos));
                let cv = ValueRef::new(src_ptr.load(), true, ctx.builder);
                try!(dst_ptr.store(ctx, cv, pos));
            }

            Ok(c)
        }
        else
        {
            let c = ValueRef::local(ctx.builder, typ);
            let cv = ValueRef::new(self.load(), true, ctx.builder);
            try!(c.store(ctx, cv, pos));
            Ok(c)
        }
    }
}
