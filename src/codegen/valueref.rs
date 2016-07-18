use llvm::prelude::*;
use llvm::core::*;
use llvm::{LLVMTypeKind, LLVMLinkage};
use codegen::{type_name, cstr};
use codegen::context::Context;
use codegen::conversions::{is_same_kind};
use codegen::expressions::{const_int};
use codegen::conversions::convert;
use compileerror::{Pos, CompileError, ErrorType, err, type_error};


#[derive(Debug, Clone)]
pub struct ValueRef
{
    ptr: LLVMValueRef,
    constant: bool,
    global: bool,
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
                global: false,
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
                global: true,
                builder: ctx.builder,
            }
        }
    }

    pub fn new(ptr: LLVMValueRef, constant: bool, builder: LLVMBuilderRef) -> ValueRef
    {
        ValueRef{
            ptr: ptr,
            constant: constant,
            global: false,
            builder: builder,
        }
    }

    pub fn is_pointer(&self) -> bool
    {
        unsafe {
            is_same_kind(LLVMGetTypeKind(LLVMTypeOf(self.ptr)), LLVMTypeKind::LLVMPointerTypeKind)
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

    pub unsafe fn store(&self, ctx: &Context, val: ValueRef, pos: Pos) -> Result<ValueRef, CompileError>
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
                    return err(pos, ErrorType::ExpectedConstExpr(format!("Global variables and constants must be initialized with a constant expression")));
                }
            }
            else
            {
                if self.constant {
                    return err(pos, ErrorType::ConstantModification);
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

    pub fn get_struct_element(&self, index: u32, pos: Pos) -> Result<ValueRef, CompileError>
    {
        if !self.is_pointer() {
            return Err(type_error(pos, format!("Attempting to get a struct element of a value")));
        }

        unsafe{
            let et = LLVMGetElementType(LLVMTypeOf(self.ptr));
            if is_same_kind(LLVMGetTypeKind(et), LLVMTypeKind::LLVMStructTypeKind)
            {
                Ok(ValueRef::new(
                    LLVMBuildStructGEP(self.builder, self.ptr, index, cstr("el")),
                    self.constant,
                    self.builder
                ))
            }
            else if is_same_kind(LLVMGetTypeKind(et), LLVMTypeKind::LLVMPointerTypeKind) &&
                    is_same_kind(LLVMGetTypeKind(LLVMGetElementType(et)), LLVMTypeKind::LLVMStructTypeKind)
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

    pub fn get_array_element(&self, ctx: &Context, index: LLVMValueRef, pos: Pos) -> Result<ValueRef, CompileError>
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
    pub fn deref(&self, pos: Pos) -> Result<ValueRef, CompileError>
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
}
