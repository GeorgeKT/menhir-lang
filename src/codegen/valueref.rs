use llvm::prelude::*;
use llvm::core::*;
use llvm::{LLVMTypeKind, LLVMLinkage};
use codegen::cstr;
use codegen::context::Context;
use codegen::conversions::{is_same_kind};
use codegen::expressions::{const_int};
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

    pub fn store(&self, val: &ValueRef, pos: Pos) -> Result<ValueRef, CompileError>
    {
        if self.is_pointer()
        {
            if self.constant {
                return err(pos, ErrorType::ConstantModification);
            }
            unsafe {
                Ok(ValueRef::new(LLVMBuildStore(self.builder, val.load(), self.ptr), self.constant, self.builder))
            }
        }
        else
        {
            Err(type_error(pos, format!("Values cannot be set")))
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

    pub fn get_type(&self) -> LLVMTypeRef
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

    pub fn get_array_element(&self, ctx: &Context, index: ValueRef, pos: Pos) -> Result<ValueRef, CompileError>
    {
        if !self.is_pointer() {
            return Err(type_error(pos, format!("Attempting to index a value")));
        }

        unsafe {
            let et = LLVMGetElementType(LLVMTypeOf(self.ptr));
            if is_same_kind(LLVMGetTypeKind(et), LLVMTypeKind::LLVMArrayTypeKind)
            {
                let mut index_expr = vec![const_int(ctx.context, 0), index.load()];
                Ok(ValueRef::new(
                    LLVMBuildGEP(self.builder, self.ptr, index_expr.as_mut_ptr(), 2, cstr("el")),
                    self.constant,
                    self.builder,
                ))
            }
            else if is_same_kind(LLVMGetTypeKind(LLVMTypeOf(self.ptr)), LLVMTypeKind::LLVMArrayTypeKind)
            {
                let mut index_expr = vec![index.load()];
                Ok(ValueRef::new(
                    LLVMBuildGEP(self.builder, self.ptr, index_expr.as_mut_ptr(), 1, cstr("el")),
                    self.constant,
                    self.builder,
                ))
            }

            else if is_same_kind(LLVMGetTypeKind(et), LLVMTypeKind::LLVMPointerTypeKind)
            {
                let elptr = LLVMBuildLoad(self.builder, self.ptr, cstr("elptr"));
                let mut index_expr = vec![index.load()];
                Ok(ValueRef::new(
                    LLVMBuildGEP(self.builder, elptr, index_expr.as_mut_ptr(), 1, cstr("el")),
                    self.constant,
                    self.builder,
                ))
            }
            else
            {
                use codegen::type_name;
                println!("ptr: {}", type_name(LLVMTypeOf(self.ptr)));
                println!("et: {}", type_name(et));
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
            if is_same_kind(LLVMGetTypeKind(et), LLVMTypeKind::LLVMStructTypeKind)
            {
                Ok(ValueRef::new(self.load(), self.constant, self.builder))
            }
            else if is_same_kind(LLVMGetTypeKind(et), LLVMTypeKind::LLVMPointerTypeKind)
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
