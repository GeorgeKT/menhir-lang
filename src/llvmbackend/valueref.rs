use libc::c_uint;
use llvm::core::*;
use llvm::prelude::*;

use ast::*;
use bytecode::{ByteCodeProperty, Operand};
use super::context::Context;
use super::instructions::{const_uint, const_int, const_bool, copy, get_operand};

#[derive(Clone)]
pub struct ValueRef
{
    pub value: LLVMValueRef,
    pub typ: Type,
}


impl ValueRef
{
    pub fn new(value: LLVMValueRef, typ: Type) -> ValueRef
    {
        ValueRef{
            value: value,
            typ: typ,
        }
    }

    pub unsafe fn store(&self, ctx: &Context, val: &ValueRef)
    {
        let element_type = self.typ.get_pointer_element_type().unwrap_or_else(|| panic!("Store not allowed on type {}", self.typ));
        match *element_type
        {
            Type::Optional(ref inner) => {
                let is_nil_ptr = LLVMBuildStructGEP(ctx.builder, self.value, 0, cstr!("is_nil_ptr"));
                LLVMBuildStore(ctx.builder, const_bool(ctx, false), is_nil_ptr);
                let inner_ptr = LLVMBuildStructGEP(ctx.builder, self.value, 1, cstr!("inner_ptr"));
                if inner.pass_by_value() {
                    LLVMBuildStore(ctx.builder, val.load(ctx), inner_ptr);
                } else {
                    copy(ctx, inner_ptr, val.value, ctx.resolve_type(inner))
                }
            },

            _ => {
                if element_type.pass_by_value() {
                    LLVMBuildStore(ctx.builder, val.load(ctx), self.value);
                } else {
                    copy(ctx, self.value, val.value, ctx.resolve_type(element_type))
                }
            }
        }

    }

    pub fn load(&self, ctx: &Context) -> LLVMValueRef
    {
        if let Some(element_type) = self.typ.get_pointer_element_type() {
            match *element_type
            {
                Type::Optional(ref inner_type) => unsafe {
                    let inner_ptr = LLVMBuildStructGEP(ctx.builder, self.value, 1, cstr!("inner_ptr"));
                    if inner_type.pass_by_value() {
                        LLVMBuildLoad(ctx.builder, inner_ptr, cstr!("inner"))
                    } else {
                        inner_ptr
                    }
                },

                _ => unsafe {
                    if element_type.pass_by_value() {
                        LLVMBuildLoad(ctx.builder, self.value, cstr!("load"))
                    } else {
                        self.value
                    }
                }
            }
        } else {
            self.value
        }
    }

    pub fn address_of(&self) -> ValueRef
    {
        match self.typ
        {
            Type::Array(_) |
            Type::Slice(_) |
            Type::Struct(_) |
            Type::Sum(_) |
            Type::Func(_) |
            Type::Optional(_) |
            Type::Pointer(_) |
            Type::String => self.clone(),

            _ => panic!("Address of not allowed on value of type {}", self.typ)
        }
    }

    pub fn is_nil(&self, ctx: &Context) -> ValueRef
    {
        match self.typ
        {
            Type::Optional(_) => unsafe {
                let is_nil_ptr = LLVMBuildStructGEP(ctx.builder, self.value, 0, cstr!("is_nil_ptr"));
                ValueRef::new(
                    LLVMBuildLoad(ctx.builder, is_nil_ptr, cstr!("is_nil")),
                    Type::Bool
                )
            },

            _ => unsafe{
                ValueRef::new(const_bool(ctx, false), Type::Bool)
            }
        }
    }

    pub fn store_nil(&self, ctx: &Context)
    {
        let element_type = self.typ.get_pointer_element_type().unwrap_or_else(|| panic!("storenil not allowed on type {}", self.typ));
        match *element_type
        {
            Type::Optional(_) => unsafe {
                let is_nil_ptr = LLVMBuildStructGEP(ctx.builder, self.value, 0, cstr!("is_nil_ptr"));
                LLVMBuildStore(ctx.builder, const_bool(ctx, true), is_nil_ptr);
            },

            _ => panic!("storenil only allowed on optional type"),
        }
    }


    pub fn get_member_ptr(&self, ctx: &Context, index: &Operand) -> ValueRef
    {
        use std::ops::Deref;
        let zero_val = unsafe{const_int(ctx, 0)};

        let element_type = self.typ.get_pointer_element_type().unwrap_or_else(|| panic!("Load member not allowed on type {}", self.typ));
        match *element_type
        {
            Type::Array(ref at) => unsafe {
                let index = get_operand(ctx, index).load(ctx);
                let mut indices = vec![zero_val, index];
                ValueRef::new(
                    LLVMBuildGEP(ctx.builder, self.value, indices.as_mut_ptr(), 2, cstr!("member")),
                    ptr_type(at.element_type.clone())
                )
            },

            Type::Slice(ref st) => unsafe {
                let index = get_operand(ctx, index).load(ctx);
                let data_ptr = LLVMBuildLoad(ctx.builder, self.slice_data_ptr(ctx), cstr!("data_ptr"));
                let mut indices = vec![index];
                ValueRef::new(
                    LLVMBuildGEP(ctx.builder, data_ptr, indices.as_mut_ptr(), 1, cstr!("member")),
                    ptr_type(st.element_type.clone())
                )
            },

            Type::Struct(ref st) => unsafe {
                let index = match *index {
                    Operand::Int(v) => v as usize,
                    Operand::UInt(v) => v as usize,
                    _ => panic!("Struct member access has to be through an integer"),
                };

                ValueRef::new(
                    LLVMBuildStructGEP(ctx.builder, self.value, index as c_uint, cstr!("member")),
                    ptr_type(st.members[index].typ.clone())
                )
            },

            Type::Sum(ref st) => unsafe {
                let index = match *index {
                    Operand::Int(v) => v as usize,
                    Operand::UInt(v) => v as usize,
                    _ => panic!("Sum type member access has to be through an integer"),
                };

                let st_data_ptr = LLVMBuildStructGEP(ctx.builder, self.value, 1, cstr!("st_data_ptr"));
                let case_type = &st.cases[index].typ;
                let type_to_cast_to = LLVMPointerType(ctx.resolve_type(case_type), 0);
                ValueRef::new(
                    LLVMBuildBitCast(ctx.builder, st_data_ptr, type_to_cast_to, cstr!("st_member_ptr")),
                    ptr_type(case_type.clone())
                )
            },

            Type::Pointer(ref inner) => {
                ValueRef::new(
                    self.load(ctx),
                    inner.deref().clone()
                ).get_member_ptr(ctx, index)
            }


            _ => panic!("Load member not allowed on type {}", self.typ),
        }

    }

    pub fn store_member(&self, ctx: &Context, index: &Operand, value: &ValueRef)
    {
        let element_type = self.typ.get_pointer_element_type()
            .unwrap_or_else(|| panic!("Store member not allowed on type {}", self.typ));
        match *element_type
        {
            Type::Array(_) | Type::Struct(_) | Type::Slice(_) | Type::Pointer(_)  => unsafe {
                let member_ptr = self.get_member_ptr(ctx, index);
                member_ptr.store(ctx, value);
            },

            _ => panic!("Store member not allowed on type {}", self.typ),
        }

    }


    pub fn get_property(&self, ctx: &Context, prop: ByteCodeProperty) -> ValueRef
    {
        let element_type = self.typ.get_pointer_element_type()
            .unwrap_or_else(|| panic!("Get property not allowed on type {}", self.typ));

        match (element_type, prop)
        {
            (&Type::Array(ref a), ByteCodeProperty::Len) => unsafe {
                ValueRef::new(
                    const_uint(ctx, a.len),
                    Type::UInt
                )
            },

            (&Type::Slice(_), ByteCodeProperty::Len) => unsafe {
                let len_ptr = self.slice_len_ptr(ctx);
                ValueRef::new(
                    LLVMBuildLoad(ctx.builder, len_ptr, cstr!("len")),
                    Type::UInt
                )
            },

            (&Type::Sum(_), ByteCodeProperty::SumTypeIndex) => unsafe {
                let sti_ptr = LLVMBuildStructGEP(ctx.builder, self.value, 0, cstr!("sti_ptr"));
                ValueRef::new(
                    LLVMBuildLoad(ctx.builder, sti_ptr, cstr!("sti")),
                    Type::UInt
                )
            },

            _ => panic!("Get property not allowed")
        }
    }

    pub fn set_property(&self, ctx: &Context, prop: ByteCodeProperty, value: usize)
    {
        let element_type = self.typ.get_pointer_element_type()
            .unwrap_or_else(|| panic!("Set property not allowed on type {}", self.typ));

        match (element_type, prop)
        {
            (&Type::Sum(_), ByteCodeProperty::SumTypeIndex) => unsafe {
                let sti_ptr = LLVMBuildStructGEP(ctx.builder, self.value, 0, cstr!("sti_ptr"));
                LLVMBuildStore(ctx.builder, const_uint(ctx, value), sti_ptr);
            },

            _ => panic!("Set property not allowed")
        }
    }

    unsafe fn slice_data_ptr(&self, ctx: &Context) -> LLVMValueRef
    {
        LLVMBuildStructGEP(ctx.builder, self.value, 0, cstr!("slice_data_ptr"))
    }

    unsafe fn slice_len_ptr(&self, ctx: &Context) -> LLVMValueRef
    {
        LLVMBuildStructGEP(ctx.builder, self.value, 1, cstr!("slice_len_ptr"))
    }

    pub unsafe fn create_slice(&self, ctx: &Context, array: &ValueRef, start: &Operand, len: &Operand)
    {
        let inner_type = array.typ.get_pointer_element_type()
            .unwrap_or_else(|| panic!("Expecting an array or slice not a {}", array.typ));

        match *inner_type
        {
            Type::Array(_) | Type::Slice(_) => {
                let data_ptr = self.slice_data_ptr(ctx);
                let len_ptr = self.slice_len_ptr(ctx);
                let member_ptr = array.get_member_ptr(ctx, start);
                LLVMBuildStore(ctx.builder, member_ptr.value, data_ptr);
                LLVMBuildStore(ctx.builder, get_operand(ctx, len).load(ctx), len_ptr);
            }

            _ =>  panic!("Expecting an array type, not a {}", self.typ),
        }
    }
}
