use libc::{c_char, c_uint};
use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::LLVMLinkage;

use super::context::Context;
use super::instructions::{const_bool, const_char, const_float, const_int, const_uint, copy, get_operand};
use crate::ast::*;
use crate::bytecode::{ByteCodeProperty, Constant, Operand};
use crate::compileerror::{code_gen_error, code_gen_result, CompileResult};
//use crate::llvmbackend::instructions::type_name;

#[derive(Clone)]
pub struct ValueRef {
    pub value: LLVMValueRef,
    pub typ: Type,
}

impl ValueRef {
    pub fn new(value: LLVMValueRef, typ: Type) -> ValueRef {
        ValueRef { value, typ }
    }

    pub unsafe fn from_const(ctx: &Context, cst: &Constant) -> CompileResult<ValueRef> {
        match cst {
            Constant::String(s) => ValueRef::const_string(ctx, s),
            Constant::Int(v, int_size) => Ok(ValueRef::new(const_int(ctx, *v), Type::Int(*int_size))),
            Constant::UInt(v, int_size) => Ok(ValueRef::new(const_uint(ctx, *v), Type::UInt(*int_size))),
            Constant::Float(v, float_size) => Ok(ValueRef::new(const_float(ctx, *v), Type::Float(*float_size))),
            Constant::Char(v) => Ok(ValueRef::new(const_char(ctx, *v), Type::Char)),
            Constant::Bool(v) => Ok(ValueRef::new(const_bool(ctx, *v), Type::Bool)),
            Constant::Array(elements) => ValueRef::const_array(ctx, elements),
            Constant::NullPtr(typ) => Ok(ValueRef::new(
                LLVMConstNull(ctx.resolve_type(typ)?),
                ptr_type(typ.clone()),
            )),
        }
    }

    unsafe fn const_string(ctx: &Context, s: &str) -> CompileResult<ValueRef> {
        let char_type = LLVMInt8TypeInContext(ctx.context);
        let glob = LLVMAddGlobal(
            ctx.module,
            LLVMArrayType(char_type, (s.len() + 1) as c_uint),
            cstr!("str_constant"),
        );
        LLVMSetLinkage(glob, LLVMLinkage::LLVMInternalLinkage);
        let const_string = LLVMConstStringInContext(
            ctx.context,
            s.as_bytes().as_ptr() as *const c_char,
            s.len() as c_uint,
            0,
        );
        LLVMSetInitializer(glob, const_string);

        let dst = ValueRef::new(
            LLVMBuildAlloca(ctx.builder, ctx.resolve_type(&Type::String)?, cstr!("str")),
            ptr_type(Type::String),
        );

        let string_data_ptr = dst.slice_data_ptr(ctx)?;
        LLVMBuildStore(
            ctx.builder,
            LLVMBuildBitCast(ctx.builder, glob, LLVMPointerType(char_type, 0), cstr!("str_ptr")),
            string_data_ptr.value,
        );

        let string_len_ptr = dst.slice_len_ptr(ctx)?;
        LLVMBuildStore(ctx.builder, const_uint(ctx, s.len() as u64), string_len_ptr.value);
        Ok(dst)
    }

    unsafe fn const_array(ctx: &Context, elements: &[Constant]) -> CompileResult<ValueRef> {
        let (element_type, array_type) = if let Some(el) = elements.first() {
            let et = el.get_type();
            (ctx.resolve_type(&et)?, array_type(et, elements.len()))
        } else {
            return code_gen_result("Empty arrays are not allowed in globals");
        };

        let glob = LLVMAddGlobal(
            ctx.module,
            LLVMArrayType(element_type, elements.len() as c_uint),
            cstr!("array_constant"),
        );
        LLVMSetLinkage(glob, LLVMLinkage::LLVMInternalLinkage);

        let mut array_data = Vec::with_capacity(elements.len());
        for e in elements {
            array_data.push(ValueRef::from_const(ctx, e)?.value);
        }

        let const_array = LLVMConstArray(element_type, array_data.as_mut_ptr(), array_data.len() as c_uint);
        LLVMSetInitializer(glob, const_array);

        Ok(ValueRef::new(glob, array_type))
    }

    pub unsafe fn store(&self, ctx: &Context, val: &ValueRef) -> CompileResult<()> {
        let Some(element_type) = self.typ.get_pointer_element_type() else {
            return code_gen_result(format!("Store not allowed on type {}", self.typ));
        };
        match element_type {
            Type::Optional(inner) => {
                let self_type = ctx.resolve_type(element_type)?;
                let dst_opt_flag_ptr =
                    LLVMBuildStructGEP2(ctx.builder, self_type, self.value, 0, cstr!("dst_opt_flag_ptr"));
                let dst_data_ptr = LLVMBuildStructGEP2(ctx.builder, self_type, self.value, 1, cstr!("dst_data_ptr"));
                if val.typ.is_pointer_to_optional() {
                    let val_type = val
                        .typ
                        .get_pointer_element_type()
                        .expect("val.typ must be a pointer type");
                    let val_type = ctx.resolve_type(val_type)?;
                    let src_opt_flag_ptr =
                        LLVMBuildStructGEP2(ctx.builder, val_type, val.value, 0, cstr!("src_opt_flag_ptr"));
                    let src_data_ptr = LLVMBuildStructGEP2(ctx.builder, val_type, val.value, 1, cstr!("src_data_ptr"));
                    let src_is_nil = LLVMBuildLoad2(
                        ctx.builder,
                        LLVMInt1TypeInContext(ctx.context),
                        src_opt_flag_ptr,
                        cstr!("src_is_nil"),
                    );
                    LLVMBuildStore(ctx.builder, src_is_nil, dst_opt_flag_ptr);
                    if inner.pass_by_value() {
                        let src_data_type = ctx.resolve_type(inner)?;
                        let src_data = LLVMBuildLoad2(ctx.builder, src_data_type, src_data_ptr, cstr!("src_data"));
                        LLVMBuildStore(ctx.builder, src_data, dst_data_ptr);
                        Ok(())
                    } else {
                        copy(ctx, dst_data_ptr, src_data_ptr, ctx.resolve_type(inner)?)
                    }
                } else {
                    LLVMBuildStore(ctx.builder, const_bool(ctx, true), dst_opt_flag_ptr);
                    if inner.pass_by_value() {
                        LLVMBuildStore(ctx.builder, val.load(ctx)?, dst_data_ptr);
                        Ok(())
                    } else {
                        copy(ctx, dst_data_ptr, val.value, ctx.resolve_type(inner)?)
                    }
                }
            }

            Type::Func(_) => {
                let func = val.load(ctx)?;
                LLVMBuildStore(ctx.builder, func, self.value);
                Ok(())
            }

            Type::Pointer(pt) => {
                let target = if val.typ == *pt.as_ref() {
                    self.load(ctx)?
                } else {
                    self.value
                };
                if pt.pass_by_value() {
                    LLVMBuildStore(ctx.builder, val.value, target);
                    Ok(())
                } else {
                    copy(ctx, target, val.value, ctx.resolve_type(pt)?)
                }
            }

            _ => {
                if element_type.pass_by_value() {
                    if self.typ.is_pointer_to(&val.typ) {
                        LLVMBuildStore(ctx.builder, val.value, self.value);
                    } else {
                        LLVMBuildStore(ctx.builder, val.load(ctx)?, self.value);
                    }
                    Ok(())
                } else {
                    copy(ctx, self.value, val.value, ctx.resolve_type(element_type)?)
                }
            }
        }
    }

    fn get_element_type(&self, ctx: &Context) -> CompileResult<LLVMTypeRef> {
        match &self.typ {
            Type::Pointer(inner) => ctx.resolve_type(inner),
            Type::Slice(s) => ctx.resolve_type(&s.element_type),
            Type::Array(s) => ctx.resolve_type(&s.element_type),
            Type::Optional(o) => ctx.resolve_type(o),
            _ => code_gen_result("Cannot get the element type of a non pointerish type"),
        }
    }

    pub fn load(&self, ctx: &Context) -> CompileResult<LLVMValueRef> {
        if let Some(element_type) = self.typ.get_pointer_element_type() {
            match element_type {
                Type::Optional(inner_type) => unsafe {
                    let self_type = ctx.resolve_type(element_type)?;
                    let inner_ptr = LLVMBuildStructGEP2(ctx.builder, self_type, self.value, 1, cstr!("inner_ptr"));
                    if inner_type.pass_by_value() {
                        Ok(LLVMBuildLoad2(
                            ctx.builder,
                            ctx.resolve_type(inner_type)?,
                            inner_ptr,
                            cstr!("inner"),
                        ))
                    } else {
                        Ok(inner_ptr)
                    }
                },

                Type::Func(_func_type) => unsafe {
                    Ok(LLVMBuildLoad2(
                        ctx.builder,
                        ctx.resolve_type(&element_type.ptr_of())?,
                        self.value,
                        cstr!("func_load"),
                    ))
                },
                _ => unsafe {
                    if element_type.pass_by_value() {
                        let typ = self.get_element_type(ctx)?;
                        Ok(LLVMBuildLoad2(ctx.builder, typ, self.value, cstr!("load")))
                    } else {
                        Ok(self.value)
                    }
                },
            }
        } else {
            Ok(self.value)
        }
    }

    pub fn address_of(&self) -> CompileResult<ValueRef> {
        match self.typ {
            Type::Array(_)
            | Type::Slice(_)
            | Type::Struct(_)
            | Type::Sum(_)
            | Type::Func(_)
            | Type::Optional(_)
            | Type::Pointer(_)
            | Type::String => Ok(self.clone()),

            _ => code_gen_result(format!("Address of not allowed on value of type {}", self.typ)),
        }
    }

    pub fn load_optional_flag(&self, ctx: &Context) -> CompileResult<ValueRef> {
        let typ = self.typ.get_pointer_element_type().unwrap_or(&self.typ);
        match *typ {
            Type::Optional(_) => unsafe {
                let self_type = ctx.resolve_type(typ)?;
                let opt_flag_ptr = LLVMBuildStructGEP2(ctx.builder, self_type, self.value, 0, cstr!("opt_flag_ptr"));
                Ok(ValueRef::new(
                    LLVMBuildLoad2(
                        ctx.builder,
                        LLVMInt1TypeInContext(ctx.context),
                        opt_flag_ptr,
                        cstr!("is_nil"),
                    ),
                    Type::Bool,
                ))
            },

            _ => code_gen_result(format!("is_nil not allowed on type {}", self.typ)),
        }
    }

    pub fn store_nil(&self, ctx: &Context) -> CompileResult<()> {
        let Some(element_type) = self.typ.get_pointer_element_type() else {
            return code_gen_result(format!("storenil not allowed on type {}", self.typ));
        };

        match element_type {
            Type::Optional(_) => unsafe {
                let self_type = ctx.resolve_type(element_type)?;
                let opt_flag_ptr = LLVMBuildStructGEP2(ctx.builder, self_type, self.value, 0, cstr!("opt_flag_ptr"));
                LLVMBuildStore(ctx.builder, const_bool(ctx, false), opt_flag_ptr);
                Ok(())
            },

            _ => code_gen_result("storenil only allowed on optional type"),
        }
    }

    pub fn get_member_ptr(&self, ctx: &mut Context, index: &Operand) -> CompileResult<ValueRef> {
        let Some(element_type) = self.typ.get_pointer_element_type() else {
            return code_gen_result(format!("Load member not allowed on type {}", self.typ));
        };

        match element_type {
            Type::Array(at) => unsafe {
                let mut indices = vec![get_operand(ctx, index)?.load(ctx)?];
                Ok(ValueRef::new(
                    LLVMBuildInBoundsGEP2(
                        ctx.builder,
                        ctx.resolve_type(&at.element_type)?,
                        self.value,
                        indices.as_mut_ptr(),
                        1,
                        cstr!("arraymember"),
                    ),
                    ptr_type(at.element_type.clone()),
                ))
            },

            Type::Slice(st) => unsafe {
                let data_ptr = self.slice_data_ptr(ctx)?;
                let mut indices = vec![get_operand(ctx, index)?.load(ctx)?];
                let element_ptr = LLVMBuildGEP2(
                    ctx.builder,
                    ctx.resolve_type(&st.element_type)?,
                    data_ptr.load(ctx)?,
                    indices.as_mut_ptr(),
                    1,
                    cstr!("slicemember"),
                );
                Ok(ValueRef::new(element_ptr, ptr_type(st.element_type.clone())))
            },

            Type::Struct(st) => unsafe {
                let index = match *index {
                    Operand::Const(Constant::Int(v, _)) => v as usize,
                    Operand::Const(Constant::UInt(v, _)) => v as usize,
                    _ => return code_gen_result("Struct member access has to be through an integer"),
                };

                let self_type = ctx.resolve_type(element_type)?;
                Ok(ValueRef::new(
                    LLVMBuildStructGEP2(
                        ctx.builder,
                        self_type,
                        self.value,
                        index as c_uint,
                        cstr!("structmember"),
                    ),
                    ptr_type(st.members[index].typ.clone()),
                ))
            },

            Type::Sum(st) => unsafe {
                let index = match *index {
                    Operand::Const(Constant::Int(v, _)) => v as usize,
                    Operand::Const(Constant::UInt(v, _)) => v as usize,
                    _ => return code_gen_result("Sum type member access has to be through an integer"),
                };

                let self_type = ctx.resolve_type(element_type)?;
                let st_data_ptr = LLVMBuildStructGEP2(ctx.builder, self_type, self.value, 1, cstr!("st_data_ptr"));
                let case_type = &st.cases[index].typ;
                let type_to_cast_to = LLVMPointerType(ctx.resolve_type(case_type)?, 0);
                Ok(ValueRef::new(
                    LLVMBuildBitCast(ctx.builder, st_data_ptr, type_to_cast_to, cstr!("st_member_ptr")),
                    ptr_type(case_type.clone()),
                ))
            },
            _ => unsafe {
                let index = get_operand(ctx, index)?.load(ctx)?;
                let mut indices = vec![index];
                Ok(ValueRef::new(
                    LLVMBuildGEP2(
                        ctx.builder,
                        self.get_element_type(ctx)?,
                        self.value,
                        indices.as_mut_ptr(),
                        1,
                        cstr!("member"),
                    ),
                    ptr_type(element_type.clone()),
                ))
            },
        }
    }

    pub fn store_member(&self, ctx: &mut Context, index: &Operand, value: &ValueRef) -> CompileResult<()> {
        unsafe {
            let member_ptr = self.get_member_ptr(ctx, index)?;
            member_ptr.store(ctx, value)
        }
    }

    pub fn get_property(&self, ctx: &Context, prop: ByteCodeProperty) -> CompileResult<ValueRef> {
        let Some(element_type) = self.typ.get_pointer_element_type() else {
            return code_gen_result(format!("Get property not allowed on type {}", self.typ));
        };

        let native_uint_type = ctx.target_machine.target.native_uint_type.clone();
        Ok(match (element_type, prop) {
            (Type::Array(a), ByteCodeProperty::Len) => unsafe {
                ValueRef::new(const_uint(ctx, a.len as u64), native_uint_type)
            },

            (&Type::Slice(_), ByteCodeProperty::Len) | (&Type::String, ByteCodeProperty::Len) => unsafe {
                let len_ptr = self.slice_len_ptr(ctx)?;
                ValueRef::new(
                    LLVMBuildLoad2(ctx.builder, ctx.native_uint_type()?, len_ptr.value, cstr!("len")),
                    native_uint_type,
                )
            },

            (Type::Slice(st), ByteCodeProperty::Data) => unsafe {
                let data_ptr = self.slice_data_ptr(ctx)?;
                ValueRef::new(
                    LLVMBuildLoad2(
                        ctx.builder,
                        ctx.resolve_type(&st.element_type)?,
                        data_ptr.value,
                        cstr!("data"),
                    ),
                    st.element_type.ptr_of(),
                )
            },

            (&Type::String, ByteCodeProperty::Data) => unsafe {
                let data_ptr = self.slice_data_ptr(ctx)?;
                ValueRef::new(
                    LLVMBuildLoad2(ctx.builder, self.get_element_type(ctx)?, data_ptr.value, cstr!("data")),
                    ptr_type(Type::UInt(IntSize::I8)),
                )
            },

            (&Type::Sum(_), ByteCodeProperty::SumTypeIndex) => unsafe {
                let self_type = ctx.resolve_type(element_type)?;
                let sti_ptr = LLVMBuildStructGEP2(ctx.builder, self_type, self.value, 0, cstr!("sti_ptr"));
                ValueRef::new(
                    LLVMBuildLoad2(ctx.builder, ctx.native_uint_type()?, sti_ptr, cstr!("sti")),
                    native_uint_type,
                )
            },

            _ => return code_gen_result("Get property not allowed"),
        })
    }

    pub fn set_property(&self, ctx: &Context, prop: ByteCodeProperty, value: usize) -> CompileResult<()> {
        let Some(element_type) = self.typ.get_pointer_element_type() else {
            return code_gen_result(format!("Set property not allowed on type {}", self.typ));
        };
        match (element_type, prop) {
            (&Type::Sum(_), ByteCodeProperty::SumTypeIndex) => unsafe {
                let self_type = ctx.resolve_type(element_type)?;
                let sti_ptr = LLVMBuildStructGEP2(ctx.builder, self_type, self.value, 0, cstr!("sti_ptr"));
                LLVMBuildStore(ctx.builder, const_uint(ctx, value as u64), sti_ptr);
                Ok(())
            },

            _ => code_gen_result("Set property not allowed"),
        }
    }

    unsafe fn slice_data_ptr(&self, ctx: &Context) -> CompileResult<ValueRef> {
        let Some(inner_type) = self.typ.get_pointer_element_type() else {
            return code_gen_result(format!("Expecting a pointer not a {}", self.typ));
        };

        let vr = LLVMBuildStructGEP2(
            ctx.builder,
            ctx.resolve_type(inner_type)?,
            self.value,
            0,
            cstr!("slice_data_ptr"),
        );
        let element_type = inner_type
            .get_element_type()
            .ok_or_else(|| code_gen_error("Expecting an element type"))?;
        Ok(ValueRef::new(vr, ptr_type(ptr_type(element_type))))
    }

    unsafe fn slice_len_ptr(&self, ctx: &Context) -> CompileResult<ValueRef> {
        let Some(inner_type) = self.typ.get_pointer_element_type() else {
            return code_gen_result(format!("Expecting a pointer not a {}", self.typ));
        };

        let vr = LLVMBuildStructGEP2(
            ctx.builder,
            ctx.resolve_type(inner_type)?,
            self.value,
            1,
            cstr!("slice_len_ptr"),
        );
        Ok(ValueRef::new(
            vr,
            ptr_type(Type::UInt(ctx.target_machine.target.int_size)),
        ))
    }

    pub unsafe fn create_slice(
        &self,
        ctx: &mut Context,
        data: &ValueRef,
        offset: &Operand,
        len: &Operand,
    ) -> CompileResult<()> {
        let data_ptr = self.slice_data_ptr(ctx)?;
        let len_ptr = self.slice_len_ptr(ctx)?;
        let len = get_operand(ctx, len)?;
        let member_ptr = data.get_member_ptr(ctx, offset)?;
        LLVMBuildStore(ctx.builder, member_ptr.value, data_ptr.value);
        LLVMBuildStore(ctx.builder, len.load(ctx)?, len_ptr.value);
        Ok(())
    }
}
