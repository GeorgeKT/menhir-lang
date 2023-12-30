use std::ops::Deref;

use libc::{c_char, c_uint};
use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::LLVMLinkage;
use llvm_sys::LLVMTypeKind;

use super::context::Context;
use super::operand::{const_bool, const_char, const_float, const_int, const_uint, copy};
use crate::ast::*;
use crate::compileerror::{code_gen_error, code_gen_result, CompileResult};
use crate::lazycode::OPTIONAL_DATA_IDX;
use crate::lazycode::{ByteCodeProperty, Constant};
//use crate::llvmbackend::instructions::type_name;

#[derive(Debug, Clone)]
pub struct ValueRef {
    pub value: LLVMValueRef,
    pub allocated: bool,
    pub typ: Type,
}

unsafe fn build_load(
    builder: LLVMBuilderRef,
    typ: LLVMTypeRef,
    pointer: LLVMValueRef,
    name: *const ::libc::c_char,
) -> LLVMValueRef {
    if LLVMGetTypeKind(LLVMTypeOf(pointer)) != LLVMTypeKind::LLVMPointerTypeKind {
        panic!("ICE: attempting load on a non pointer type");
    }

    LLVMBuildLoad2(builder, typ, pointer, name)
}

const SUM_TYPE_TAG_IDX: u32 = 0;
const SUM_TYPE_DATA_IDX: u32 = 1;

impl ValueRef {
    pub fn new(value: LLVMValueRef, typ: Type) -> ValueRef {
        ValueRef {
            value,
            allocated: false,
            typ,
        }
    }

    pub fn allocated(value: LLVMValueRef, typ: Type) -> ValueRef {
        ValueRef {
            value,
            allocated: true,
            typ,
        }
    }

    /// If stack allocated and underlying type is pass_by_value then load it otherwise return a
    /// clone of this ValueRef
    pub fn get_value(&self, ctx: &Context) -> CompileResult<ValueRef> {
        if let Type::Pointer(it) = &self.typ {
            if it.pass_by_value() && self.allocated {
                self.load(ctx)
            } else {
                Ok(self.clone())
            }
        } else if self.typ.pass_by_value() {
            self.load(ctx)
        } else {
            Ok(self.clone())
        }
    }

    pub fn const_uint(ctx: &Context, value: u64) -> ValueRef {
        ValueRef::new(
            unsafe { const_uint(ctx, value) },
            ctx.target_machine.target.native_uint_type.clone(),
        )
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

    pub unsafe fn const_global(ctx: &Context, cst: &Constant) -> CompileResult<ValueRef> {
        match cst {
            Constant::String(s) => {
                let char_type = LLVMInt8TypeInContext(ctx.context);
                let str_data = ValueRef::const_string_data(ctx, s)?;
                let mut data = [
                    LLVMConstPointerCast(str_data, LLVMPointerType(char_type, 0)),
                    const_uint(ctx, s.len() as u64),
                ];
                let cs = LLVMConstStructInContext(ctx.context, data.as_mut_ptr(), 2, 0);
                Ok(ValueRef::new(cs, Type::String))
            }
            _ => ValueRef::from_const(ctx, cst),
        }
    }

    unsafe fn const_string_data(ctx: &Context, s: &str) -> CompileResult<LLVMValueRef> {
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
        Ok(glob)
    }

    unsafe fn const_string(ctx: &Context, s: &str) -> CompileResult<ValueRef> {
        let char_type = LLVMInt8TypeInContext(ctx.context);
        let glob = ValueRef::const_string_data(ctx, s)?;
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
                    let src_is_nil = build_load(
                        ctx.builder,
                        ctx.native_uint_type()?,
                        src_opt_flag_ptr,
                        cstr!("src_is_nil"),
                    );
                    LLVMBuildStore(ctx.builder, src_is_nil, dst_opt_flag_ptr);
                    if inner.pass_by_value() {
                        let src_data_type = ctx.resolve_type(inner)?;
                        let src_data = build_load(ctx.builder, src_data_type, src_data_ptr, cstr!("src_data"));
                        LLVMBuildStore(ctx.builder, src_data, dst_data_ptr);
                        Ok(())
                    } else {
                        copy(ctx, dst_data_ptr, src_data_ptr, ctx.resolve_type(inner)?)
                    }
                } else {
                    LLVMBuildStore(ctx.builder, const_uint(ctx, 0), dst_opt_flag_ptr);
                    if inner.pass_by_value() {
                        LLVMBuildStore(ctx.builder, val.load(ctx)?.value, dst_data_ptr);
                        Ok(())
                    } else {
                        copy(ctx, dst_data_ptr, val.value, ctx.resolve_type(inner)?)
                    }
                }
            }

            Type::Func(_) => {
                let func = val.load(ctx)?;
                LLVMBuildStore(ctx.builder, func.value, self.value);
                Ok(())
            }

            Type::Pointer(pt) => {
                let target = if val.typ == *pt.as_ref() {
                    self.load(ctx)?
                } else {
                    self.clone()
                };
                if self.typ.is_pointer_to(&val.typ) {
                    LLVMBuildStore(ctx.builder, val.value, target.value);
                } else {
                    LLVMBuildStore(ctx.builder, val.load(ctx)?.value, target.value);
                }
                Ok(())
            }

            _ => {
                if element_type.pass_by_value() {
                    if self.typ.is_pointer_to(&val.typ) {
                        LLVMBuildStore(ctx.builder, val.value, self.value);
                    } else {
                        LLVMBuildStore(ctx.builder, val.load(ctx)?.value, self.value);
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

    pub fn load(&self, ctx: &Context) -> CompileResult<ValueRef> {
        if let Some(element_type) = self.typ.get_pointer_element_type() {
            match element_type {
                Type::Optional(inner_type) => unsafe {
                    let self_type = ctx.resolve_type(element_type)?;
                    let inner_ptr = LLVMBuildStructGEP2(ctx.builder, self_type, self.value, 1, cstr!("inner_ptr"));
                    if inner_type.pass_by_value() {
                        let load = build_load(ctx.builder, ctx.resolve_type(inner_type)?, inner_ptr, cstr!("inner"));
                        Ok(ValueRef::new(load, inner_type.deref().clone()))
                    } else {
                        Ok(ValueRef::new(inner_ptr, inner_type.ptr_of()))
                    }
                },

                Type::Func(_func_type) => unsafe {
                    let load = build_load(
                        ctx.builder,
                        ctx.resolve_type(&element_type.ptr_of())?,
                        self.value,
                        cstr!("func_load"),
                    );
                    Ok(ValueRef::new(load, element_type.clone()))
                },
                _ => unsafe {
                    if element_type.pass_by_value() {
                        let typ = self.get_element_type(ctx)?;
                        Ok(ValueRef::new(
                            build_load(ctx.builder, typ, self.value, cstr!("load")),
                            element_type.clone(),
                        ))
                    } else {
                        Ok(self.clone())
                    }
                },
            }
        } else {
            Ok(self.clone())
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

    pub fn get_member_ptr_static(&self, ctx: &mut Context, index: usize) -> CompileResult<ValueRef> {
        let Some(element_type) = self.typ.get_element_type() else {
            return code_gen_result(format!("Get member not allowed on type {}", self.typ));
        };

        match &element_type {
            Type::Struct(st) => unsafe {
                let self_type = ctx.resolve_type(&element_type)?;
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
                let self_type = ctx.resolve_type(&element_type)?;
                let st_data_ptr = LLVMBuildStructGEP2(
                    ctx.builder,
                    self_type,
                    self.value,
                    SUM_TYPE_DATA_IDX,
                    cstr!("st_data_ptr"),
                );
                if let Some(case_type) = &st.cases[index].typ {
                    let type_to_cast_to = LLVMPointerType(ctx.resolve_type(case_type)?, 0);
                    Ok(ValueRef::new(
                        LLVMBuildBitCast(ctx.builder, st_data_ptr, type_to_cast_to, cstr!("st_member_ptr")),
                        ptr_type(case_type.clone()),
                    ))
                } else {
                    code_gen_result(format!("Cannot get member pointer to {}", st.cases[index].name))
                }
            },
            Type::Result(rt) => unsafe {
                let self_type = ctx.resolve_type(&element_type)?;
                let st_data_ptr = LLVMBuildStructGEP2(
                    ctx.builder,
                    self_type,
                    self.value,
                    SUM_TYPE_DATA_IDX,
                    cstr!("rt_data_ptr"),
                );
                let case_type = if index == 0 { &rt.ok_typ } else { &rt.err_typ };
                let type_to_cast_to = LLVMPointerType(ctx.resolve_type(case_type)?, 0);
                Ok(ValueRef::new(
                    LLVMBuildBitCast(ctx.builder, st_data_ptr, type_to_cast_to, cstr!("rt_member_ptr")),
                    ptr_type(case_type.clone()),
                ))
            },

            Type::Optional(ot) => unsafe {
                let self_type = ctx.resolve_type(&element_type)?;
                let opt_data_ptr =
                    LLVMBuildStructGEP2(ctx.builder, self_type, self.value, index as u32, cstr!("opt_data_ptr"));

                assert!(index == OPTIONAL_DATA_IDX as usize);
                Ok(ValueRef::new(opt_data_ptr, ptr_type(ot.deref().clone())))
            },

            Type::Range(int_type) => unsafe {
                let self_type = ctx.resolve_type(&element_type)?;
                Ok(ValueRef::new(
                    LLVMBuildStructGEP2(
                        ctx.builder,
                        self_type,
                        self.value,
                        index as c_uint,
                        cstr!("rangemember"),
                    ),
                    ptr_type(int_type.deref().clone()),
                ))
            },
            _ => self.get_member_ptr(ctx, &ValueRef::const_uint(ctx, index as u64)),
        }
    }

    pub fn get_member_ptr(&self, ctx: &mut Context, index: &ValueRef) -> CompileResult<ValueRef> {
        let Some(element_type) = self.typ.get_element_type() else {
            return code_gen_result(format!("Load member not allowed on type {}", self.typ));
        };

        match &element_type {
            Type::Pointer(_pt) => {
                let load = self.load(ctx)?;
                load.get_member_ptr(ctx, index)
            }

            Type::Array(at) => unsafe {
                let mut indices = vec![index.load(ctx)?.value];
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
                let mut indices = vec![index.load(ctx)?.value];
                let element_ptr = LLVMBuildGEP2(
                    ctx.builder,
                    ctx.resolve_type(&st.element_type)?,
                    data_ptr.load(ctx)?.value,
                    indices.as_mut_ptr(),
                    1,
                    cstr!("slicemember"),
                );
                Ok(ValueRef::new(element_ptr, ptr_type(st.element_type.clone())))
            },

            _ => unsafe {
                let index = index.load(ctx)?;
                let mut indices = vec![index.value];
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

    pub fn get_property(&self, ctx: &mut Context, prop: ByteCodeProperty) -> CompileResult<ValueRef> {
        let Some(element_type) = self.typ.get_pointer_element_type() else {
            return code_gen_result(format!("Get property not allowed on type {}", self.typ));
        };

        let native_uint_type = ctx.target_machine.target.native_uint_type.clone();
        Ok(match (element_type, prop) {
            (Type::Array(a), ByteCodeProperty::Len) => unsafe {
                ValueRef::new(const_uint(ctx, a.len as u64), native_uint_type)
            },

            (Type::Slice(_), ByteCodeProperty::Len) | (Type::String, ByteCodeProperty::Len) => unsafe {
                let len_ptr = self.slice_len_ptr(ctx)?;
                ValueRef::new(
                    build_load(ctx.builder, ctx.native_uint_type()?, len_ptr.value, cstr!("len")),
                    native_uint_type,
                )
            },

            (Type::Slice(st), ByteCodeProperty::Data) => unsafe {
                let data_ptr = self.slice_data_ptr(ctx)?;
                ValueRef::new(
                    build_load(
                        ctx.builder,
                        ctx.resolve_type(&st.element_type)?,
                        data_ptr.value,
                        cstr!("data"),
                    ),
                    st.element_type.ptr_of(),
                )
            },

            (Type::String, ByteCodeProperty::Data) => unsafe {
                let data_ptr = self.slice_data_ptr(ctx)?;
                let et = data_ptr.get_element_type(ctx)?;
                ValueRef::new(
                    build_load(ctx.builder, et, data_ptr.value, cstr!("data")),
                    ptr_type(Type::UInt(IntSize::I8)),
                )
            },

            (Type::Array(_), ByteCodeProperty::Data) => self.get_member_ptr_static(ctx, 0)?,

            (Type::Sum(_), ByteCodeProperty::SumTypeIndex) => unsafe {
                let self_type = ctx.resolve_type(element_type)?;
                let sti_ptr =
                    LLVMBuildStructGEP2(ctx.builder, self_type, self.value, SUM_TYPE_TAG_IDX, cstr!("sti_ptr"));
                ValueRef::new(
                    build_load(ctx.builder, ctx.native_uint_type()?, sti_ptr, cstr!("sti")),
                    native_uint_type,
                )
            },

            (Type::Result(_), ByteCodeProperty::SumTypeIndex) => unsafe {
                let self_type = ctx.resolve_type(element_type)?;
                let rti_ptr =
                    LLVMBuildStructGEP2(ctx.builder, self_type, self.value, SUM_TYPE_TAG_IDX, cstr!("rti_ptr"));
                ValueRef::new(
                    build_load(ctx.builder, ctx.native_uint_type()?, rti_ptr, cstr!("rti")),
                    native_uint_type,
                )
            },

            (Type::Optional(_), ByteCodeProperty::SumTypeIndex) => unsafe {
                let self_type = ctx.resolve_type(element_type)?;
                let rti_ptr =
                    LLVMBuildStructGEP2(ctx.builder, self_type, self.value, SUM_TYPE_TAG_IDX, cstr!("rti_ptr"));
                ValueRef::new(
                    build_load(ctx.builder, ctx.native_uint_type()?, rti_ptr, cstr!("rti")),
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
                let sti_ptr =
                    LLVMBuildStructGEP2(ctx.builder, self_type, self.value, SUM_TYPE_TAG_IDX, cstr!("sti_ptr"));
                LLVMBuildStore(ctx.builder, const_uint(ctx, value as u64), sti_ptr);
                Ok(())
            },
            (Type::Result(_), ByteCodeProperty::SumTypeIndex) => unsafe {
                let self_type = ctx.resolve_type(element_type)?;
                let rti_ptr =
                    LLVMBuildStructGEP2(ctx.builder, self_type, self.value, SUM_TYPE_TAG_IDX, cstr!("rti_ptr"));
                LLVMBuildStore(ctx.builder, const_uint(ctx, value as u64), rti_ptr);
                Ok(())
            },
            (Type::Optional(_), ByteCodeProperty::SumTypeIndex) => unsafe {
                let self_type = ctx.resolve_type(element_type)?;
                let sti_ptr =
                    LLVMBuildStructGEP2(ctx.builder, self_type, self.value, SUM_TYPE_TAG_IDX, cstr!("rti_ptr"));
                LLVMBuildStore(ctx.builder, const_uint(ctx, value as u64), sti_ptr);
                Ok(())
            },
            _ => code_gen_result("Set property not allowed"),
        }
    }

    pub unsafe fn slice_data_ptr(&self, ctx: &Context) -> CompileResult<ValueRef> {
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

    pub unsafe fn slice_len_ptr(&self, ctx: &Context) -> CompileResult<ValueRef> {
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
}
