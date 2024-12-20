use libc::*;
use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::LLVMType;
use std::ptr;

use super::target::TargetMachine;
use crate::ast::*;
use crate::compileerror::CompileError::CodeGeneration;
use crate::compileerror::{code_gen_result, CompileResult};

unsafe fn string_to_llvm_type(context: LLVMContextRef, target_machine: &TargetMachine) -> CompileResult<LLVMTypeRef> {
    struct_to_llvm_type(
        context,
        target_machine,
        &string_type_representation(target_machine.target.int_size),
    )
}

unsafe fn slice_to_llvm_type(
    context: LLVMContextRef,
    target_machine: &TargetMachine,
    slice_type: &SliceType,
) -> CompileResult<LLVMTypeRef> {
    let element_type = to_llvm_type(context, target_machine, &slice_type.element_type)?;
    let mut member_types = [
        LLVMPointerType(element_type, 0),              // Pointer to data
        native_llvm_int_type(context, target_machine), // Length of string
    ];
    Ok(LLVMStructTypeInContext(
        context,
        member_types.as_mut_ptr(),
        member_types.len() as c_uint,
        0,
    ))
}

unsafe fn array_to_llvm_type(
    context: LLVMContextRef,
    target_machine: &TargetMachine,
    at: &ArrayType,
) -> CompileResult<LLVMTypeRef> {
    let element_type = to_llvm_type(context, target_machine, &at.element_type)?;
    Ok(LLVMArrayType(element_type, at.len as c_uint))
}

unsafe fn create_sum_type(
    context: LLVMContextRef,
    target_machine: &TargetMachine,
    largest_case_type: LLVMTypeRef,
) -> LLVMTypeRef {
    let mut member_types = vec![native_llvm_int_type(context, target_machine), largest_case_type];
    LLVMStructTypeInContext(context, member_types.as_mut_ptr(), member_types.len() as c_uint, 0)
}

unsafe fn sum_type_to_llvm_type(
    context: LLVMContextRef,
    target_machine: &TargetMachine,
    st: &SumType,
) -> CompileResult<LLVMTypeRef> {
    // Calculate the biggest type
    let mut largest_type = ptr::null_mut::<LLVMType>();
    for c in &st.cases {
        if let Some(ct) = &c.typ {
            let case_typ = to_llvm_type(context, target_machine, ct)?;
            if largest_type.is_null()
                || target_machine.size_of_type(case_typ) > target_machine.size_of_type(largest_type)
            {
                largest_type = case_typ;
            }
        }
    }

    // Use the largest type, we will cast to the other case types
    Ok(create_sum_type(context, target_machine, largest_type))
}

unsafe fn result_type_to_llvm_type(
    context: LLVMContextRef,
    target_machine: &TargetMachine,
    rt: &ResultType,
) -> CompileResult<LLVMTypeRef> {
    match (&rt.ok_typ, &rt.err_typ) {
        (Type::Void, Type::Void) => Err(CodeGeneration(format!(
            "Type void ! void has no runtime size and cannot be used"
        ))),

        (Type::Void, _) => {
            let err_typ = to_llvm_type(context, target_machine, &rt.err_typ)?;
            Ok(create_sum_type(context, target_machine, err_typ))
        }

        (_, Type::Void) => {
            let ok_typ = to_llvm_type(context, target_machine, &rt.ok_typ)?;
            Ok(create_sum_type(context, target_machine, ok_typ))
        }

        (_, _) => {
            let ok_typ = to_llvm_type(context, target_machine, &rt.ok_typ)?;
            let err_typ = to_llvm_type(context, target_machine, &rt.err_typ)?;
            let largest = if target_machine.size_of_type(ok_typ) > target_machine.size_of_type(err_typ) {
                ok_typ
            } else {
                err_typ
            };

            Ok(create_sum_type(context, target_machine, largest))
        }
    }
}

unsafe fn func_to_llvm_type(
    context: LLVMContextRef,
    target_machine: &TargetMachine,
    ft: &FuncType,
) -> CompileResult<LLVMTypeRef> {
    let mut llvm_arg_types = Vec::with_capacity(ft.args.len());
    for arg in &ft.args {
        let mut arg_type = to_llvm_type(context, target_machine, &arg.typ)?;
        if !arg.typ.pass_by_value() {
            arg_type = LLVMPointerType(arg_type, 0);
        }
        llvm_arg_types.push(arg_type);
    }

    let ret = to_llvm_type(context, target_machine, &ft.return_type)?;
    let nft = LLVMFunctionType(ret, llvm_arg_types.as_mut_ptr(), ft.args.len() as c_uint, 0);

    Ok(nft)
}

unsafe fn struct_to_llvm_type(
    context: LLVMContextRef,
    target_machine: &TargetMachine,
    st: &StructType,
) -> CompileResult<LLVMTypeRef> {
    let mut llvm_member_types = Vec::with_capacity(st.members.len());
    for m in &st.members {
        llvm_member_types.push(to_llvm_type(context, target_machine, &m.typ)?);
    }
    Ok(LLVMStructTypeInContext(
        context,
        llvm_member_types.as_mut_ptr(),
        llvm_member_types.len() as c_uint,
        0,
    ))
}

unsafe fn optional_to_llvm_type(
    context: LLVMContextRef,
    target_machine: &TargetMachine,
    inner: &Type,
) -> CompileResult<LLVMTypeRef> {
    let inner = to_llvm_type(context, target_machine, inner)?;
    Ok(create_sum_type(context, target_machine, inner))
}

unsafe fn range_to_llvm_type(
    context: LLVMContextRef,
    target_machine: &TargetMachine,
    range_int_type: &Type,
) -> CompileResult<LLVMTypeRef> {
    let int_type = to_llvm_type(context, target_machine, range_int_type)?;
    let mut member_types = [int_type, int_type];
    Ok(LLVMStructTypeInContext(
        context,
        member_types.as_mut_ptr(),
        member_types.len() as c_uint,
        0,
    ))
}

pub unsafe fn native_llvm_int_type(context: LLVMContextRef, target_machine: &TargetMachine) -> LLVMTypeRef {
    llvm_int_type(context, target_machine.target.int_size)
}

pub unsafe fn llvm_int_type(context: LLVMContextRef, int_size: IntSize) -> LLVMTypeRef {
    match int_size {
        IntSize::I8 => LLVMInt8TypeInContext(context),
        IntSize::I16 => LLVMInt16TypeInContext(context),
        IntSize::I32 => LLVMInt32TypeInContext(context),
        IntSize::I64 => LLVMInt64TypeInContext(context),
    }
}

pub unsafe fn to_llvm_type(
    context: LLVMContextRef,
    target_machine: &TargetMachine,
    typ: &Type,
) -> CompileResult<LLVMTypeRef> {
    match typ {
        Type::Void => Ok(LLVMVoidTypeInContext(context)),
        Type::Int(IntSize::I8) | Type::UInt(IntSize::I8) => Ok(LLVMInt8TypeInContext(context)),
        Type::Int(IntSize::I16) | Type::UInt(IntSize::I16) => Ok(LLVMInt16TypeInContext(context)),
        Type::Char | Type::Int(IntSize::I32) | Type::UInt(IntSize::I32) => Ok(LLVMInt32TypeInContext(context)),
        Type::Int(IntSize::I64) | Type::UInt(IntSize::I64) => Ok(LLVMInt64TypeInContext(context)),
        Type::Enum(_) => Ok(native_llvm_int_type(context, target_machine)),
        Type::Bool => Ok(LLVMInt1TypeInContext(context)),
        Type::Float(FloatSize::F32) => Ok(LLVMFloatTypeInContext(context)),
        Type::Float(FloatSize::F64) => Ok(LLVMDoubleTypeInContext(context)),
        Type::Pointer(inner) => Ok(LLVMPointerType(to_llvm_type(context, target_machine, inner)?, 0)),
        Type::Array(at) => array_to_llvm_type(context, target_machine, at),
        Type::Slice(st) => slice_to_llvm_type(context, target_machine, st),
        Type::String => string_to_llvm_type(context, target_machine),
        Type::Func(ft) => func_to_llvm_type(context, target_machine, ft),
        Type::Struct(st) => struct_to_llvm_type(context, target_machine, st),
        Type::Sum(st) => sum_type_to_llvm_type(context, target_machine, st),
        Type::Optional(ot) => optional_to_llvm_type(context, target_machine, ot),
        Type::Result(rt) => result_type_to_llvm_type(context, target_machine, rt),
        Type::Range(int_type) => range_to_llvm_type(context, target_machine, int_type),
        Type::Generic(_) => {
            code_gen_result("Internal Compiler Error: All generic types must have been resolved before code generation")
        }
        Type::Unresolved(_) => {
            code_gen_result("Internal Compiler Error: All types must be resolved before code generation")
        }
        Type::Unknown => code_gen_result("Internal Compiler Error: all types must be known before code generation"),
        Type::SelfType => code_gen_result("Internal Compiler Error: self type must be known at this point"),
        Type::Interface(_) => code_gen_result("Internal Compiler Error: interface type must be known at this point"),
    }
}
