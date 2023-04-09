use libc::*;
use llvm::core::*;
use llvm::prelude::*;
use llvm_sys::LLVMType;
use std::ptr;

use super::target::TargetMachine;
use crate::ast::*;

unsafe fn string_to_llvm_type(context: LLVMContextRef, target_machine: &TargetMachine) -> LLVMTypeRef {
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
) -> LLVMTypeRef {
    let element_type = to_llvm_type(context, target_machine, &slice_type.element_type);
    let mut member_types = vec![
        LLVMPointerType(element_type, 0),              // Pointer to data
        native_llvm_int_type(context, target_machine), // Length of string
    ];
    LLVMStructTypeInContext(context, member_types.as_mut_ptr(), member_types.len() as c_uint, 0)
}

unsafe fn array_to_llvm_type(context: LLVMContextRef, target_machine: &TargetMachine, at: &ArrayType) -> LLVMTypeRef {
    let element_type = to_llvm_type(context, target_machine, &at.element_type);
    LLVMArrayType(element_type, at.len as c_uint)
}

unsafe fn sum_type_to_llvm_type(context: LLVMContextRef, target_machine: &TargetMachine, st: &SumType) -> LLVMTypeRef {
    let mut member_types = vec![native_llvm_int_type(context, target_machine)]; // first entry is the tag

    // Calculate the biggest type
    let mut largest_type = ptr::null_mut::<LLVMType>();
    for c in &st.cases {
        let case_typ = to_llvm_type(context, target_machine, &c.typ);
        if largest_type.is_null() || target_machine.size_of_type(case_typ) > target_machine.size_of_type(largest_type) {
            largest_type = case_typ;
        }
    }

    // Use the largest type, we will cast to the other case types
    member_types.push(largest_type);
    LLVMStructTypeInContext(context, member_types.as_mut_ptr(), member_types.len() as c_uint, 0)
}

unsafe fn func_to_llvm_type(context: LLVMContextRef, target_machine: &TargetMachine, ft: &FuncType) -> LLVMTypeRef {
    let mut llvm_arg_types = Vec::with_capacity(ft.args.len());
    for arg in &ft.args {
        llvm_arg_types.push(to_llvm_type(context, target_machine, arg));
    }

    LLVMPointerType(
        LLVMFunctionType(
            to_llvm_type(context, target_machine, &ft.return_type),
            llvm_arg_types.as_mut_ptr(),
            ft.args.len() as c_uint,
            0,
        ),
        0,
    )
}

unsafe fn struct_to_llvm_type(context: LLVMContextRef, target_machine: &TargetMachine, st: &StructType) -> LLVMTypeRef {
    let mut llvm_member_types = Vec::with_capacity(st.members.len());
    for m in &st.members {
        llvm_member_types.push(to_llvm_type(context, target_machine, &m.typ));
    }
    LLVMStructTypeInContext(
        context,
        llvm_member_types.as_mut_ptr(),
        llvm_member_types.len() as c_uint,
        0,
    )
}

unsafe fn optional_to_llvm_type(context: LLVMContextRef, target_machine: &TargetMachine, inner: &Type) -> LLVMTypeRef {
    let inner = to_llvm_type(context, target_machine, inner);
    let mut member_types = vec![
        LLVMInt1TypeInContext(context), // nil or not
        inner,
    ];
    LLVMStructTypeInContext(context, member_types.as_mut_ptr(), member_types.len() as c_uint, 0)
}

pub unsafe fn native_llvm_int_type(context: LLVMContextRef, target_machine: &TargetMachine) -> LLVMTypeRef {
    match target_machine.target.int_size {
        IntSize::I8 => LLVMInt8TypeInContext(context),
        IntSize::I16 => LLVMInt16TypeInContext(context),
        IntSize::I32 => LLVMInt32TypeInContext(context),
        IntSize::I64 => LLVMInt64TypeInContext(context),
    }
}

pub unsafe fn to_llvm_type(context: LLVMContextRef, target_machine: &TargetMachine, typ: &Type) -> LLVMTypeRef {
    match *typ {
        Type::Void => LLVMVoidTypeInContext(context),
        Type::Int(IntSize::I8) | Type::UInt(IntSize::I8) => LLVMInt8TypeInContext(context),
        Type::Int(IntSize::I16) | Type::UInt(IntSize::I16) => LLVMInt16TypeInContext(context),
        Type::Char | Type::Int(IntSize::I32) | Type::UInt(IntSize::I32) => LLVMInt32TypeInContext(context),
        Type::Int(IntSize::I64) | Type::UInt(IntSize::I64) => LLVMInt64TypeInContext(context),
        Type::Enum(_) => native_llvm_int_type(context, target_machine),
        Type::Bool => LLVMInt1TypeInContext(context),
        Type::Float(FloatSize::F32) => LLVMFloatTypeInContext(context),
        Type::Float(FloatSize::F64) => LLVMDoubleTypeInContext(context),
        Type::Pointer(ref inner) => LLVMPointerType(to_llvm_type(context, target_machine, inner), 0),
        Type::Array(ref at) => array_to_llvm_type(context, target_machine, at),
        Type::Slice(ref st) => slice_to_llvm_type(context, target_machine, st),
        Type::String => string_to_llvm_type(context, target_machine),
        Type::Func(ref ft) => func_to_llvm_type(context, target_machine, ft),
        Type::Struct(ref st) => struct_to_llvm_type(context, target_machine, st),
        Type::Sum(ref st) => sum_type_to_llvm_type(context, target_machine, st),
        Type::Optional(ref ot) => optional_to_llvm_type(context, target_machine, ot),
        Type::Generic(_) => {
            panic!("Internal Compiler Error: All generic types must have been resolved before code generation")
        }
        Type::Unresolved(_) => panic!("Internal Compiler Error: All types must be resolved before code generation"),
        Type::Unknown => panic!("Internal Compiler Error: all types must be known before code generation"),
        Type::SelfType => panic!("Internal Compiler Error: self type must be known at this point"),
        Type::Interface(_) => panic!("Internal Compiler Error: interface type must be known at this point"),
    }
}
