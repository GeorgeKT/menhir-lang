use std::ffi::{c_uint, c_ulonglong};

use crate::ast::{BinaryOperator, Type, UnaryOperator};
use crate::compileerror::CompileResult;
use crate::compileerror::{code_gen_error, code_gen_result};
use crate::lazycode::{ByteCodeProperty, CallArg, Constant, Operand, OPTIONAL_DATA_IDX};
#[allow(unused)]
use crate::llvmbackend::instructions::type_name;

use super::types::native_llvm_int_type;
use super::valueref::ValueRef;
use super::Context;

use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::*;

pub fn get_const_usize(op: &Operand) -> CompileResult<usize> {
    match op {
        Operand::Constant { value } => match value {
            Constant::Int(v, _) => Ok(*v as usize),
            Constant::UInt(v, _) => Ok(*v as usize),
            _ => code_gen_result("Expected const usize"),
        },
        _ => code_gen_result("Expected const usize"),
    }
}

pub unsafe fn const_int(ctx: &Context, v: i64) -> LLVMValueRef {
    LLVMConstInt(
        native_llvm_int_type(ctx.context, &ctx.target_machine),
        v as c_ulonglong,
        1,
    )
}

pub unsafe fn const_uint(ctx: &Context, v: u64) -> LLVMValueRef {
    LLVMConstInt(
        native_llvm_int_type(ctx.context, &ctx.target_machine),
        v as c_ulonglong,
        0,
    )
}

pub unsafe fn const_bool(ctx: &Context, v: bool) -> LLVMValueRef {
    LLVMConstInt(LLVMInt1TypeInContext(ctx.context), if v { 1 } else { 0 }, 0)
}

pub unsafe fn const_float(ctx: &Context, v: f64) -> LLVMValueRef {
    LLVMConstReal(LLVMDoubleTypeInContext(ctx.context), v)
}

pub unsafe fn const_char(ctx: &Context, c: char) -> LLVMValueRef {
    LLVMConstInt(LLVMInt32TypeInContext(ctx.context), c as c_ulonglong, 0)
}

pub unsafe fn copy(ctx: &Context, dst: LLVMValueRef, src: LLVMValueRef, typ: LLVMTypeRef) -> CompileResult<()> {
    let align = ctx.target_machine.alignment_of_type(typ) as u32;
    let size = const_uint(ctx, ctx.target_machine.size_of_type(typ) as u64);
    //println!("size of {}: {}", type_name(typ), ctx.target_machine.size_of_type(typ));
    LLVMBuildMemCpy(ctx.builder, dst, align, src, align, size);
    Ok(())
}

unsafe fn gen_unary_op(
    ctx: &mut Context,
    operator: UnaryOperator,
    src: &Operand,
    typ: &Type,
) -> CompileResult<ValueRef> {
    let src_value = gen_operand(ctx, src, None)?.value;
    let result = match (operator, typ) {
        (UnaryOperator::Sub, &Type::Int(_)) | (UnaryOperator::Sub, &Type::UInt(_)) => {
            LLVMBuildNeg(ctx.builder, src_value, cstr!("neg"))
        }
        (UnaryOperator::Sub, &Type::Float(_)) => LLVMBuildFNeg(ctx.builder, src_value, cstr!("neg")),
        (UnaryOperator::Not, &Type::Bool) => LLVMBuildNot(ctx.builder, src_value, cstr!("not")),
        _ => return code_gen_result("Unsupported unary operator"),
    };

    Ok(ValueRef::new(result, typ.clone()))
}

unsafe fn gen_binary_op(
    ctx: &mut Context,
    op: BinaryOperator,
    left: &Operand,
    right: &Operand,
    typ: &Type,
) -> CompileResult<ValueRef> {
    let left = gen_operand(ctx, left, None)?;
    let right = gen_operand(ctx, right, None)?;

    let value = match (op, left.typ) {
        (BinaryOperator::Add, Type::Pointer(et)) => {
            let mut indices = [right.value];
            LLVMBuildGEP2(
                ctx.builder,
                ctx.resolve_type(&et)?,
                left.value,
                indices.as_mut_ptr(),
                1,
                cstr!("bop"),
            )
        }
        (BinaryOperator::Sub, Type::Pointer(et)) => {
            let neg = LLVMBuildNeg(ctx.builder, right.value, cstr!("neg"));
            let mut indices = [neg];
            LLVMBuildGEP2(
                ctx.builder,
                ctx.resolve_type(&et)?,
                left.value,
                indices.as_mut_ptr(),
                1,
                cstr!("bop"),
            )
        }

        (BinaryOperator::Add, Type::Int(_)) => LLVMBuildAdd(ctx.builder, left.value, right.value, cstr!("bop")),
        (BinaryOperator::Add, Type::UInt(_)) => LLVMBuildAdd(ctx.builder, left.value, right.value, cstr!("bop")),
        (BinaryOperator::Add, Type::Float(_)) => LLVMBuildFAdd(ctx.builder, left.value, right.value, cstr!("bop")),

        (BinaryOperator::Sub, Type::Int(_)) => LLVMBuildSub(ctx.builder, left.value, right.value, cstr!("bop")),
        (BinaryOperator::Sub, Type::UInt(_)) => LLVMBuildSub(ctx.builder, left.value, right.value, cstr!("bop")),
        (BinaryOperator::Sub, Type::Float(_)) => LLVMBuildFSub(ctx.builder, left.value, right.value, cstr!("bop")),

        (BinaryOperator::Mul, Type::Int(_)) => LLVMBuildMul(ctx.builder, left.value, right.value, cstr!("bop")),
        (BinaryOperator::Mul, Type::UInt(_)) => LLVMBuildMul(ctx.builder, left.value, right.value, cstr!("bop")),
        (BinaryOperator::Mul, Type::Float(_)) => LLVMBuildFMul(ctx.builder, left.value, right.value, cstr!("bop")),

        (BinaryOperator::Div, Type::Int(_)) => LLVMBuildSDiv(ctx.builder, left.value, right.value, cstr!("bop")),
        (BinaryOperator::Div, Type::UInt(_)) => LLVMBuildUDiv(ctx.builder, left.value, right.value, cstr!("bop")),
        (BinaryOperator::Div, Type::Float(_)) => LLVMBuildFDiv(ctx.builder, left.value, right.value, cstr!("bop")),

        (BinaryOperator::Mod, Type::Int(_)) => LLVMBuildSRem(ctx.builder, left.value, right.value, cstr!("bop")),
        (BinaryOperator::Mod, Type::UInt(_)) => LLVMBuildURem(ctx.builder, left.value, right.value, cstr!("bop")),

        (BinaryOperator::LessThan, Type::Int(_)) => LLVMBuildICmp(
            ctx.builder,
            LLVMIntPredicate::LLVMIntSLT,
            left.value,
            right.value,
            cstr!("bop"),
        ),
        (BinaryOperator::LessThan, Type::UInt(_)) => LLVMBuildICmp(
            ctx.builder,
            LLVMIntPredicate::LLVMIntULT,
            left.value,
            right.value,
            cstr!("bop"),
        ),
        (BinaryOperator::LessThan, Type::Float(_)) => LLVMBuildFCmp(
            ctx.builder,
            LLVMRealPredicate::LLVMRealULT,
            left.value,
            right.value,
            cstr!("bop"),
        ),
        (BinaryOperator::LessThan, Type::Char) => LLVMBuildICmp(
            ctx.builder,
            LLVMIntPredicate::LLVMIntULT,
            left.value,
            right.value,
            cstr!("bop"),
        ),

        (BinaryOperator::GreaterThan, Type::Int(_)) => LLVMBuildICmp(
            ctx.builder,
            LLVMIntPredicate::LLVMIntSGT,
            left.value,
            right.value,
            cstr!("bop"),
        ),
        (BinaryOperator::GreaterThan, Type::UInt(_)) => LLVMBuildICmp(
            ctx.builder,
            LLVMIntPredicate::LLVMIntUGT,
            left.value,
            right.value,
            cstr!("bop"),
        ),
        (BinaryOperator::GreaterThan, Type::Float(_)) => LLVMBuildFCmp(
            ctx.builder,
            LLVMRealPredicate::LLVMRealUGT,
            left.value,
            right.value,
            cstr!("bop"),
        ),
        (BinaryOperator::GreaterThan, Type::Char) => LLVMBuildICmp(
            ctx.builder,
            LLVMIntPredicate::LLVMIntUGT,
            left.value,
            right.value,
            cstr!("bop"),
        ),

        (BinaryOperator::LessThanEquals, Type::Int(_)) => LLVMBuildICmp(
            ctx.builder,
            LLVMIntPredicate::LLVMIntSLE,
            left.value,
            right.value,
            cstr!("bop"),
        ),
        (BinaryOperator::LessThanEquals, Type::UInt(_)) => LLVMBuildICmp(
            ctx.builder,
            LLVMIntPredicate::LLVMIntULE,
            left.value,
            right.value,
            cstr!("bop"),
        ),
        (BinaryOperator::LessThanEquals, Type::Float(_)) => LLVMBuildFCmp(
            ctx.builder,
            LLVMRealPredicate::LLVMRealULE,
            left.value,
            right.value,
            cstr!("bop"),
        ),
        (BinaryOperator::LessThanEquals, Type::Char) => LLVMBuildICmp(
            ctx.builder,
            LLVMIntPredicate::LLVMIntULE,
            left.value,
            right.value,
            cstr!("bop"),
        ),

        (BinaryOperator::GreaterThanEquals, Type::Int(_)) => LLVMBuildICmp(
            ctx.builder,
            LLVMIntPredicate::LLVMIntSGE,
            left.value,
            right.value,
            cstr!("bop"),
        ),
        (BinaryOperator::GreaterThanEquals, Type::UInt(_)) => LLVMBuildICmp(
            ctx.builder,
            LLVMIntPredicate::LLVMIntUGE,
            left.value,
            right.value,
            cstr!("bop"),
        ),
        (BinaryOperator::GreaterThanEquals, Type::Float(_)) => LLVMBuildFCmp(
            ctx.builder,
            LLVMRealPredicate::LLVMRealUGE,
            left.value,
            right.value,
            cstr!("bop"),
        ),
        (BinaryOperator::GreaterThanEquals, Type::Char) => LLVMBuildICmp(
            ctx.builder,
            LLVMIntPredicate::LLVMIntUGE,
            left.value,
            right.value,
            cstr!("bop"),
        ),

        (BinaryOperator::Equals, Type::Int(_)) => LLVMBuildICmp(
            ctx.builder,
            LLVMIntPredicate::LLVMIntEQ,
            left.value,
            right.value,
            cstr!("bop"),
        ),
        (BinaryOperator::Equals, Type::UInt(_)) => LLVMBuildICmp(
            ctx.builder,
            LLVMIntPredicate::LLVMIntEQ,
            left.value,
            right.value,
            cstr!("bop"),
        ),
        (BinaryOperator::Equals, Type::Float(_)) => LLVMBuildFCmp(
            ctx.builder,
            LLVMRealPredicate::LLVMRealUEQ,
            left.value,
            right.value,
            cstr!("bop"),
        ),
        (BinaryOperator::Equals, Type::Char) => LLVMBuildICmp(
            ctx.builder,
            LLVMIntPredicate::LLVMIntEQ,
            left.value,
            right.value,
            cstr!("bop"),
        ),
        (BinaryOperator::Equals, Type::Bool) => LLVMBuildICmp(
            ctx.builder,
            LLVMIntPredicate::LLVMIntEQ,
            left.value,
            right.value,
            cstr!("bop"),
        ),
        (BinaryOperator::Equals, Type::Enum(_)) => LLVMBuildICmp(
            ctx.builder,
            LLVMIntPredicate::LLVMIntEQ,
            left.value,
            right.value,
            cstr!("bop"),
        ),

        (BinaryOperator::NotEquals, Type::Int(_)) => LLVMBuildICmp(
            ctx.builder,
            LLVMIntPredicate::LLVMIntNE,
            left.value,
            right.value,
            cstr!("bop"),
        ),
        (BinaryOperator::NotEquals, Type::UInt(_)) => LLVMBuildICmp(
            ctx.builder,
            LLVMIntPredicate::LLVMIntNE,
            left.value,
            right.value,
            cstr!("bop"),
        ),
        (BinaryOperator::NotEquals, Type::Float(_)) => LLVMBuildFCmp(
            ctx.builder,
            LLVMRealPredicate::LLVMRealUNE,
            left.value,
            right.value,
            cstr!("bop"),
        ),
        (BinaryOperator::NotEquals, Type::Char) => LLVMBuildICmp(
            ctx.builder,
            LLVMIntPredicate::LLVMIntNE,
            left.value,
            right.value,
            cstr!("bop"),
        ),
        (BinaryOperator::NotEquals, Type::Bool) => LLVMBuildICmp(
            ctx.builder,
            LLVMIntPredicate::LLVMIntNE,
            left.value,
            right.value,
            cstr!("bop"),
        ),
        (BinaryOperator::NotEquals, Type::Enum(_)) => LLVMBuildICmp(
            ctx.builder,
            LLVMIntPredicate::LLVMIntNE,
            left.value,
            right.value,
            cstr!("bop"),
        ),

        (BinaryOperator::And, Type::Bool) => LLVMBuildAnd(ctx.builder, left.value, right.value, cstr!("bop")),
        (BinaryOperator::Or, Type::Bool) => LLVMBuildOr(ctx.builder, left.value, right.value, cstr!("bop")),

        (_, t) => return code_gen_result(format!("Operator {} not supported on type {}", op, t)),
    };

    Ok(ValueRef::new(value, typ.clone()))
}

unsafe fn gen_cast(ctx: &mut Context, src: &Operand, typ: Type) -> CompileResult<ValueRef> {
    let operand = gen_operand(ctx, src, None)?;
    let casted = match (&typ, &operand.typ) {
        (&Type::UInt(_), &Type::Int(_)) | (&Type::Int(_), &Type::UInt(_)) => LLVMBuildIntCast(
            ctx.builder,
            operand.value,
            ctx.resolve_type(&typ)?,
            cstr!("cast_to_int"),
        ),

        (&Type::Int(_), &Type::Float(_)) => LLVMBuildFPToSI(
            ctx.builder,
            operand.value,
            ctx.resolve_type(&typ)?,
            cstr!("cast_to_int"),
        ),

        (&Type::UInt(_), &Type::Float(_)) => LLVMBuildFPToUI(
            ctx.builder,
            operand.value,
            ctx.resolve_type(&typ)?,
            cstr!("cast_to_int"),
        ),

        (&Type::Float(_), &Type::Int(_)) => LLVMBuildSIToFP(
            ctx.builder,
            operand.value,
            ctx.resolve_type(&typ)?,
            cstr!("cast_to_int"),
        ),

        (&Type::Float(_), &Type::UInt(_)) => LLVMBuildUIToFP(
            ctx.builder,
            operand.value,
            ctx.resolve_type(&typ)?,
            cstr!("cast_to_int"),
        ),

        (&Type::Pointer(_), &Type::Pointer(_)) => {
            LLVMBuildBitCast(ctx.builder, operand.value, ctx.resolve_type(&typ)?, cstr!("ptr_cast"))
        }

        (&Type::Pointer(_), &Type::Array(_)) => {
            LLVMBuildBitCast(ctx.builder, operand.value, ctx.resolve_type(&typ)?, cstr!("ptr_cast"))
        }

        (&Type::Bool, &Type::Pointer(_)) => LLVMBuildIsNotNull(ctx.builder, operand.value, cstr!("isnt_null")),

        _ => {
            return code_gen_result(format!(
                "Cast from type {} to type {} is not allowed",
                &operand.typ, typ
            ))
        }
    };

    Ok(ValueRef::new(casted, typ))
}

unsafe fn gen_function_arg(ctx: &mut Context, arg: &CallArg) -> CompileResult<ValueRef> {
    let src = gen_operand(ctx, &arg.arg, None)?;
    if let Some(inner_type) = src.typ.get_pointer_element_type() {
        if let Type::Func(_) = inner_type {
            return src.load(ctx);
        } else if &arg.arg_type == inner_type && arg.arg_type.pass_by_value() {
            return src.load(ctx);
        } else {
            return Ok(src);
        }
    }

    if arg.mutable && !arg.arg_type.pass_by_value() {
        // Copy the argument to allow the function to mutate it
        let dst = ctx.stack_alloc("argcopy", &src.typ)?;
        copy(ctx, dst.value, src.value, ctx.resolve_type(&src.typ)?)?;
        Ok(dst)
    } else {
        Ok(src)
    }
}

unsafe fn gen_call(
    ctx: &mut Context,
    callee: &Operand,
    args: &Vec<CallArg>,
    typ: &Type,
    rvo: bool,
    dst: Option<ValueRef>,
) -> CompileResult<ValueRef> {
    let callee = gen_operand(ctx, callee, None)?;

    let mut func_args = Vec::with_capacity(args.len());
    for (idx, arg) in args.iter().enumerate() {
        if idx == args.len() - 1 && rvo {
            if let Some(dst) = &dst {
                func_args.push(dst.value);
                continue;
            }
        }
        let fa = gen_function_arg(ctx, arg)?;
        func_args.push(fa.value);
    }

    let ct = ctx.resolve_type(&callee.typ)?;
    let call = LLVMBuildCall2(
        ctx.builder,
        ct,
        callee.value,
        func_args.as_mut_ptr(),
        args.len() as c_uint,
        if rvo || typ == &Type::Void {
            cstr!("")
        } else {
            cstr!("call")
        },
    );

    if !rvo {
        Ok(ValueRef::new(call, typ.clone()))
    } else {
        Ok(ValueRef::new(
            func_args
                .last()
                .cloned()
                .expect("ICE: rvo without args should not happen"),
            typ.clone(),
        ))
    }
}

unsafe fn get_dst(ctx: &mut Context, name: &str, dst: Option<ValueRef>, typ: &Type) -> CompileResult<ValueRef> {
    if let Some(dst) = dst {
        Ok(dst)
    } else {
        ctx.stack_alloc(name, typ)
    }
}

unsafe fn gen_seq(
    ctx: &mut Context,
    members: &[Operand],
    typ: &Type,
    dst: Option<ValueRef>,
) -> CompileResult<ValueRef> {
    let vr = get_dst(ctx, "seq", dst, typ)?;
    for (idx, m) in members.iter().enumerate() {
        let mem_ptr = vr.get_member_ptr_static(ctx, idx)?;
        gen_operand_dst(ctx, m, &mem_ptr)?;
    }

    Ok(vr)
}

unsafe fn gen_sum(
    ctx: &mut Context,
    variant: usize,
    inner: &Option<Box<Operand>>,
    typ: &Type,
    dst: Option<ValueRef>,
) -> CompileResult<ValueRef> {
    let vr = get_dst(ctx, "sum", dst, typ)?;
    vr.set_property(ctx, ByteCodeProperty::SumTypeIndex, variant)?;
    if let Some(inner) = inner {
        let mem_ptr = vr.get_member_ptr_static(ctx, variant)?;
        gen_operand_dst(ctx, inner, &mem_ptr)?;
    }
    Ok(vr)
}

unsafe fn gen_enum(ctx: &mut Context, variant: usize, typ: &Type, dst: Option<ValueRef>) -> CompileResult<ValueRef> {
    let vr = get_dst(ctx, "enum", dst, typ)?;
    vr.store(ctx, &ValueRef::new(const_uint(ctx, variant as u64), typ.clone()))?;
    Ok(vr)
}

unsafe fn gen_dereference(ctx: &mut Context, inner: &Operand) -> CompileResult<ValueRef> {
    let inner_vr = gen_operand(ctx, inner, None)?;
    inner_vr.load(ctx)
}

unsafe fn gen_result(
    ctx: &mut Context,
    ok: bool,
    inner: &Operand,
    typ: &Type,
    dst: Option<ValueRef>,
) -> CompileResult<ValueRef> {
    let vr = get_dst(ctx, "result", dst, typ)?;
    let idx = if ok { 0 } else { 1 };
    vr.set_property(ctx, ByteCodeProperty::SumTypeIndex, idx)?;
    let mem_ptr = vr.get_member_ptr_static(ctx, idx)?;
    gen_operand_dst(ctx, inner, &mem_ptr)?;
    Ok(vr)
}

unsafe fn gen_optional(
    ctx: &mut Context,
    inner: &Option<Box<Operand>>,
    typ: &Type,
    dst: Option<ValueRef>,
) -> CompileResult<ValueRef> {
    let vr = get_dst(ctx, "opt", dst, typ)?;
    let idx = if inner.is_some() { 0 } else { 1 };
    vr.set_property(ctx, ByteCodeProperty::SumTypeIndex, idx)?;
    if let Some(inner) = inner {
        let mem_ptr = vr.get_member_ptr_static(ctx, OPTIONAL_DATA_IDX as usize)?;
        gen_operand_dst(ctx, inner, &mem_ptr)?;
    }
    Ok(vr)
}

unsafe fn gen_property(ctx: &mut Context, operand: &Operand, prop: ByteCodeProperty) -> CompileResult<ValueRef> {
    let op = gen_operand(ctx, operand, None)?;
    let prop = op.get_property(ctx, prop)?;
    Ok(prop)
}

unsafe fn gen_slice(
    ctx: &mut Context,
    array: &Operand,
    range: &Operand,
    typ: &Type,
    dst: Option<ValueRef>,
) -> CompileResult<ValueRef> {
    let slice = get_dst(ctx, "slice", dst, typ)?;
    let array = gen_operand(ctx, array, None)?;
    let range = gen_operand(ctx, range, None)?;
    let slice_data_ptr = slice.slice_data_ptr(ctx)?;
    let slice_len_ptr = slice.slice_len_ptr(ctx)?;

    let start = range.get_member_ptr_static(ctx, 0)?;
    let end = range.get_member_ptr_static(ctx, 1)?;
    let slice_len = LLVMBuildSub(
        ctx.builder,
        end.load(ctx)?.value,
        start.load(ctx)?.value,
        cstr!("slice_len"),
    );

    slice_len_ptr.store(
        ctx,
        &ValueRef::new(slice_len, ctx.target_machine.target.native_uint_type.clone()),
    )?;
    let array_member_ptr = array.get_member_ptr(ctx, &start)?;
    slice_data_ptr.store(ctx, &array_member_ptr)?;

    Ok(slice)
}

unsafe fn gen_range(
    ctx: &mut Context,
    start: &Operand,
    end: &Operand,
    typ: &Type,
    dst: Option<ValueRef>,
) -> CompileResult<ValueRef> {
    let vr = get_dst(ctx, "range", dst, typ)?;
    let start_ptr = vr.get_member_ptr_static(ctx, 0)?;
    let end_ptr = vr.get_member_ptr_static(ctx, 1)?;
    gen_operand_dst(ctx, start, &start_ptr)?;
    gen_operand_dst(ctx, end, &end_ptr)?;
    Ok(vr)
}

pub unsafe fn gen_operand_dst(ctx: &mut Context, operand: &Operand, dst: &ValueRef) -> CompileResult<()> {
    let vr = gen_operand(ctx, operand, Some(dst.clone()))?;
    if vr.value != dst.value {
        dst.store(ctx, &vr)?;
    }
    Ok(())
}

pub unsafe fn gen_operand(ctx: &mut Context, operand: &Operand, dst: Option<ValueRef>) -> CompileResult<ValueRef> {
    match operand {
        Operand::Var { name, .. } => {
            let vi = ctx
                .get_variable(name)
                .ok_or_else(|| code_gen_error(format!("Unknown variable {name}")))?;
            let v = vi.value.get_value(ctx)?;
            Ok(v)
        }
        Operand::VarPtr { name, .. } => {
            let vi = ctx
                .get_variable(name)
                .ok_or_else(|| code_gen_error(format!("Unknown variable {name}")))?;
            Ok(vi.value.clone())
        }
        Operand::Constant { value } => ValueRef::from_const(ctx, value),
        Operand::Member { obj, idx, .. } => {
            let obj = gen_operand(ctx, obj, None)?;
            let m = if let Ok(v) = get_const_usize(idx) {
                obj.get_member_ptr_static(ctx, v)?
            } else {
                let idx = gen_operand(ctx, idx, None)?;
                obj.get_member_ptr(ctx, &idx)?
            };
            m.load(ctx)
        }
        Operand::MemberPtr { obj, idx, .. } => {
            let obj = gen_operand(ctx, obj, None)?;
            if let Ok(v) = get_const_usize(idx) {
                obj.get_member_ptr_static(ctx, v)
            } else {
                let idx = gen_operand(ctx, idx, None)?;
                obj.get_member_ptr(ctx, &idx)
            }
        }
        Operand::AddressOf { obj } => gen_operand(ctx, obj, None)?.address_of(),
        Operand::Binary { op, left, right, typ } => gen_binary_op(ctx, *op, left, right, typ),
        Operand::Unary { op, inner, typ } => gen_unary_op(ctx, *op, inner, typ),
        Operand::Struct { members, typ } => gen_seq(ctx, members, typ, dst),
        Operand::Array { members, typ } => gen_seq(ctx, members, typ, dst),
        Operand::Sum { variant, inner, typ } => gen_sum(ctx, *variant, inner, typ, dst),
        Operand::Enum { variant, typ } => gen_enum(ctx, *variant, typ, dst),
        Operand::Dereference { inner, .. } => gen_dereference(ctx, inner),
        Operand::SizeOf { typ, .. } => {
            let llvm_type = ctx.resolve_type(typ)?;
            Ok(ValueRef::new(
                LLVMSizeOf(llvm_type),
                ctx.target_machine.target.native_uint_type.clone(),
            ))
        }
        Operand::Func { name, .. } => {
            let fi = ctx.get_function(name)?;
            Ok(ValueRef::new(fi.function, fi.sig.typ.clone()))
        }
        Operand::Call { callee, args, typ, rvo } => gen_call(ctx, callee, args, typ, *rvo, dst),
        Operand::Property { operand, property, .. } => gen_property(ctx, operand, *property),
        Operand::Slice { array, range, typ } => gen_slice(ctx, array, range, typ, dst),
        Operand::Result { ok, inner, typ } => gen_result(ctx, *ok, inner, typ, dst),
        Operand::Optional { inner, typ } => gen_optional(ctx, inner, typ, dst),
        Operand::Cast { inner, typ } => gen_cast(ctx, inner, typ.clone()),
        Operand::Null { typ } => {
            let llvm_typ = ctx.resolve_type(typ)?;
            Ok(ValueRef::new(LLVMConstNull(llvm_typ), typ.clone()))
        }
        Operand::New { inner, typ } => {
            let Some(element_type) = typ.get_pointer_element_type() else {
                return Err(code_gen_error("No pointer element type for heap alloc"));
            };
            let alloc = LLVMBuildMalloc(ctx.builder, ctx.resolve_type(element_type)?, cstr!("alloc"));
            gen_operand_dst(ctx, inner, &ValueRef::new(alloc, typ.clone()))?;
            Ok(ValueRef::new(alloc, typ.clone()))
        }
        Operand::Void => Ok(ValueRef::new(std::ptr::null_mut(), Type::Void)),
        Operand::Range { start, end, typ } => gen_range(ctx, start, end, typ, dst),
    }
}
