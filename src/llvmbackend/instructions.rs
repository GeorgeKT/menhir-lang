use super::context::Context;
use super::function::gen_function_ptr;
use super::types::native_llvm_int_type;
use super::valueref::ValueRef;
use crate::ast::{ptr_type, BinaryOperator, Type, UnaryOperator};
use crate::bytecode::*;
use crate::compileerror::{code_gen_error, code_gen_result, CompileResult};
use libc::*;
use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::*;
use std::collections::HashMap;
use std::ffi::CString;
use std::ptr;

pub unsafe fn const_int(ctx: &Context, v: i64) -> LLVMValueRef {
    LLVMConstInt(
        native_llvm_int_type(ctx.context, ctx.target_machine),
        v as c_ulonglong,
        1,
    )
}

pub unsafe fn const_uint(ctx: &Context, v: u64) -> LLVMValueRef {
    LLVMConstInt(
        native_llvm_int_type(ctx.context, ctx.target_machine),
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

#[allow(unused)]
pub unsafe fn type_name(typ: LLVMTypeRef) -> String {
    use std::ffi::CStr;
    let name = LLVMPrintTypeToString(typ);
    let ret_name = String::from(CStr::from_ptr(name).to_str().expect("Invalid string"));
    LLVMDisposeMessage(name);
    ret_name
}

pub unsafe fn get_operand(ctx: &mut Context, operand: &Operand) -> CompileResult<ValueRef> {
    match operand {
        Operand::Func(func) => {
            let fi = ctx.get_function(func).expect("Unknown function");
            Ok(ValueRef::new(fi.function, fi.typ.clone()))
        }

        Operand::Var(v) => ctx.get_variable(&v.name, &v.typ),

        Operand::AddressOf(v) => ctx.get_variable(&v.name, &v.typ)?.address_of(),

        Operand::Dereference(v) => {
            let vr = ctx.get_variable(&v.name, &v.typ)?;
            Ok(ValueRef::new(
                vr.load(ctx)?,
                vr.typ
                    .get_pointer_element_type()
                    .ok_or_else(|| code_gen_error("Cannot dereference a non pointer type"))?
                    .clone(),
            ))
        }

        Operand::Const(c) => ValueRef::from_const(ctx, c),

        Operand::SizeOf(typ) => {
            let llvm_type = ctx.resolve_type(typ)?;
            Ok(ValueRef::new(
                LLVMSizeOf(llvm_type),
                ctx.target_machine.target.native_uint_type.clone(),
            ))
        }
    }
}

pub unsafe fn copy(ctx: &Context, dst: LLVMValueRef, src: LLVMValueRef, typ: LLVMTypeRef) -> CompileResult<()> {
    let func = ctx
        .get_function("memcpy")
        .ok_or_else(|| code_gen_error("memcpy not found"))?;
    let void_ptr_type = LLVMPointerType(LLVMVoidTypeInContext(ctx.context), 0);
    let mut args = vec![
        LLVMBuildBitCast(ctx.builder, dst, void_ptr_type, cstr!("dst_cast")),
        LLVMBuildBitCast(ctx.builder, src, void_ptr_type, cstr!("src_cast")),
        const_uint(ctx, ctx.target_machine.size_of_type(typ) as u64),
    ];

    LLVMBuildCall2(
        ctx.builder,
        ctx.resolve_type(&func.typ)?,
        func.function,
        args.as_mut_ptr(),
        args.len() as c_uint,
        cstr!("ac"),
    );
    Ok(())
}

unsafe fn get_function_arg(ctx: &mut Context, operand: &Operand) -> CompileResult<LLVMValueRef> {
    match operand {
        Operand::Var(v) => {
            let src = ctx.get_variable(&v.name, &v.typ)?;
            if !src.typ.is_pointer() {
                return Ok(src.value);
            }
            let inner_type = src
                .typ
                .get_pointer_element_type()
                .ok_or_else(|| code_gen_error("Expecting pointer type here"))?;
            if let Type::Func(_) = inner_type {
                src.load(ctx)
            } else if inner_type.pass_by_value() {
                src.load(ctx)
            } else if v.typ == src.typ {
                Ok(src.value)
            } else {
                let dst = ctx.stack_alloc("argcopy", inner_type)?;
                copy(ctx, dst, src.value, ctx.resolve_type(inner_type)?)?;
                Ok(dst)
            }
        }

        Operand::AddressOf(v) => Ok(ctx.get_variable(&v.name, &v.typ)?.value),

        _ => get_operand(ctx, operand)?.load(ctx),
    }
}

unsafe fn gen_unary_op(ctx: &mut Context, dst: &Var, operator: UnaryOperator, src: &Operand) -> CompileResult<()> {
    let src_value = get_operand(ctx, src)?.load(ctx)?;
    let result = match (operator, &dst.typ) {
        (UnaryOperator::Sub, &Type::Int(_)) | (UnaryOperator::Sub, &Type::UInt(_)) => {
            LLVMBuildNeg(ctx.builder, src_value, cstr!("neg"))
        }
        (UnaryOperator::Sub, &Type::Float(_)) => LLVMBuildFNeg(ctx.builder, src_value, cstr!("neg")),
        (UnaryOperator::Not, &Type::Bool) => LLVMBuildNot(ctx.builder, src_value, cstr!("not")),
        _ => return code_gen_result("Unsupported unary operator"),
    };

    ctx.set_variable(&dst.name, ValueRef::new(result, dst.typ.clone()))
}

unsafe fn gen_binary_op(
    ctx: &mut Context,
    dst: &Var,
    op: BinaryOperator,
    left: &Operand,
    right: &Operand,
) -> CompileResult<()> {
    let left_type = left.get_type(ctx.target_machine.target.int_size);
    let left = get_operand(ctx, left)?.load(ctx)?;
    let right = get_operand(ctx, right)?.load(ctx)?;

    let value = match (op, left_type) {
        (BinaryOperator::Add, Type::Int(_)) => LLVMBuildAdd(ctx.builder, left, right, cstr!("bop")),
        (BinaryOperator::Add, Type::UInt(_)) => LLVMBuildAdd(ctx.builder, left, right, cstr!("bop")),
        (BinaryOperator::Add, Type::Float(_)) => LLVMBuildFAdd(ctx.builder, left, right, cstr!("bop")),

        (BinaryOperator::Sub, Type::Int(_)) => LLVMBuildSub(ctx.builder, left, right, cstr!("bop")),
        (BinaryOperator::Sub, Type::UInt(_)) => LLVMBuildSub(ctx.builder, left, right, cstr!("bop")),
        (BinaryOperator::Sub, Type::Float(_)) => LLVMBuildFSub(ctx.builder, left, right, cstr!("bop")),

        (BinaryOperator::Mul, Type::Int(_)) => LLVMBuildMul(ctx.builder, left, right, cstr!("bop")),
        (BinaryOperator::Mul, Type::UInt(_)) => LLVMBuildMul(ctx.builder, left, right, cstr!("bop")),
        (BinaryOperator::Mul, Type::Float(_)) => LLVMBuildFMul(ctx.builder, left, right, cstr!("bop")),

        (BinaryOperator::Div, Type::Int(_)) => LLVMBuildSDiv(ctx.builder, left, right, cstr!("bop")),
        (BinaryOperator::Div, Type::UInt(_)) => LLVMBuildUDiv(ctx.builder, left, right, cstr!("bop")),
        (BinaryOperator::Div, Type::Float(_)) => LLVMBuildFDiv(ctx.builder, left, right, cstr!("bop")),

        (BinaryOperator::Mod, Type::Int(_)) => LLVMBuildSRem(ctx.builder, left, right, cstr!("bop")),
        (BinaryOperator::Mod, Type::UInt(_)) => LLVMBuildURem(ctx.builder, left, right, cstr!("bop")),

        (BinaryOperator::LessThan, Type::Int(_)) => {
            LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntSLT, left, right, cstr!("bop"))
        }
        (BinaryOperator::LessThan, Type::UInt(_)) => {
            LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntULT, left, right, cstr!("bop"))
        }
        (BinaryOperator::LessThan, Type::Float(_)) => {
            LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealULT, left, right, cstr!("bop"))
        }
        (BinaryOperator::LessThan, Type::Char) => {
            LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntULT, left, right, cstr!("bop"))
        }

        (BinaryOperator::GreaterThan, Type::Int(_)) => {
            LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntSGT, left, right, cstr!("bop"))
        }
        (BinaryOperator::GreaterThan, Type::UInt(_)) => {
            LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntUGT, left, right, cstr!("bop"))
        }
        (BinaryOperator::GreaterThan, Type::Float(_)) => {
            LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealUGT, left, right, cstr!("bop"))
        }
        (BinaryOperator::GreaterThan, Type::Char) => {
            LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntUGT, left, right, cstr!("bop"))
        }

        (BinaryOperator::LessThanEquals, Type::Int(_)) => {
            LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntSLE, left, right, cstr!("bop"))
        }
        (BinaryOperator::LessThanEquals, Type::UInt(_)) => {
            LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntULE, left, right, cstr!("bop"))
        }
        (BinaryOperator::LessThanEquals, Type::Float(_)) => {
            LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealULE, left, right, cstr!("bop"))
        }
        (BinaryOperator::LessThanEquals, Type::Char) => {
            LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntULE, left, right, cstr!("bop"))
        }

        (BinaryOperator::GreaterThanEquals, Type::Int(_)) => {
            LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntSGE, left, right, cstr!("bop"))
        }
        (BinaryOperator::GreaterThanEquals, Type::UInt(_)) => {
            LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntUGE, left, right, cstr!("bop"))
        }
        (BinaryOperator::GreaterThanEquals, Type::Float(_)) => {
            LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealUGE, left, right, cstr!("bop"))
        }
        (BinaryOperator::GreaterThanEquals, Type::Char) => {
            LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntUGE, left, right, cstr!("bop"))
        }

        (BinaryOperator::Equals, Type::Int(_)) => {
            LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntEQ, left, right, cstr!("bop"))
        }
        (BinaryOperator::Equals, Type::UInt(_)) => {
            LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntEQ, left, right, cstr!("bop"))
        }
        (BinaryOperator::Equals, Type::Float(_)) => {
            LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealUEQ, left, right, cstr!("bop"))
        }
        (BinaryOperator::Equals, Type::Char) => {
            LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntEQ, left, right, cstr!("bop"))
        }
        (BinaryOperator::Equals, Type::Bool) => {
            LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntEQ, left, right, cstr!("bop"))
        }
        (BinaryOperator::Equals, Type::Enum(_)) => {
            LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntEQ, left, right, cstr!("bop"))
        }

        (BinaryOperator::NotEquals, Type::Int(_)) => {
            LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntNE, left, right, cstr!("bop"))
        }
        (BinaryOperator::NotEquals, Type::UInt(_)) => {
            LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntNE, left, right, cstr!("bop"))
        }
        (BinaryOperator::NotEquals, Type::Float(_)) => {
            LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealUNE, left, right, cstr!("bop"))
        }
        (BinaryOperator::NotEquals, Type::Char) => {
            LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntNE, left, right, cstr!("bop"))
        }
        (BinaryOperator::NotEquals, Type::Bool) => {
            LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntNE, left, right, cstr!("bop"))
        }
        (BinaryOperator::NotEquals, Type::Enum(_)) => {
            LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntNE, left, right, cstr!("bop"))
        }

        (BinaryOperator::And, Type::Bool) => LLVMBuildAnd(ctx.builder, left, right, cstr!("bop")),
        (BinaryOperator::Or, Type::Bool) => LLVMBuildOr(ctx.builder, left, right, cstr!("bop")),

        (_, t) => return code_gen_result(format!("Operator {} not supported on type {}", op, t)),
    };

    ctx.set_variable(&dst.name, ValueRef::new(value, dst.typ.clone()))?;
    Ok(())
}

unsafe fn gen_cast(ctx: &mut Context, dst: &Var, src: &Operand) -> CompileResult<()> {
    let operand = get_operand(ctx, src)?;
    let src_type = src.get_type(ctx.target_machine.target.int_size);
    let casted = match (&dst.typ, &src_type) {
        (&Type::UInt(_), &Type::Int(_)) | (&Type::Int(_), &Type::UInt(_)) => LLVMBuildIntCast(
            ctx.builder,
            operand.load(ctx)?,
            ctx.resolve_type(&dst.typ)?,
            cstr!("cast_to_int"),
        ),

        (&Type::Int(_), &Type::Float(_)) => LLVMBuildFPToSI(
            ctx.builder,
            operand.load(ctx)?,
            ctx.resolve_type(&dst.typ)?,
            cstr!("cast_to_int"),
        ),

        (&Type::UInt(_), &Type::Float(_)) => LLVMBuildFPToUI(
            ctx.builder,
            operand.load(ctx)?,
            ctx.resolve_type(&dst.typ)?,
            cstr!("cast_to_int"),
        ),

        (&Type::Float(_), &Type::Int(_)) => LLVMBuildSIToFP(
            ctx.builder,
            operand.load(ctx)?,
            ctx.resolve_type(&dst.typ)?,
            cstr!("cast_to_int"),
        ),

        (&Type::Float(_), &Type::UInt(_)) => LLVMBuildUIToFP(
            ctx.builder,
            operand.load(ctx)?,
            ctx.resolve_type(&dst.typ)?,
            cstr!("cast_to_int"),
        ),

        (&Type::Pointer(_), &Type::Pointer(_)) => LLVMBuildBitCast(
            ctx.builder,
            operand.value,
            ctx.resolve_type(&dst.typ)?,
            cstr!("ptr_cast"),
        ),

        (&Type::Pointer(_), &Type::Array(_)) => LLVMBuildBitCast(
            ctx.builder,
            operand.load(ctx)?,
            ctx.resolve_type(&dst.typ)?,
            cstr!("ptr_cast"),
        ),

        (&Type::Bool, &Type::Pointer(_)) => LLVMBuildIsNull(ctx.builder, operand.value, cstr!("isnt_null")),

        _ => {
            return code_gen_result(format!(
                "Cast from type {} to type {} is not allowed",
                src_type, dst.typ
            ))
        }
    };

    ctx.set_variable(&dst.name, ValueRef::new(casted, dst.typ.clone()))
}

pub unsafe fn gen_instruction(
    ctx: &mut Context,
    instr: &Instruction,
    blocks: &HashMap<BasicBlockRef, LLVMBasicBlockRef>,
) -> CompileResult<()> {
    match instr {
        Instruction::Store { dst, src } => {
            let dst_var = ctx.get_variable(&dst.name, &dst.typ)?;
            let vr = get_operand(ctx, src)?;
            dst_var.store(ctx, &vr)?;
            if let Type::Func(ft) = &dst.typ {
                gen_function_ptr(ctx, &dst.name, vr.value, ft.return_type.clone(), dst.typ.clone())?;
            }
        }

        Instruction::Load { dst, ptr } => {
            let src_var = ctx.get_variable(&ptr.name, &ptr.typ)?;
            let val = src_var.load(ctx)?;
            ctx.set_variable(&dst.name, ValueRef::new(val, dst.typ.clone()))?;
        }

        Instruction::LoadMember { dst, obj, member_index } => {
            let obj_var = ctx.get_variable(&obj.name, &obj.typ)?;
            let member_ptr = obj_var.get_member_ptr(ctx, member_index)?;
            ctx.set_variable(&dst.name, member_ptr)?;
        }

        Instruction::AddressOfMember { dst, obj, member_index } => {
            let obj_var = ctx.get_variable(&obj.name, &obj.typ)?;
            let member_ptr = obj_var.get_member_ptr(ctx, member_index)?;
            ctx.set_variable(&dst.name, member_ptr)?;
        }

        Instruction::StoreMember { obj, member_index, src } => {
            let src_val = get_operand(ctx, src)?;
            let obj_var = ctx.get_variable(&obj.name, &obj.typ)?;
            obj_var.store_member(ctx, member_index, &src_val)?;
        }

        Instruction::AddressOf { dst, obj } => {
            let v = ctx.get_variable(&obj.name, &obj.typ)?.address_of()?;
            ctx.set_variable(&dst.name, v)?;
        }

        Instruction::GetProperty { dst, obj, prop } => {
            let obj_var = ctx.get_variable(&obj.name, &obj.typ)?;
            let prop = obj_var.get_property(ctx, *prop)?;
            ctx.set_variable(&dst.name, prop)?;
        }

        Instruction::SetProperty { obj, prop, val } => {
            let obj_var = ctx.get_variable(&obj.name, &obj.typ)?;
            obj_var.set_property(ctx, *prop, *val)?;
        }

        Instruction::UnaryOp { dst, op, src } => {
            gen_unary_op(ctx, dst, *op, src)?;
        }

        Instruction::BinaryOp { dst, op, left, right } => {
            gen_binary_op(ctx, dst, *op, left, right)?;
        }

        Instruction::Call { dst, func, args } => {
            let func = ctx
                .get_function(func)
                .ok_or_else(|| code_gen_error(format!("Unknown function {}", func)))?;
            let mut func_args = Vec::with_capacity(args.len());
            for arg in args {
                func_args.push(get_function_arg(ctx, arg)?);
            }

            let call = LLVMBuildCall2(
                ctx.builder,
                ctx.resolve_type(&func.typ)?,
                func.function,
                func_args.as_mut_ptr(),
                args.len() as c_uint,
                if func.return_type != Type::Void {
                    cstr!("call")
                } else {
                    cstr!("")
                },
            );

            if let Some(dst) = dst {
                let ret = ValueRef::new(call, func.return_type.clone());
                ctx.set_variable(&dst.name, ret)?;
            }
        }

        Instruction::Slice { dst, src, start, len } => {
            let dst_var = ctx.get_variable(&dst.name, &dst.typ)?;
            let src_var = ctx.get_variable(&src.name, &dst.typ)?;
            dst_var.create_slice(ctx, &src_var, start, len)?;
        }

        Instruction::MakeSlice { dst, data, len } => {
            let dst_var = ctx.get_variable(&dst.name, &dst.typ)?;
            let data_var = ctx.get_variable(&data.name, &data.typ)?;
            let start = Operand::Const(Constant::UInt(0, ctx.target_machine.target.int_size));
            let len = Operand::Var(len.clone());
            dst_var.create_slice(ctx, &data_var, &start, &len)?;
        }

        Instruction::LoadOptionalFlag { dst, obj } => {
            let obj_var = ctx.get_variable(&obj.name, &obj.typ)?;
            let opt_flag = obj_var.load_optional_flag(ctx)?;
            ctx.set_variable(&dst.name, opt_flag)?;
        }

        Instruction::StoreNil(dst) => {
            let dst_var = ctx.get_variable(&dst.name, &dst.typ)?;
            dst_var.store_nil(ctx)?;
        }

        Instruction::Cast { dst, src } => {
            gen_cast(ctx, dst, src)?;
        }

        Instruction::HeapAlloc(var) => {
            let name = CString::new(&var.name[..]).expect("Invalid string");
            let value = LLVMBuildMalloc(ctx.builder, ctx.resolve_type(&var.typ)?, name.as_ptr());
            ctx.set_variable(&var.name, ValueRef::new(value, ptr_type(var.typ.clone())))?;
        }

        Instruction::StackAlloc(var) => {
            let alloc = ctx.stack_alloc(&var.name, &var.typ)?;
            ctx.set_variable(&var.name, ValueRef::new(alloc, ptr_type(var.typ.clone())))?;
        }

        Instruction::StartScope => {
            ctx.push_stack(ptr::null_mut());
        }

        Instruction::EndScope => {
            ctx.pop_stack();
        }

        Instruction::Return(operand) => {
            LLVMBuildRet(ctx.builder, get_operand(ctx, operand)?.load(ctx)?);
        }

        Instruction::ReturnVoid => {
            LLVMBuildRetVoid(ctx.builder);
        }

        Instruction::Branch(bbref) => {
            let llvm_bb = blocks.get(bbref).expect("Unknown basic block");
            LLVMBuildBr(ctx.builder, *llvm_bb);
        }

        Instruction::BranchIf {
            cond,
            on_true,
            on_false,
        } => {
            let on_true_bb = blocks.get(on_true).expect("Unknown basic block");
            let on_false_bb = blocks.get(on_false).expect("Unknown basic block");
            LLVMBuildCondBr(
                ctx.builder,
                get_operand(ctx, cond)?.load(ctx)?,
                *on_true_bb,
                *on_false_bb,
            );
        }

        Instruction::Delete(var) => {
            LLVMBuildFree(ctx.builder, ctx.get_variable(&var.name, &var.typ)?.value);
        }
    }

    Ok(())
}
