use std::collections::HashMap;
use std::ffi::CString;
use std::mem;
use libc::*;
use llvm::*;
use llvm::core::*;
use llvm::prelude::*;
use libcobra::bytecode::*;
use libcobra::ast::{Type, Operator};
use super::valueref::ValueRef;
use super::context::Context;

pub unsafe fn const_int(ctx: LLVMContextRef, v: isize) -> LLVMValueRef
{
    if mem::size_of::<isize>() == 8 {
        LLVMConstInt(LLVMInt64TypeInContext(ctx), v as c_ulonglong, 1)
    } else {
        LLVMConstInt(LLVMInt32TypeInContext(ctx), v as c_ulonglong, 1)
    }

}

pub unsafe fn const_uint(ctx: LLVMContextRef, v: usize) -> LLVMValueRef
{
    if mem::size_of::<usize>() == 8 {
        LLVMConstInt(LLVMInt64TypeInContext(ctx), v as c_ulonglong, 0)
    } else {
        LLVMConstInt(LLVMInt32TypeInContext(ctx), v as c_ulonglong, 0)
    }
}

unsafe fn const_bool(ctx: LLVMContextRef, v: bool) -> LLVMValueRef
{
    LLVMConstInt(LLVMInt1TypeInContext(ctx), if v {1} else {0}, 0)
}

unsafe fn const_float(ctx: LLVMContextRef, v: f64) -> LLVMValueRef
{
    LLVMConstReal(LLVMDoubleTypeInContext(ctx), v)
}

unsafe fn const_char(ctx: LLVMContextRef, c: u8) -> LLVMValueRef
{
    LLVMConstInt(LLVMInt32TypeInContext(ctx), c as c_ulonglong, 0)
}

unsafe fn get_variable(ctx: &Context, name: &str) -> LLVMValueRef
{
    ctx.get_variable(name)
        .expect("Unknown variable")
        .value.load(ctx.builder)
}

unsafe fn get_operand(ctx: &Context, operand: &Operand) -> LLVMValueRef
{
    match *operand
    {
        Operand::Func(ref func) => {
            ctx.get_function(func)
                .expect("Unknown function")
                .function
        }

        Operand::Var(ref v) => {
            get_variable(ctx, &v.name)
        }

        Operand::Int(v) => const_int(ctx.context, v),
        Operand::UInt(v) => const_uint(ctx.context, v),
        Operand::Float(v) => const_float(ctx.context, v),
        Operand::Char(v) => const_char(ctx.context, v),
        Operand::String(ref _s) => panic!("NYI"),
        Operand::Bool(v) => const_bool(ctx.context, v),
        Operand::Nil => panic!("NYI"),
    }
}

unsafe fn stack_alloc(ctx: &Context, typ: LLVMTypeRef, name: &str) -> LLVMValueRef
{
    let func = ctx.get_current_function();
    let entry_bb = LLVMGetEntryBasicBlock(func);
    let current_bb = LLVMGetInsertBlock(ctx.builder);
    // We allocate in the entry block
    LLVMPositionBuilder(ctx.builder, entry_bb, LLVMGetFirstInstruction(entry_bb));

    let name = CString::new(name).expect("Invalid string");
    let alloc = LLVMBuildAlloca(ctx.builder, typ, name.as_ptr());
    LLVMPositionBuilderAtEnd(ctx.builder, current_bb); // Position the builder where it was before
    alloc
}

unsafe fn gen_unary_op(ctx: &Context, dst: &Var, operator: Operator, src: &Operand)
{
    let dst_var = ctx.get_variable(&dst.name).expect("Unknown variable");
    let src_value = get_operand(ctx, src);
    let result = match (operator, &dst.typ)
    {
        (Operator::Sub, &Type::Int) |
        (Operator::Sub, &Type::UInt) => LLVMBuildNeg(ctx.builder, src_value, cstr!("neg")),
        (Operator::Sub, &Type::Float) => LLVMBuildFNeg(ctx.builder, src_value, cstr!("neg")),
        (Operator::Not, &Type::Bool) => LLVMBuildNot(ctx.builder, src_value, cstr!("not")),
        _ => panic!("Unsupported unary operator"),
    };

    dst_var.value.store(ctx.builder, result);
}

unsafe fn gen_binary_op(ctx: &Context, dst: &Var, op: Operator, left: &Operand, right: &Operand)
{
    let left_type = left.get_type();
    let left = get_operand(ctx, left);
    let right = get_operand(ctx, right);

    let value = match (op, left_type)
    {
        (Operator::Add, Type::Int) => LLVMBuildAdd(ctx.builder, left, right, cstr!("bop")),
        (Operator::Add, Type::UInt) => LLVMBuildAdd(ctx.builder, left, right, cstr!("bop")),
        (Operator::Add, Type::Float) => LLVMBuildFAdd(ctx.builder, left, right, cstr!("bop")),

        (Operator::Sub, Type::Int) => LLVMBuildSub(ctx.builder, left, right, cstr!("bop")),
        (Operator::Sub, Type::UInt) => LLVMBuildSub(ctx.builder, left, right, cstr!("bop")),
        (Operator::Sub, Type::Float) => LLVMBuildFSub(ctx.builder, left, right, cstr!("bop")),

        (Operator::Mul, Type::Int) => LLVMBuildMul(ctx.builder, left, right, cstr!("bop")),
        (Operator::Mul, Type::UInt) => LLVMBuildMul(ctx.builder, left, right, cstr!("bop")),
        (Operator::Mul, Type::Float) => LLVMBuildFMul(ctx.builder, left, right, cstr!("bop")),

        (Operator::Div, Type::Int) => LLVMBuildSDiv(ctx.builder, left, right, cstr!("bop")),
        (Operator::Div, Type::UInt) => LLVMBuildUDiv(ctx.builder, left, right, cstr!("bop")),
        (Operator::Div, Type::Float) => LLVMBuildFDiv(ctx.builder, left, right, cstr!("bop")),

        (Operator::Mod, Type::Int) => LLVMBuildSRem(ctx.builder, left, right, cstr!("bop")),
        (Operator::Mod, Type::UInt) => LLVMBuildURem(ctx.builder, left, right, cstr!("bop")),

        (Operator::LessThan, Type::Int) => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntSLT, left, right, cstr!("bop")),
        (Operator::LessThan, Type::UInt) => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntULT, left, right, cstr!("bop")),
        (Operator::LessThan, Type::Float) => LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealULT, left, right, cstr!("bop")),
        (Operator::LessThan, Type::Char) => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntULT, left, right, cstr!("bop")),

        (Operator::GreaterThan, Type::Int) => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntSGT, left, right, cstr!("bop")),
        (Operator::GreaterThan, Type::UInt) => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntUGT, left, right, cstr!("bop")),
        (Operator::GreaterThan, Type::Float) => LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealUGT, left, right, cstr!("bop")),
        (Operator::GreaterThan, Type::Char) => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntUGT, left, right, cstr!("bop")),

        (Operator::LessThanEquals, Type::Int) => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntSLE, left, right, cstr!("bop")),
        (Operator::LessThanEquals, Type::UInt) => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntULE, left, right, cstr!("bop")),
        (Operator::LessThanEquals, Type::Float) => LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealULE, left, right, cstr!("bop")),
        (Operator::LessThanEquals, Type::Char) => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntULE, left, right, cstr!("bop")),

        (Operator::GreaterThanEquals, Type::Int) => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntSGE, left, right, cstr!("bop")),
        (Operator::GreaterThanEquals, Type::UInt) => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntUGE, left, right, cstr!("bop")),
        (Operator::GreaterThanEquals, Type::Float) => LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealUGE, left, right, cstr!("bop")),
        (Operator::GreaterThanEquals, Type::Char) => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntUGE, left, right, cstr!("bop")),

        (Operator::Equals, Type::Int) => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntEQ, left, right, cstr!("bop")),
        (Operator::Equals, Type::UInt) => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntEQ, left, right, cstr!("bop")),
        (Operator::Equals, Type::Float) => LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealUEQ, left, right, cstr!("bop")),
        (Operator::Equals, Type::Char) => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntEQ, left, right, cstr!("bop")),
        (Operator::Equals, Type::Bool) => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntEQ, left, right, cstr!("bop")),

        /*
        (Operator::Equals, Value::String(ref l), Value::String(ref r)) => Value::Bool(*l == *r),
        (Operator::Equals, Value::Optional(ref inner), Value::Nil) => Value::Bool(inner.is_nil()),
        (Operator::Equals, Value::Nil, Value::Nil) => Value::Bool(true),
        (Operator::Equals, _, Value::Nil) |
        (Operator::Equals, Value::Nil, _) => Value::Bool(false),
        */

        (Operator::NotEquals, Type::Int) => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntNE, left, right, cstr!("bop")),
        (Operator::NotEquals, Type::UInt) => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntNE, left, right, cstr!("bop")),
        (Operator::NotEquals, Type::Float) => LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealUNE, left, right, cstr!("bop")),
        (Operator::NotEquals, Type::Char) => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntNE, left, right, cstr!("bop")),
        (Operator::NotEquals, Type::Bool) => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntNE, left, right, cstr!("bop")),

        /*
        (Operator::NotEquals, Value::String(ref l), Value::String(ref r)) => Value::Bool(*l != *r),
        (Operator::NotEquals, Value::Optional(ref inner), Value::Nil) => Value::Bool(!inner.is_nil()),
        (Operator::NotEquals, Value::Nil, Value::Nil) => Value::Bool(false),
        (Operator::NotEquals, _, Value::Nil) |
        (Operator::NotEquals, Value::Nil, _) => Value::Bool(true),
*/
        (Operator::And, Type::Bool) => LLVMBuildAnd(ctx.builder, left, right, cstr!("bop")),
        (Operator::Or, Type::Bool) => LLVMBuildOr(ctx.builder, left, right, cstr!("bop")),

        /*
        (Operator::Or, Value::Optional(inner), right) => {
            if inner.is_nil() {
                right
            } else {
                inner.deref().clone()
            }
        }
        */

        (_, t) => panic!("Operator {} not supported on type {}", op, t),
    };


    let dst_var = ctx.get_variable(&dst.name).expect("Unknown variable");
    dst_var.value.store(ctx.builder, value);
}

pub unsafe fn gen_instruction(ctx: &mut Context, instr: &Instruction, blocks: &HashMap<BasicBlockRef, LLVMBasicBlockRef>)
{
    match *instr
    {
        Instruction::Store{ref dst, ref src} => {
            let vr = get_operand(ctx, src);
            let dst_var = ctx.get_variable(&dst.name).expect("Unknown variable");
            dst_var.value.store(ctx.builder, vr);
        }

        Instruction::Load{ref dst, ref ptr} => {
            let dst_var = ctx.get_variable(&dst.name).expect("Unknown variable");
            let src_var = ctx.get_variable(&ptr.name).expect("Unknown variable");
            dst_var.value.store(ctx.builder, src_var.value.load(ctx.builder));
        }

        Instruction::LoadMember{ref dst, ref obj, ref member_index} => {
            let index = get_operand(ctx, member_index);
            let dst_var = ctx.get_variable(&dst.name).expect("Unknown variable");
            let obj_var = ctx.get_variable(&obj.name).expect("Unknown variable");
            let member_ptr = obj_var.value.get_member_ptr(ctx.context, ctx.builder, index);
            dst_var.value.store(ctx.builder, LLVMBuildLoad(ctx.builder, member_ptr, cstr!("memberload")));
        }

        Instruction::AddressOfMember{ref dst, ref obj, ref member_index} => {
            let index = get_operand(ctx, member_index);
            let dst_var = ctx.get_variable(&dst.name).expect("Unknown variable");
            let obj_var = ctx.get_variable(&obj.name).expect("Unknown variable");
            let member_ptr = obj_var.value.get_member_ptr(ctx.context, ctx.builder, index);
            dst_var.value.store(ctx.builder, member_ptr);
        }

        Instruction::StoreMember{ref obj, ref member_index, ref src} => {
            let src_val = get_operand(ctx, src);
            let idx = get_operand(ctx, member_index);
            let obj_var = ctx.get_variable(&obj.name).expect("Unknown variable");
            obj_var.value.store_member(ctx.context, ctx.builder, idx, src_val);
        }

        Instruction::AddressOf{ref dst, ref obj} => {
            panic!("NYI");
        }

        Instruction::GetProperty{ref dst, ref obj, ref prop} => {
            let dst_var = ctx.get_variable(&dst.name).expect("Unknown variable");
            let obj_var = ctx.get_variable(&obj.name).expect("Unknown variable");
            let value = obj_var.value.get_property(ctx.context, *prop);
            dst_var.value.store(ctx.builder, value);
        }

        Instruction::SetProperty{ref obj, ref prop, ref val} => {
            panic!("NYI");
        }

        Instruction::UnaryOp{ref dst, ref op, ref src} => {
            gen_unary_op(ctx, dst, *op, src);
        }

        Instruction::BinaryOp{ref dst, ref op, ref left, ref right} => {
            gen_binary_op(ctx, dst, *op, left, right);
        }

        Instruction::Call{ref dst, ref func, ref args} => {
            let func = ctx.get_function(func).expect("Unknown function");
            let dst_var = ctx.get_variable(&dst.name).expect("Unknown function");
            let mut func_args = args.iter().map(|a| get_operand(ctx, a)).collect::<Vec<_>>();
            unsafe {
                let ret = LLVMBuildCall(ctx.builder, func.function, func_args.as_mut_ptr(), args.len() as c_uint, cstr!("call"));
                dst_var.value.store(ctx.builder, ret);
            }
        }

        Instruction::Slice{ref dst, ref src, ref start, ref len} => {
            panic!("NYI");
        }

        Instruction::Cast{ref dst, ref src} => {
            panic!("NYI");
        }

        Instruction::GlobalAlloc(ref var) => {
            panic!("NYI");
        }

        Instruction::StackAlloc(ref var) => {
            let typ = ctx.resolve_type(&var.typ);
            let val = stack_alloc(ctx, typ, &var.name);
            ctx.add_variable(&var.name, ValueRef::new(val, &var.typ, true));
        }

        Instruction::HeapAlloc(ref var) => {
            let name = CString::new(&var.name[..]).expect("Invalid string");
            let value = LLVMBuildMalloc(ctx.builder, ctx.resolve_type(&var.typ), name.as_ptr());
            ctx.add_variable(&var.name, ValueRef::new(value, &var.typ, true))
        }

        Instruction::StartScope => {
        }

        Instruction::EndScope => {
        }

        Instruction::Return(ref operand) => {
            LLVMBuildRet(ctx.builder, get_operand(ctx, operand));
        }

        Instruction::ReturnVoid => {
            LLVMBuildRetVoid(ctx.builder);
        }

        Instruction::Branch(ref bbref) => {
            let llvm_bb = blocks.get(bbref).expect("Unknown basic block");
            LLVMBuildBr(ctx.builder, *llvm_bb);
        }

        Instruction::BranchIf{ref cond, ref on_true, ref on_false} => {
            let on_true_bb = blocks.get(on_true).expect("Unknown basic block");
            let on_false_bb = blocks.get(on_false).expect("Unknown basic block");
            LLVMBuildCondBr(ctx.builder, get_operand(ctx, cond), *on_true_bb, *on_false_bb);
        }

        Instruction::Delete(ref var) => {
            LLVMBuildFree(ctx.builder, get_variable(ctx, &var.name));
        }

        Instruction::Exit => {
        }
    }
}
