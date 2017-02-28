use std::collections::HashMap;
use std::ffi::CString;
use libc::*;
use llvm::core::*;
use llvm::prelude::*;
use libcobra::bytecode::*;
use libcobra::ast::{Type, Operator};
use super::valueref::ValueRef;
use super::context::Context;

unsafe fn const_int(ctx: &Context, v: i64) -> LLVMValueRef
{
    LLVMConstInt(LLVMInt64TypeInContext(ctx.context), v as c_ulonglong, 1)
}

unsafe fn const_uint(ctx: &Context, v: u64) -> LLVMValueRef
{
    LLVMConstInt(LLVMInt64TypeInContext(ctx.context), v, 0)
}

unsafe fn const_bool(ctx: &Context, v: bool) -> LLVMValueRef
{
    LLVMConstInt(LLVMInt1TypeInContext(ctx.context), if v {1} else {0}, 0)
}

unsafe fn const_float(ctx: &Context, v: f64) -> LLVMValueRef
{
    LLVMConstReal(LLVMDoubleTypeInContext(ctx.context), v)
}

unsafe fn const_char(ctx: &Context, c: u8) -> LLVMValueRef
{
    LLVMConstInt(LLVMInt32TypeInContext(ctx.context), c as c_ulonglong, 0)
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

        Operand::Int(v) => const_int(ctx, v),
        Operand::UInt(v) => const_uint(ctx, v),
        Operand::Float(v) => const_float(ctx, v),
        Operand::Char(v) => const_char(ctx, v),
        Operand::String(ref _s) => panic!("NYI"),
        Operand::Bool(v) => const_bool(ctx, v),
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

unsafe fn get_unary_op(ctx: &Context, dst: &Var, operator: Operator, src: &Operand)
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
            panic!("NYI");
        }

        Instruction::AddressOf{ref dst, ref obj} => {
            panic!("NYI");
        }

        Instruction::GetProperty{ref dst, ref obj, ref prop} => {
            panic!("NYI");
        }

        Instruction::SetProperty{ref obj, ref prop, ref val} => {
            panic!("NYI");
        }

        Instruction::UnaryOp{ref dst, ref op, ref src} => {
            get_unary_op(ctx, dst, *op, src);
        }

        Instruction::BinaryOp{ref dst, ref op, ref left, ref right} => {
            panic!("NYI");
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
