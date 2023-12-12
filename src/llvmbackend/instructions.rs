use super::context::Context;
use super::operand::gen_operand;
use super::operand::gen_operand_dst;
use super::valueref::ValueRef;
use crate::ast::ptr_type;
use crate::ast::Type;
use crate::compileerror::CompileResult;
use crate::lazycode::*;
use llvm_sys::core::*;
use llvm_sys::prelude::*;
use std::collections::HashMap;
use std::ptr;

#[allow(unused)]
pub unsafe fn type_name(typ: LLVMTypeRef) -> String {
    use std::ffi::CStr;
    let name = LLVMPrintTypeToString(typ);
    let ret_name = String::from(CStr::from_ptr(name).to_str().expect("Invalid string"));
    LLVMDisposeMessage(name);
    ret_name
}

pub unsafe fn gen_instruction(
    ctx: &mut Context,
    func: &ByteCodeFunction,
    instr: &Instruction,
    blocks: &HashMap<BasicBlockRef, LLVMBasicBlockRef>,
) -> CompileResult<()> {
    match instr {
        Instruction::Exec { operand } => {
            gen_operand(ctx, operand, None)?;
            Ok(())
        }
        Instruction::Declare { name, init, typ } => {
            let vr = ValueRef::new(ctx.stack_alloc(name, typ)?, ptr_type(typ.clone()));
            if let Some(init) = init {
                gen_operand_dst(ctx, init, &vr)?;
            }
            ctx.set_variable(name, vr)?;
            Ok(())
        }
        Instruction::Store { dst, value } => {
            let dst = gen_operand(ctx, dst, None)?;
            let _value = gen_operand_dst(ctx, value, &dst)?;
            Ok(())
        }
        Instruction::Branch { block } => {
            let llvm_bb = blocks.get(block).expect("Unknown basic block");
            LLVMBuildBr(ctx.builder, *llvm_bb);
            Ok(())
        }
        Instruction::BranchIf {
            cond,
            on_true,
            on_false,
        } => {
            let on_true_bb = blocks.get(on_true).expect("Unknown basic block");
            let on_false_bb = blocks.get(on_false).expect("Unknown basic block");
            let cond = gen_operand(ctx, cond, None)?;
            LLVMBuildCondBr(ctx.builder, cond.load(ctx)?, *on_true_bb, *on_false_bb);
            Ok(())
        }
        Instruction::Return { value } => {
            let val = gen_operand(ctx, value, None)?;
            if val.typ == Type::Void || func.sig.rvo {
                LLVMBuildRetVoid(ctx.builder);
            } else {
                LLVMBuildRet(ctx.builder, val.load(ctx)?);
            }
            Ok(())
        }
        Instruction::Delete { object } => {
            let var = gen_operand(ctx, object, None)?;
            LLVMBuildFree(ctx.builder, var.load(ctx)?);
            Ok(())
        }
        Instruction::ScopeStart => {
            ctx.push_stack(ptr::null_mut());
            Ok(())
        }
        Instruction::ScopeEnd => {
            ctx.pop_stack();
            Ok(())
        }
    }
}
