use super::context::Context;
use super::operand::gen_operand;
use super::operand::gen_operand_dst;
use super::symboltable::FunctionInstance;
use super::valueref::ValueRef;
use crate::ast::FunctionSignature;
use crate::ast::Type;
use crate::compileerror::code_gen_error;
use crate::compileerror::CompileResult;
use crate::lazycode::*;
use llvm_sys::core::*;
use llvm_sys::prelude::*;
use std::collections::BTreeMap;
use std::rc::Rc;

#[allow(unused)]
pub unsafe fn type_name(typ: LLVMTypeRef) -> String {
    use std::ffi::CStr;
    let name = LLVMPrintTypeToString(typ);
    let ret_name = String::from(CStr::from_ptr(name).to_str().expect("Invalid string"));
    LLVMDisposeMessage(name);
    ret_name
}

unsafe fn add_variable(ctx: &mut Context, name: &str, vr: ValueRef, typ: &Type) -> CompileResult<()> {
    if typ.is_function() {
        let fi = Rc::new(FunctionInstance {
            function: vr.load(ctx)?.value,
            name: name.into(),
            sig: FunctionSignature::from_type(name, typ)
                .ok_or_else(|| code_gen_error(format!("Cannot generate function signature for {name}")))?,
        });
        ctx.add_function(fi)?;
    }

    ctx.set_variable(name, vr)?;
    Ok(())
}

pub unsafe fn gen_instruction(
    ctx: &mut Context,
    func: &ByteCodeFunction,
    instr: &Instruction,
    blocks: &BTreeMap<usize, LLVMBasicBlockRef>,
) -> CompileResult<()> {
    match instr {
        Instruction::Mark { .. } => Ok(()),
        Instruction::Exec { operand } => {
            gen_operand(ctx, operand, None)?;
            Ok(())
        }
        Instruction::Declare { name, init, typ } => {
            let vr = ctx.stack_alloc(name, typ)?;
            if let Some(init) = init {
                gen_operand_dst(ctx, init, &vr)?;
            }

            add_variable(ctx, name, vr, typ)
        }
        Instruction::Alias { name, value, typ } => {
            let vr = gen_operand(ctx, value, None)?;
            add_variable(ctx, name, vr, typ)
        }
        Instruction::Store { dst, value } => {
            let dst_var = gen_operand(ctx, dst, None)?;
            gen_operand_dst(ctx, value, &dst_var)?;
            Ok(())
        }
        Instruction::Branch { to } => {
            let llvm_bb = blocks.get(&to.id).expect("Unknown basic block");
            LLVMBuildBr(ctx.builder, *llvm_bb);
            Ok(())
        }
        Instruction::BranchIf {
            cond,
            on_true,
            on_false,
        } => {
            let on_true_bb = blocks.get(&on_true.id).expect("Unknown basic block");
            let on_false_bb = blocks.get(&on_false.id).expect("Unknown basic block");
            let cond = gen_operand(ctx, cond, None)?;
            LLVMBuildCondBr(ctx.builder, cond.value, *on_true_bb, *on_false_bb);
            Ok(())
        }
        Instruction::Return { value } => {
            let val = gen_operand(ctx, value, None)?;
            if val.typ == Type::Void || func.sig.rvo {
                LLVMBuildRetVoid(ctx.builder);
            } else {
                LLVMBuildRet(ctx.builder, val.value);
            }
            Ok(())
        }
        Instruction::Delete { object } => {
            let var = gen_operand(ctx, object, None)?;
            LLVMBuildFree(ctx.builder, var.value);
            Ok(())
        }
        Instruction::Label { label } => {
            let llvm_bb = blocks.get(&label.id).expect("Unknown basic block");
            LLVMPositionBuilderAtEnd(ctx.builder, *llvm_bb);
            Ok(())
        }
    }
}
