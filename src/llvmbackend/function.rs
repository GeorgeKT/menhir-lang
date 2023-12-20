use llvm_sys::core::*;
use llvm_sys::prelude::*;
use std::collections::HashMap;
use std::ffi::CString;
use std::ptr;
use std::rc::Rc;

use super::context::Context;
use super::instructions::*;
use super::symboltable::FunctionInstance;
use super::valueref::ValueRef;
use crate::ast::*;
use crate::compileerror::{code_gen_error, CompileResult};
use crate::lazycode::*;

pub unsafe fn gen_function_sig(
    ctx: &mut Context,
    sig: &FunctionSignature,
    name_override: Option<&str>,
) -> CompileResult<()> {
    let ret_type = ctx.resolve_type(&sig.return_type)?;
    let mut arg_types: Vec<_> = Vec::new();
    for arg in &sig.args {
        let llvm_type = ctx.resolve_type(&arg.typ)?;
        if arg.typ.pass_by_value() {
            arg_types.push(llvm_type);
        } else {
            arg_types.push(LLVMPointerType(llvm_type, 0));
        }
    }

    let function_type = LLVMFunctionType(ret_type, arg_types.as_mut_ptr(), arg_types.len() as libc::c_uint, 0);
    let llvm_name = name_override.unwrap_or(&sig.name);
    let cstring = CString::new(llvm_name.as_bytes()).map_err(|_| code_gen_error("Invalid string"))?;
    let name = cstring.as_ptr();
    let func = LLVMAddFunction(ctx.module, name, function_type);

    let fi = FunctionInstance::new(func, &sig.name, sig.clone());
    ctx.add_function(Rc::new(fi))?;
    Ok(())
}

pub unsafe fn gen_function_ptr(
    ctx: &mut Context,
    name: impl Into<String>,
    func_ptr: LLVMValueRef,
    sig: FunctionSignature,
) -> CompileResult<()> {
    let name: String = name.into();
    let fi = FunctionInstance::new(func_ptr, name, sig);
    ctx.add_function(Rc::new(fi))
}

unsafe fn gen_scope(
    ctx: &mut Context,
    func: &ByteCodeFunction,
    scope: &Scope,
    blocks: &HashMap<usize, LLVMBasicBlockRef>,
) -> CompileResult<()> {
    for n in &scope.nodes {
        match n {
            ScopeNode::Instruction(i) => gen_instruction(ctx, func, i, blocks)?,
            ScopeNode::Scope(s) => {
                ctx.push_stack(ptr::null_mut());
                gen_scope(ctx, func, s, blocks)?;
                ctx.pop_stack();
            }
        }
    }
    Ok(())
}

pub unsafe fn gen_function(ctx: &mut Context, func: &ByteCodeFunction) -> CompileResult<()> {
    //println!("gen_function {}", func.sig.name);
    let fi = ctx.get_function(&func.sig.name)?;

    let entry_bb = LLVMAppendBasicBlockInContext(ctx.context, fi.function, cstr!("entry"));
    LLVMPositionBuilderAtEnd(ctx.builder, entry_bb);

    ctx.push_stack(fi.function);

    for (i, arg) in func.sig.args.iter().enumerate() {
        let var = LLVMGetParam(fi.function, i as libc::c_uint);
        match &arg.typ {
            Type::Func(_ft) => {
                let sig =
                    FunctionSignature::from_type(&arg.name, &arg.typ).expect("Already checked it is a function type");
                gen_function_ptr(ctx, &arg.name, var, sig)?;
                ctx.set_variable(&arg.name, ValueRef::new(var, arg.typ.clone()))?;
            }

            _ => {
                if arg.typ.pass_by_value() {
                    if arg.mutable && !arg.typ.is_pointer() {
                        // To make it mutable, copy the argument into a local variable
                        // and use that instead
                        let argcopy = LLVMBuildAlloca(ctx.builder, ctx.resolve_type(&arg.typ)?, cstr!("argcopy"));
                        LLVMBuildStore(ctx.builder, var, argcopy);
                        ctx.set_variable(&arg.name, ValueRef::new(argcopy, ptr_type(arg.typ.clone())))?;
                    } else {
                        ctx.set_variable(&arg.name, ValueRef::new(var, arg.typ.clone()))?;
                    }
                } else {
                    ctx.set_variable(&arg.name, ValueRef::new(var, ptr_type(arg.typ.clone())))?;
                }
            }
        }
    }

    let mut blocks = HashMap::new();
    blocks.insert(0, entry_bb);

    func.toplevel_scope.for_each_instruction(&mut |inst| {
        if let Instruction::Label { label } = inst {
            let bb_name = CString::new(label.block_name().as_bytes()).expect("Invalid block name");
            let new_bb = LLVMAppendBasicBlockInContext(ctx.context, fi.function, bb_name.as_ptr());
            blocks.insert(label.id, new_bb);
        }
        true
    });

    gen_scope(ctx, func, &func.toplevel_scope, &blocks)?;

    ctx.pop_stack();
    Ok(())
}
