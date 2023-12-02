use llvm_sys::core::*;
use llvm_sys::prelude::*;
use std::collections::HashMap;
use std::ffi::CString;
use std::rc::Rc;

use super::context::Context;
use super::instructions::*;
use super::symboltable::FunctionInstance;
use super::valueref::ValueRef;
use crate::ast::*;
use crate::bytecode::*;
use crate::compileerror::{code_gen_error, code_gen_result, CompileResult};
use crate::span::Span;

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
    let fi = FunctionInstance::new(&sig.name, func, sig.return_type.clone(), sig.get_type(), sig.rvo);
    ctx.add_function(Rc::new(fi))?;
    Ok(())
}

pub unsafe fn gen_function_ptr(
    ctx: &mut Context,
    name: &str,
    func_ptr: LLVMValueRef,
    return_type: Type,
    typ: Type,
    rvo: bool,
) -> CompileResult<()> {
    let fi = FunctionInstance::new(name, func_ptr, return_type, typ, rvo);
    ctx.add_function(Rc::new(fi))
}

pub unsafe fn gen_function(ctx: &mut Context, func: &ByteCodeFunction) -> CompileResult<()> {
    let Some(fi) = ctx.get_function(&func.sig.name) else {
        return code_gen_result("Internal Compiler Error: Unknown function");
    };

    let entry_bb = LLVMAppendBasicBlockInContext(ctx.context, fi.function, cstr!("entry"));
    LLVMPositionBuilderAtEnd(ctx.builder, entry_bb);

    ctx.push_stack(fi.function);

    for (i, arg) in func.sig.args.iter().enumerate() {
        let var = LLVMGetParam(fi.function, i as libc::c_uint);
        match arg.typ {
            Type::Func(ref ft) => {
                let rvo = arg.name == RVO_PARAM_NAME;
                gen_function_ptr(ctx, &arg.name, var, ft.return_type.clone(), arg.typ.clone(), rvo)?;
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

    for (bb_ref, bb) in &func.blocks {
        if bb.name != "entry" {
            let bb_name = CString::new(bb.name.as_bytes()).map_err(|_| code_gen_error("Invalid block name"))?;
            let new_bb = LLVMAppendBasicBlockInContext(ctx.context, fi.function, bb_name.as_ptr());
            blocks.insert(*bb_ref, new_bb);
        }
    }

    for (bb_ref, block) in &func.blocks {
        let bb = blocks
            .get(bb_ref)
            .ok_or_else(|| code_gen_error("Unknown basic block"))?;
        LLVMPositionBuilderAtEnd(ctx.builder, *bb);
        for inst in &block.instructions {
            gen_instruction(ctx, inst, &blocks)?;
        }
    }

    ctx.pop_stack();
    Ok(())
}

pub unsafe fn add_libc_functions(ctx: &mut Context) -> CompileResult<()> {
    // memcpy
    let memcpy_sig = sig(
        "memcpy",
        ptr_type(Type::Void),
        vec![
            Argument::new("dst", ptr_type(Type::Void), false, Span::default()),
            Argument::new("src", ptr_type(Type::Void), false, Span::default()),
            Argument::new(
                "size",
                ctx.target_machine.target.native_uint_type.clone(),
                false,
                Span::default(),
            ),
        ],
        Span::default(),
    );

    gen_function_sig(ctx, &memcpy_sig, None)?;
    Ok(())
}
