use std::ptr;
use std::ffi::CString;
use std::collections::HashMap;
use std::rc::Rc;
use libc;
use llvm::core::*;
use llvm::prelude::*;

use libcobra::ast::*;
use libcobra::bytecode::*;
use super::symboltable::FunctionInstance;
use super::context::Context;
use super::instructions::*;
use super::valueref::ValueRef;


fn make_function_instance(ctx: &Context, sig: &FunctionSignature) -> FunctionInstance
{
    let ret_type = ctx.resolve_type(&sig.return_type);
    let arg_types: Vec<_> = sig.args.iter().map(|arg| ctx.resolve_type(&arg.typ)).collect();

    FunctionInstance{
        name: sig.name.clone(),
        args: arg_types,
        return_type: ret_type,
        function: ptr::null_mut(),
        sig: sig.clone(),
    }
}

unsafe fn gen_function_sig(ctx: &mut Context, sig: &FunctionSignature) -> FunctionInstance
{
    let mut fi = make_function_instance(ctx, sig);
    let function_type = LLVMFunctionType(fi.return_type, fi.args.as_mut_ptr(), fi.args.len() as libc::c_uint, 0);
    let name = CString::new(sig.name.as_bytes()).expect("Invalid string");
    fi.function = LLVMAddFunction(ctx.module, name.into_raw(), function_type);
    fi
}

unsafe fn gen_function_ptr(ctx: &Context, func_ptr: LLVMValueRef, sig: FunctionSignature) -> FunctionInstance
{
    let mut fi = make_function_instance(ctx, &sig);
    fi.function = func_ptr;
    fi
}

pub unsafe fn gen_function(ctx: &mut Context, func: &ByteCodeFunction)
{
    let fi = ctx.get_function(&func.sig.name).expect("Internal Compiler Error: Unknown function");
    let entry_bb = LLVMAppendBasicBlockInContext(ctx.context, fi.function, cstr!("entry"));
    LLVMPositionBuilderAtEnd(ctx.builder, entry_bb);

    ctx.push_stack(fi.function);

    for (i, arg) in func.sig.args.iter().enumerate() {
        let var = LLVMGetParam(fi.function, i as libc::c_uint);
        match arg.typ
        {
            Type::Func(ref ft) => {
                let func_sig = anon_sig(&arg.name, &ft.return_type, &ft.args);
                let fi = gen_function_ptr(ctx, var, func_sig);
                ctx.add_variable(&arg.name, ValueRef::new(fi.function, &arg.typ));
                ctx.add_function(Rc::new(fi));
            },
            _ => {
                ctx.add_variable(&arg.name, ValueRef::new(var, &arg.typ));
            },
        }
    }

    let mut blocks = HashMap::new();
    blocks.insert(0, entry_bb);

    for (bb_ref, bb) in func.blocks.iter() {
        if bb.name != "entry" {
            let bb_name = CString::new(bb.name.as_bytes()).expect("Invalid block name");
            let new_bb = LLVMAppendBasicBlockInContext(ctx.context, fi.function, bb_name.as_ptr());
            blocks.insert(*bb_ref, new_bb);
        }
    }

    for block in func.blocks.values() {
        for inst in &block.instructions {
            gen_instruction(ctx, inst, &blocks);
        }
    }
}
