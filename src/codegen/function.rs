use std::ptr;
use std::ffi::CString;
use std::rc::Rc;
use std::collections::HashMap;
use libc;

use llvm::core::*;
use llvm::prelude::*;

use ast::*;
use codegen::*;
use llrep::*;


unsafe fn make_function_instance(ctx: &Context, sig: &FunctionSignature) -> FunctionInstance
{
    let mut ret_type = ctx.resolve_type(&sig.return_type);
    let mut arg_types: Vec<_> = sig.args.iter().map(|arg| {
        let arg_type = ctx.resolve_type(&arg.typ);
        if arg.typ.pass_by_ptr() {
            LLVMPointerType(arg_type, 0)
        } else {
            arg_type
        }
    }).collect();

    if sig.return_type.return_by_ptr() {
        arg_types.push(LLVMPointerType(ret_type, 0));
        ret_type = LLVMVoidTypeInContext(ctx.context);
    }

    FunctionInstance{
        name: sig.name.clone(),
        args: arg_types,
        return_type: ret_type,
        function: ptr::null_mut(),
        sig: sig.clone(),
    }
}

pub unsafe fn gen_function_sig(ctx: &mut Context, sig: &FunctionSignature) -> FunctionInstance
{
    let mut fi = make_function_instance(ctx, sig);
    let function_type = LLVMFunctionType(fi.return_type, fi.args.as_mut_ptr(), fi.args.len() as libc::c_uint, 0);
    let name = CString::new(sig.name.as_bytes()).expect("Invalid string");
    fi.function = LLVMAddFunction(ctx.module, name.into_raw(), function_type);
    fi
}

pub unsafe fn gen_function_ptr(ctx: &Context, func_ptr: LLVMValueRef, sig: FunctionSignature) -> FunctionInstance
{
    let mut fi = make_function_instance(ctx, &sig);
    fi.function = func_ptr;
    fi
}

pub unsafe fn gen_function(ctx: &mut Context, func: &LLFunction)
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
                if arg.typ.pass_by_ptr() {
                    ctx.add_variable(&arg.name, ValueRef::new(var, &arg.typ));
                } else {
                    ctx.add_variable(&arg.name, ValueRef::Const(var));
                }
            },
        }
    }

    if func.sig.return_type.return_by_ptr()
    {
        let ret_var = LLVMGetParam(fi.function, func.sig.args.len() as libc::c_uint);
        ctx.add_variable("$ret", ValueRef::new(ret_var, &func.sig.return_type));
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

    for bb_ref in func.block_order.iter() {
        let bb = func.blocks.get(bb_ref).expect("Unknown basic block");
        for instr in &bb.instructions {
            let llvm_bb = blocks.get(bb_ref).expect("Unknown basic block");
            LLVMPositionBuilderAtEnd(ctx.builder, *llvm_bb);
            gen_instruction(ctx, instr, &blocks);
        }
    }
    ctx.pop_stack();
}
