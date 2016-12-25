use std::ptr;
use std::ffi::CString;
use std::rc::Rc;
use std::collections::HashMap;
use libc;

use llvm::core::*;
use llvm::prelude::*;

use ast::*;
use codegen::*;
use bytecode::*;


unsafe fn make_function_instance(ctx: &Context, sig: &FunctionSignature) -> FunctionInstance
{
    let arg_type = |typ: &Type| {
        let rt = ctx.resolve_type(typ);
        if typ.pass_by_ptr() {
            LLVMPointerType(rt, 0)
        } else {
            rt
        }
    };

    let ret_type = arg_type(&sig.return_type);
    let arg_types: Vec<_> = sig.args.iter().map(|arg| arg_type(&arg.typ)).collect();

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
                if arg.typ.allocate_on_heap() {
                    ctx.add_variable(&arg.name, ValueRef::new(var, &arg.typ));
                } else {
                    ctx.add_variable(&arg.name, ValueRef::Const(var));
                }
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
