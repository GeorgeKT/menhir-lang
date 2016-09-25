use std::ptr;
use std::ffi::CString;
use std::rc::Rc;
use libc;

use llvm::core::*;
use llvm::prelude::*;
use llvm::*;

use ast::*;
use codegen::*;
use llrep::*;


unsafe fn make_function_instance(ctx: &Context, sig: &FunctionSignature) -> FunctionInstance
{
    let mut ret_type = ctx.resolve_type(&sig.return_type);
    let mut arg_types_with_passing_mode: Vec<_> = sig.args.iter().map(|arg| {
        let arg_type = ctx.resolve_type(&arg.typ);
        match arg.passing_mode
        {
            ArgumentPassingMode::ByValue => {
                (arg_type, arg.passing_mode)
            },
            ArgumentPassingMode::ByPtr => {
                let arg_ptr_type = LLVMPointerType(arg_type, 0);
                (arg_ptr_type, arg.passing_mode)
            }
        }
    }).collect();

    if sig.return_type.return_by_ptr() {
        arg_types_with_passing_mode.push((LLVMPointerType(ret_type, 0), ArgumentPassingMode::ByPtr));
        ret_type = LLVMVoidTypeInContext(ctx.context);
    }

    FunctionInstance{
        name: sig.name.clone(),
        args: arg_types_with_passing_mode,
        return_type: ret_type,
        function: ptr::null_mut(),
        sig: sig.clone(),
    }
}

pub unsafe fn gen_function_sig(ctx: &mut Context, sig: &FunctionSignature) -> FunctionInstance
{
    let mut fi = make_function_instance(ctx, sig);
    let mut arg_types: Vec<LLVMTypeRef> = fi.args.iter().map(|&(typ, _)| typ).collect();
    let function_type = LLVMFunctionType(fi.return_type, arg_types.as_mut_ptr(), arg_types.len() as libc::c_uint, 0);
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
    let bb = LLVMAppendBasicBlockInContext(ctx.context, fi.function, cstr!("entry"));
    let current_bb = LLVMGetInsertBlock(ctx.builder);
    LLVMPositionBuilderAtEnd(ctx.builder, bb);

    ctx.push_stack(fi.function);

    for (i, arg) in func.sig.args.iter().enumerate() {
        let var = LLVMGetParam(fi.function, i as libc::c_uint);
        let (_, mode) = fi.args[i];
        match mode
        {
            ArgumentPassingMode::ByPtr => {
                match arg.typ
                {
                    Type::Func(ref ft) => {
                        let func_sig = anon_sig(&arg.name, &ft.return_type, &ft.args);
                        let fi = gen_function_ptr(ctx, var, func_sig);
                        ctx.add_function(Rc::new(fi));
                    },
                    _ => ctx.add_variable(&arg.name, ValueRef::new(var, &arg.typ)),
                }
            },
            ArgumentPassingMode::ByValue => {
                ctx.add_variable(&arg.name, ValueRef::Const(var));
            },
        }
    }

    for instr in &func.instructions {
        gen_instruction(ctx, instr);
    }

    ctx.pop_stack();
}
