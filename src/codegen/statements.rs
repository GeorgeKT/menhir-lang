use std::ptr;
use std::ffi::CStr;
use llvm::core::*;
use llvm::prelude::*;
use libc;

use ast::*;
use codegen::*;
use compileerror::*;

pub unsafe fn type_name(tr: LLVMTypeRef) -> String
{
    let n = LLVMPrintTypeToString(tr);
    let name = CStr::from_ptr(n).to_str().expect("Invalid C String").to_owned();
    LLVMDisposeMessage(n);
    name
}

#[allow(unused_variables)]
fn gen_import(ctx: &mut Context, import: &Import) -> Result<(), CompileError>
{
     err(Pos::new(0, 0), ErrorType::UnexpectedEOF)
}

unsafe fn gen_variable(ctx: &mut Context, v: &Variable) -> Result<(), CompileError>
{
    if ctx.has_variable(&v.name) {
        return err(v.span.start, ErrorType::RedefinitionOfVariable(v.name.clone()));
    }

    let initial_value = try!(gen_expression(ctx, &v.init));
    let initial_value_type = LLVMTypeOf(initial_value);

    if let Some(ref tr) = v.typ {
        if let Some(llvm_type_ref) = ctx.resolve_type(&tr) {
            if llvm_type_ref != initial_value_type {
                return err(v.span.start, ErrorType::TypeError(format!("Mismatched types in initialization")))
            }
        } else {
            return err(v.span.start, ErrorType::TypeError(format!("Unknown type '{}'", tr)));
        }
    }

    let var = LLVMBuildAlloca(ctx.builder, initial_value_type, cstr("var"));
    LLVMBuildStore(ctx.builder, initial_value, var);

    let mut sf = ctx.top_stack_frame();
    sf.add_variable(&v.name, var, v.is_const);
    Ok(())
}

unsafe fn gen_function_sig(ctx: &mut Context, sig: &FunctionSignature, span: &Span) -> Result<(LLVMValueRef, LLVMTypeRef, Vec<LLVMTypeRef>), CompileError>
{
    let ret_type = try!(ctx
        .resolve_type(&sig.return_type)
        .ok_or(CompileError::new(span.start, ErrorType::TypeError(format!("Cannot resolve the return type of function '{}'", sig.name)))));

    let mut arg_types = Vec::new();
    for arg in &sig.args {
        let arg_type = try!(ctx
            .resolve_type(&arg.typ)
            .ok_or(CompileError::new(arg.span.start, ErrorType::TypeError(format!("Cannot resolve the type of argument '{}'", arg.name)))));
        arg_types.push(arg_type);
    }

    let function_type = LLVMFunctionType(ret_type, arg_types.as_mut_ptr(), arg_types.len() as libc::c_uint, 0);
    let function = LLVMAddFunction(ctx.module, cstr(&sig.name), function_type);
    Ok((function, ret_type, arg_types))
}

unsafe fn gen_function(ctx: &mut Context, f: &Function) -> Result<(), CompileError>
{
    if ctx.has_function(&f.sig.name) {
        return err(f.span.start, ErrorType::RedefinitionOfFunction(f.sig.name.clone()));
    }

    let (function, ret_type, arg_types) = try!(gen_function_sig(ctx, &f.sig, &f.span));
    let bb = LLVMAppendBasicBlockInContext(ctx.context, function, cstr("entry"));
    LLVMPositionBuilderAtEnd(ctx.builder, bb);


    ctx.top_stack_frame().add_function(FunctionInstance{
        name: f.sig.name.clone(),
        args: arg_types.clone(),
        return_type: ret_type,
        function: function,
    });

    ctx.push_stack_frame(function, bb);

    for (i, arg) in f.sig.args.iter().enumerate() {
        let var = LLVMGetParam(function, i as libc::c_uint);
        let alloc = LLVMBuildAlloca(ctx.builder, arg_types[i], cstr("argtmp"));
        LLVMBuildStore(ctx.builder, var, alloc);
        ctx.top_stack_frame().add_variable(&arg.name, alloc, arg.constant);
    }

    for s in &f.block.statements {
        try!(gen_statement(ctx, s));
    }

    if f.sig.return_type == Type::Void {
        LLVMBuildRetVoid(ctx.builder);
    }

    ctx.pop_stack_frame();
    LLVMPositionBuilderAtEnd(ctx.builder, ctx.top_stack_frame().get_current_bb());
    Ok(())
}

#[allow(unused_variables)]
unsafe fn gen_external_function(ctx: &mut Context, f: &ExternalFunction) -> Result<(), CompileError>
{
    let (function, ret_type, arg_types) = try!(gen_function_sig(ctx, &f.sig, &f.span));

    ctx.top_stack_frame().add_function(FunctionInstance{
        name: f.sig.name.clone(),
        args: arg_types.clone(),
        return_type: ret_type,
        function: function,
    });
    Ok(())
}

#[allow(unused_variables)]
fn gen_while(ctx: &mut Context, f: &While) -> Result<(), CompileError>
{
     err(Pos::new(0, 0), ErrorType::UnexpectedEOF)
}

#[allow(unused_variables)]
fn gen_if(ctx: &mut Context, f: &If) -> Result<(), CompileError>
{
     err(Pos::new(0, 0), ErrorType::UnexpectedEOF)
}

unsafe fn gen_return(ctx: &mut Context, f: &Return) -> Result<(), CompileError>
{
    let ret = try!(gen_expression(ctx, &f.expr));
    let builder = ctx.builder;
    let sf = ctx.top_stack_frame();
    let ret_type =  LLVMTypeOf(ret);
    let func_type = sf.return_type();
    if ret_type != func_type {
        err(f.span.start, ErrorType::TypeError(
            format!("Attempting to return type '{}' expecting '{}'", type_name(ret_type), type_name(func_type))))
    } else {
        LLVMBuildRet(builder, ret);
        Ok(())
    }
}

#[allow(unused_variables)]
fn gen_struct(ctx: &mut Context, f: &Struct) -> Result<(), CompileError>
{
     err(Pos::new(0, 0), ErrorType::UnexpectedEOF)
}

#[allow(unused_variables)]
fn gen_union(ctx: &mut Context, f: &Union) -> Result<(), CompileError>
{
     err(Pos::new(0, 0), ErrorType::UnexpectedEOF)
}

#[allow(unused_variables)]
fn gen_match(ctx: &mut Context, f: &Match) -> Result<(), CompileError>
{
     err(Pos::new(0, 0), ErrorType::UnexpectedEOF)
}

unsafe fn gen_statement(ctx: &mut Context, stmt: &Statement) -> Result<(), CompileError>
{
    match *stmt {
        Statement::Import(ref i) => gen_import(ctx, i),
        Statement::Variable(ref vars) => {
            for v in vars {
                try!(gen_variable(ctx, v))
            }
            Ok(())
        },
        Statement::Function(ref fun) => gen_function(ctx, fun),
        Statement::ExternalFunction(ref fun) => gen_external_function(ctx, fun),
        Statement::While(ref w) => gen_while(ctx, w),
        Statement::If(ref i) => gen_if(ctx, i),
        Statement::Return(ref r) => gen_return(ctx, r),
        Statement::Struct(ref s) => gen_struct(ctx, s),
        Statement::Union(ref u) => gen_union(ctx, u),
        Statement::Match(ref m) => gen_match(ctx, m),
        Statement::Expression(ref e) => gen_expression(ctx, e).map(|_| ()),
    }
}

pub unsafe fn gen_program(ctx: &mut Context, prog: &Program) -> Result<(), CompileError>
{
    let main_ret_type = LLVMInt64TypeInContext(ctx.context);
    let function_type = LLVMFunctionType(main_ret_type, ptr::null_mut(), 0, 0);
    let function = LLVMAddFunction(ctx.module, cstr("main"), function_type);
    let bb = LLVMAppendBasicBlockInContext(ctx.context, function, cstr("entry"));
    LLVMPositionBuilderAtEnd(ctx.builder, bb);

    ctx.push_stack_frame(function, bb);
    for s in &prog.block.statements {
        try!(gen_statement(ctx, s));
    }
    Ok(())
}
