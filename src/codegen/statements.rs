use std::ptr;
use llvm::core::*;

use ast::*;
use codegen::*;
use compileerror::*;

#[allow(unused_variables)]
fn gen_import(ctx: &mut Context, import: &Import) -> Result<(), CompileError>
{
     err(Pos::new(0, 0), ErrorType::UnexpectedEOF)
}

#[allow(unused_variables)]
fn gen_variable(ctx: &mut Context, v: &Variable) -> Result<(), CompileError>
{
     err(Pos::new(0, 0), ErrorType::UnexpectedEOF)
}

#[allow(unused_variables)]
fn gen_function(ctx: &mut Context, f: &Function) -> Result<(), CompileError>
{
     err(Pos::new(0, 0), ErrorType::UnexpectedEOF)
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

#[allow(unused_variables)]
fn gen_return(ctx: &mut Context, f: &Return) -> Result<(), CompileError>
{
     err(Pos::new(0, 0), ErrorType::UnexpectedEOF)
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

    for s in &prog.block.statements {
        try!(gen_statement(ctx, s));
    }

    LLVMBuildRet(ctx.builder, LLVMConstInt(main_ret_type, 42, 0));
    Ok(())
}
