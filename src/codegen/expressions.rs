use std::ptr;
use std::rc::Rc;

use libc;
use llvm::prelude::*;
use llvm::core::*;
use llvm::*;

use ast::{Expression, UnaryOp, BinaryOp, Function, FunctionSignature};
use codegen::{type_name, cstr};
use codegen::context::Context;
use codegen::valueref::ValueRef;
use codegen::symboltable::{FunctionInstance, VariableInstance};
use compileerror::{Span, CompileResult, CompileError, ErrorCode, err};


unsafe fn gen_integer(ctx: &mut Context, v: u64) -> CompileResult<ValueRef>
{
    Ok(ValueRef::new(LLVMConstInt(LLVMInt64TypeInContext(ctx.context), v, 0), true, ctx.builder))
}

unsafe fn gen_float(ctx: &Context, num: &str, span: &Span) -> CompileResult<ValueRef>
{
    match num.parse::<f64>() 
    {
        Ok(f) => Ok(ValueRef::new(LLVMConstReal(LLVMDoubleTypeInContext(ctx.context), f), true, ctx.builder)),
        Err(_) => err(span.start, ErrorCode::InvalidFloatingPoint, format!("{} is not a valid floating point number", num))
    }
}

unsafe fn gen_const_string_literal(ctx: &Context, s: &str) -> CompileResult<ValueRef>
{
    Ok(ValueRef::new(LLVMConstStringInContext(ctx.context, s.as_ptr() as *const i8, s.len() as u32, 1), true, ctx.builder))
}

unsafe fn gen_string_literal(ctx: &Context, s: &str, _span: &Span) -> CompileResult<ValueRef>
{
    let glob = LLVMAddGlobal(ctx.module, LLVMArrayType(LLVMInt8TypeInContext(ctx.context), s.len() as u32), cstr("string"));

    LLVMSetLinkage(glob, LLVMLinkage::LLVMInternalLinkage);
    LLVMSetGlobalConstant(glob, 1);

    // Initialize with string:
    LLVMSetInitializer(glob, try!(gen_const_string_literal(ctx, s)).load());
    Ok(ValueRef::new(glob, true, ctx.builder))
}

unsafe fn gen_unary_op(ctx: &mut Context, u: &UnaryOp) -> CompileResult<ValueRef>
{
    err(u.span.start, ErrorCode::UnexpectedEOF, format!("NYI"))
}

unsafe fn gen_binary_op(ctx: &mut Context, u: &BinaryOp) -> CompileResult<ValueRef>
{
    err(u.span.start, ErrorCode::UnexpectedEOF, format!("NYI"))
}


unsafe fn gen_function_sig(ctx: &mut Context, sig: &FunctionSignature, span: &Span) -> CompileResult<FunctionInstance>
{
    let ret_type = try!(ctx
        .resolve_type(&sig.return_type)
        .ok_or(CompileError::new(span.start, ErrorCode::TypeError, format!("Cannot resolve the return type of function '{}'", sig.name))));

    let mut arg_types = Vec::new();
    for arg in &sig.args {
        let arg_type = try!(ctx
            .resolve_type(&arg.typ)
            .ok_or(CompileError::new(arg.span.start, ErrorCode::TypeError, format!("Cannot resolve the type of argument '{}'", arg.name))));

        println!("arg {} {}", type_name(arg_type), arg.typ);
        arg_types.push(arg_type);
    }

    let name = sig.name.clone();

    let function_type = LLVMFunctionType(ret_type, arg_types.as_mut_ptr(), arg_types.len() as libc::c_uint, 0);
    let function = LLVMAddFunction(ctx.module, cstr(&name), function_type);

    Ok(FunctionInstance{
        name: name,
        args: arg_types,
        return_type: ret_type,
        function: function,
        sig: sig.clone(),
    })
}

unsafe fn is_block_terminated(bb: LLVMBasicBlockRef) -> bool
{
    if bb == ptr::null_mut() {
        return true;
    }
    let last = LLVMGetLastInstruction(bb);
    last != ptr::null_mut() && LLVMIsATerminatorInst(last) != ptr::null_mut()
}

unsafe fn gen_function(ctx: &mut Context, f: &Function) -> CompileResult<ValueRef>
{
    let fi = try!(gen_function_sig(ctx, &f.sig, &f.span));
    let bb = LLVMAppendBasicBlockInContext(ctx.context, fi.function, cstr("entry"));
    let current_bb = LLVMGetInsertBlock(ctx.builder);
    LLVMPositionBuilderAtEnd(ctx.builder, bb);

    ctx.push_stack();

    for (i, arg) in f.sig.args.iter().enumerate() {
        let var = LLVMGetParam(fi.function, i as libc::c_uint);
        let llvm_arg = fi.args[i];
        let alloc = LLVMBuildAlloca(ctx.builder, llvm_arg, cstr("argtmp"));
        LLVMBuildStore(ctx.builder, var, alloc);

        let var = Rc::new(VariableInstance{
            value: alloc,
            name: arg.name.clone(),
            typ: arg.typ.clone(),
        });

        ctx.add_variable(var);
    }

    ctx.add_function(Rc::new(fi));

    let ret = try!(gen_expression(ctx, &f.expression));
    LLVMBuildRet(ctx.builder, ret.get());
    ctx.pop_stack();

    if current_bb != ptr::null_mut() {
        LLVMPositionBuilderAtEnd(ctx.builder, current_bb);
    }

    Ok(ret)
}

pub fn gen_expression(ctx: &mut Context, e: &Expression) -> CompileResult<ValueRef>
{
    unsafe 
    {
        match *e
        {
            Expression::UnaryOp(ref u) => gen_unary_op(ctx, u),
            Expression::BinaryOp(ref op) => gen_binary_op(ctx, op),
            Expression::ArrayLiteral(ref a) => err(a.span.start, ErrorCode::UnexpectedEOF, format!("NYI")),
            Expression::ArrayInitializer(ref a) => err(a.span.start, ErrorCode::UnexpectedEOF, format!("NYI")),
            Expression::ArrayPattern(ref a) => err(a.span.start, ErrorCode::UnexpectedEOF, format!("NYI")), 
            Expression::Call(ref c) => err(c.span.start, ErrorCode::UnexpectedEOF, format!("NYI")),
            Expression::NameRef(ref nr) => err(nr.span.start, ErrorCode::UnexpectedEOF, format!("NYI")),
            Expression::Function(ref f) => gen_function(ctx, f),
            Expression::Match(ref m) => err(m.span.start, ErrorCode::UnexpectedEOF, format!("NYI")),
            Expression::Lambda(ref l) => err(l.span.start, ErrorCode::UnexpectedEOF, format!("NYI")),
            Expression::Enclosed(_, ref inner) => gen_expression(ctx, inner),
            Expression::IntLiteral(_, v) => gen_integer(ctx, v),
            Expression::FloatLiteral(ref span, ref v_str) => gen_float(ctx, &v_str, span),
            Expression::StringLiteral(ref span, ref s)  => gen_string_literal(ctx, s, span),
            Expression::BoolLiteral(_, v) => gen_integer(ctx, if v {1} else {0}),
        }
    }
}