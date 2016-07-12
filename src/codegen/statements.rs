use std::ptr;
use std::rc::Rc;
use llvm::*;
use llvm::prelude::*;
use llvm::core::*;
use libc;

use ast::*;
use codegen::*;
use compileerror::*;
use codegen::context::*;
use codegen::expressions::*;
use codegen::symbols::*;


fn gen_import(ctx: &mut Context, import: &Import) -> Result<(), CompileError>
{
    use codegen::modulecontext::import_module;
    for md in &import.modules {
        try!(import_module(ctx, md));
    }
    Ok(())
}

unsafe fn gen_variable(ctx: &mut Context, v: &Variable) -> Result<(), CompileError>
{
    if ctx.has_variable(&v.name) {
        return err(v.span.start, ErrorType::RedefinitionOfVariable(v.name.clone()));
    }

    let initial_value = try!(gen_expression(ctx, &v.init));
    let initial_value_type = LLVMTypeOf(initial_value);

    let v_typ = if v.typ == Type::Unknown {
        try!(ctx.infer_type(&v.init))
    } else {
        v.typ.clone()
    };

    if let Some(llvm_type_ref) = ctx.resolve_type(&v_typ) {
        if llvm_type_ref != initial_value_type {
            return err(v.span.start, ErrorType::TypeError(format!("Mismatched types in initialization ({} vs {})",
                type_name(llvm_type_ref), type_name(initial_value_type))));
        }
    } else {
        return err(v.span.start, ErrorType::TypeError(format!("Unknown type '{}'", v.typ)));
    }

    if ctx.in_global_context() {
        let glob = LLVMAddGlobal(ctx.get_module_ref(), initial_value_type, cstr("glob_var"));
        LLVMSetLinkage(glob, LLVMLinkage::LLVMInternalLinkage);
        LLVMSetGlobalConstant(glob, if v.is_const {1} else {0});
        LLVMSetInitializer(glob, initial_value);

        let name = ctx.prepend_namespace(&v.name);
        ctx.add_variable(&name, glob, v.is_const, v.public, v_typ);
    } else {
        let var = LLVMBuildAlloca(ctx.builder, initial_value_type, cstr("local_var"));
        LLVMBuildStore(ctx.builder, initial_value, var);
        ctx.add_variable(&v.name, var, v.is_const, v.public, v_typ);
    }

    Ok(())
}

unsafe fn gen_function_sig(ctx: &mut Context, sig: &FunctionSignature, public: bool, external: bool, span: &Span) -> Result<FunctionInstance, CompileError>
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

    let name = if ctx.in_global_context() && !external {
        // global functions have to be namespaced
        ctx.prepend_namespace(&sig.name)
    } else {
        sig.name.clone()
    };

    let function_type = LLVMFunctionType(ret_type, arg_types.as_mut_ptr(), arg_types.len() as libc::c_uint, 0);
    let function = LLVMAddFunction(ctx.get_module_ref(), cstr(&name), function_type);

    Ok(FunctionInstance{
        name: name,
        args: arg_types,
        return_type: ret_type,
        function: function,
        sig: sig.clone(),
        public: public,
        external: external,
    })
}

unsafe fn gen_function(ctx: &mut Context, f: &Function) -> Result<FunctionInstance, CompileError>
{
    if ctx.has_function(&f.sig.name) {
        return err(f.span.start, ErrorType::RedefinitionOfFunction(f.sig.name.clone()));
    }

    let fi = try!(gen_function_sig(ctx, &f.sig, f.public, false, &f.span));
    let bb = LLVMAppendBasicBlockInContext(ctx.context, fi.function, cstr("entry"));
    let current_bb = LLVMGetInsertBlock(ctx.builder);
    LLVMPositionBuilderAtEnd(ctx.builder, bb);

    ctx.push_stack_frame(fi.function);

    for (i, arg) in f.sig.args.iter().enumerate() {
        let var = LLVMGetParam(fi.function, i as libc::c_uint);
        let alloc = LLVMBuildAlloca(ctx.builder, fi.args[i], cstr("argtmp"));
        LLVMBuildStore(ctx.builder, var, alloc);
        ctx.add_variable(&arg.name, alloc, arg.constant, false, arg.typ.clone());
    }

    for s in &f.block.statements {
        try!(gen_statement(ctx, s));
    }

    if f.sig.return_type == Type::Void {
        LLVMBuildRetVoid(ctx.builder);
    } else if LLVMIsATerminatorInst(LLVMGetLastInstruction(LLVMGetInsertBlock(ctx.builder))) == ptr::null_mut() {
        return err(f.span.end, ErrorType::MissingReturn(f.sig.name.clone()));
    }

    ctx.pop_stack_frame();

    if current_bb != ptr::null_mut() {
        LLVMPositionBuilderAtEnd(ctx.builder, current_bb);
    }

    Ok(fi)
}

unsafe fn gen_external_function(ctx: &mut Context, f: &ExternalFunction) -> Result<(), CompileError>
{
    if ctx.has_function(&f.sig.name) {
        return err(f.span.start, ErrorType::RedefinitionOfFunction(f.sig.name.clone()));
    }

    let fi = try!(gen_function_sig(ctx, &f.sig, true, true, &f.span));
    ctx.add_function(fi);
    Ok(())
}

unsafe fn gen_block(ctx: &mut Context, b: &Block) -> Result<(), CompileError>
{
    for s in &b.statements {
        try!(gen_statement(ctx, s));
    }
    Ok(())
}

unsafe fn gen_while(ctx: &mut Context, f: &While) -> Result<(), CompileError>
{
    let func = ctx.get_current_function();
    let loop_cond_bb = LLVMAppendBasicBlockInContext(ctx.context, func, cstr("loop_cond"));
    let loop_body_bb = LLVMAppendBasicBlockInContext(ctx.context, func, cstr("loop_body"));
    let post_loop_bb = LLVMAppendBasicBlockInContext(ctx.context, func, cstr("loop_done"));

    LLVMBuildBr(ctx.builder, loop_cond_bb);
    LLVMPositionBuilderAtEnd(ctx.builder, loop_cond_bb);
    let cond = try!(gen_expression(ctx, &f.cond));
    LLVMBuildCondBr(ctx.builder, cond, loop_body_bb, post_loop_bb);
    LLVMPositionBuilderAtEnd(ctx.builder, loop_body_bb);

    try!(gen_block(ctx, &f.block));

    LLVMBuildBr(ctx.builder, loop_cond_bb);
    LLVMPositionBuilderAtEnd(ctx.builder, post_loop_bb);
    Ok(())
}

unsafe fn gen_if(ctx: &mut Context, f: &If) -> Result<(), CompileError>
{
    let func = ctx.get_current_function();
    let if_bb = LLVMAppendBasicBlockInContext(ctx.context, func, cstr("if_bb"));
    let after_if_bb = LLVMAppendBasicBlockInContext(ctx.context, func, cstr("after_if_bb"));
    let else_bb = LLVMAppendBasicBlockInContext(ctx.context, func, cstr("else_bb"));
    let cond = try!(gen_expression(ctx, &f.cond));

    LLVMBuildCondBr(ctx.builder, cond, if_bb, else_bb);
    LLVMPositionBuilderAtEnd(ctx.builder, if_bb);

    try!(gen_block(ctx, &f.if_block));
    LLVMBuildBr(ctx.builder, after_if_bb);

    match f.else_part {
        ElsePart::Block(ref else_block) => {
            LLVMPositionBuilderAtEnd(ctx.builder, else_bb);
            try!(gen_block(ctx, else_block));
            LLVMBuildBr(ctx.builder, after_if_bb);
        },
        ElsePart::Empty => {
            LLVMPositionBuilderAtEnd(ctx.builder, else_bb);
            LLVMBuildBr(ctx.builder, after_if_bb);
        },
        ElsePart::If(ref next_if) => {
            LLVMPositionBuilderAtEnd(ctx.builder, else_bb);
            try!(gen_if(ctx, next_if));
            LLVMBuildBr(ctx.builder, after_if_bb);
        }
    }

    LLVMPositionBuilderAtEnd(ctx.builder, after_if_bb);
    Ok(())
}

unsafe fn return_type(func: LLVMValueRef) -> LLVMTypeRef
{
    LLVMGetReturnType(LLVMGetElementType(LLVMTypeOf(func)))
}

unsafe fn gen_return(ctx: &mut Context, f: &Return) -> Result<(), CompileError>
{
    let ret = try!(gen_expression(ctx, &f.expr));
    let builder = ctx.builder;
    let ret_type =  LLVMTypeOf(ret);
    let func_type = return_type(ctx.get_current_function());
    if ret_type != func_type {
        err(f.span.start, ErrorType::TypeError(
            format!("Attempting to return type '{}' expecting '{}'", type_name(ret_type), type_name(func_type))))
    } else {
        LLVMBuildRet(builder, ret);
        Ok(())
    }
}

unsafe fn gen_struct(ctx: &mut Context, s: &Struct) -> Result<(), CompileError>
{
    if let Some(_) = ctx.get_complex_type(&s.name) {
        return err(s.span.start, ErrorType::RedefinitionOfStruct(s.name.clone()));
    }

    let mut members = Vec::with_capacity(s.variables.len());
    let mut element_types = Vec::with_capacity(s.variables.len());
    for v in &s.variables
    {
        let typ = if v.typ == Type::Unknown {
            try!(ctx.infer_type(&v.init))
        } else {
            v.typ.clone()
        };

         if let Some(llvm_typ) = ctx.resolve_type(&typ) {
            members.push(Rc::new(StructMemberVar{
                name: v.name.clone(),
                typ: typ,
                llvm_typ: llvm_typ,
                constant: v.is_const,
                public: v.public,
                init: v.init.clone(),
            }));
            element_types.push(llvm_typ);
         } else {
            return err(v.span.start, ErrorType::TypeError(
                format!("Unable to determine type of member '{}' of struct '{}'", v.name, s.name)));
        }
    }

    let struct_type = StructType{
        name: if ctx.in_global_context() {ctx.prepend_namespace(&s.name)} else {s.name.clone()},
        typ: LLVMStructTypeInContext(ctx.context, element_types.as_mut_ptr(), s.variables.len() as u32, 0),
        members: members,
        public: s.public,
    };

    ctx.add_complex_type(struct_type);

    for f in &s.functions {
        let func = try!(gen_function(ctx, f));
        ctx.add_function(func)
    }

    Ok(())
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
        Statement::Function(ref fun) => {
            let function_instance = try!(gen_function(ctx, fun));
            ctx.add_function(function_instance);
            Ok(())
        },
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

pub unsafe fn gen_module(ctx: &mut Context, module: &Module) -> Result<(), CompileError>
{
    try!(gen_block(ctx, &module.block));
    Ok(())
}
