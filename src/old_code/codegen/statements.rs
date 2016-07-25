use std::ptr;
use std::rc::Rc;
use llvm::*;
use llvm::prelude::*;
use llvm::core::*;
use libc;

use ast::{Import, Variable, Function, Type, FunctionSignature, ExternalFunction, Block, While, If,
    ElsePart, Return, Struct, Union, Match, Statement, Module, Trait};
use codegen::{type_name, cstr};
use compileerror::{CompileResult, Span, Pos, ErrorCode, err, type_error};
use codegen::context::{Context};
use codegen::expressions::{gen_expression_store, gen_expression};
use codegen::symbols::{FunctionInstance, PassingMode, StructMemberVar, StructType};
use codegen::conversions::{is_struct, is_array};
use codegen::valueref::{ValueRef};


fn gen_import(ctx: &mut Context, import: &Import) -> CompileResult<()>
{
    use codegen::modulecontext::import_module;
    for md in &import.modules {
        try!(import_module(ctx, md));
    }
    Ok(())
}

unsafe fn gen_variable(ctx: &mut Context, v: &Variable) -> CompileResult<()>
{
    if ctx.has_variable(&v.name) {
        return err(v.span.start, ErrorCode::RedefinitionOfVariable, format!("Variable {} already exists", v.name));
    }

    let v_typ = if v.typ == Type::Unknown {
        try!(ctx.infer_type(&v.init))
    } else {
        v.typ.clone()
    };

    let llvm_type = try!(ctx.resolve_type(&v_typ).ok_or(type_error(v.span.start, format!("Unknown type '{}'", v.typ))));
    if ctx.in_global_context()
    {
        let glob = ValueRef::global(ctx, llvm_type, LLVMLinkage::LLVMInternalLinkage, v.is_const);
        try!(gen_expression_store(ctx, &v.init, &glob));

        let name = ctx.prepend_namespace(&v.name);
        ctx.add_variable(name, glob.get(), v.is_const, v.public, true, v_typ);
    }
    else
    {
        let var = ValueRef::local(ctx.builder, llvm_type);
        try!(gen_expression_store(ctx, &v.init, &var));
        ctx.add_variable(v.name.clone(), var.get(), v.is_const, v.public, false, v_typ);
    }

    Ok(())
}

unsafe fn gen_function_sig(ctx: &mut Context, sig: &FunctionSignature, public: bool, external: bool, span: &Span) -> CompileResult<FunctionInstance>
{
    let ret_type = try!(ctx
        .resolve_type(&sig.return_type)
        .ok_or(type_error(span.start, format!("Cannot resolve the return type of function '{}'", sig.name))));

    let mut arg_types = Vec::new();
    let mut arg_types_and_modes = Vec::new();
    for arg in &sig.args {
        let mut arg_type = try!(ctx
            .resolve_type(&arg.typ)
            .ok_or(type_error(arg.span.start, format!("Cannot resolve the type of argument '{}'", arg.name))));

        let mut mode = PassingMode::Value;
        if is_struct(arg_type) || is_array(arg_type) {
            // Pass structs and arrays as pointer
            arg_type = LLVMPointerType(arg_type, 0);
            mode = PassingMode::Copy; // Before passing, copy them
        }

        println!("arg {} {}", type_name(arg_type), arg.typ);
        arg_types.push(arg_type);
        arg_types_and_modes.push((arg_type, mode));
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
        args: arg_types_and_modes,
        return_type: ret_type,
        function: function,
        sig: sig.clone(),
        public: public,
        external: external,
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

unsafe fn gen_function(ctx: &mut Context, f: &Function) -> CompileResult<FunctionInstance>
{
    if ctx.has_function(&f.sig.name) {
        return err(f.span.start, ErrorCode::RedefinitionOfFunction, format!("Function {} is already defined", f.sig.name));
    }

    let fi = try!(gen_function_sig(ctx, &f.sig, f.public, false, &f.span));
    let bb = LLVMAppendBasicBlockInContext(ctx.context, fi.function, cstr("entry"));
    let current_bb = LLVMGetInsertBlock(ctx.builder);
    LLVMPositionBuilderAtEnd(ctx.builder, bb);

    ctx.push_stack_frame(fi.function);

    for (i, arg) in f.sig.args.iter().enumerate() {
        let var = LLVMGetParam(fi.function, i as libc::c_uint);
        let (llvm_arg, _) = fi.args[i];
        let alloc = LLVMBuildAlloca(ctx.builder, llvm_arg, cstr("argtmp"));
        LLVMBuildStore(ctx.builder, var, alloc);
        ctx.add_variable(arg.name.clone(), alloc, arg.constant, false, false, arg.typ.clone());
    }

    for s in &f.block.statements {
        try!(gen_statement(ctx, s));
    }

    if f.sig.return_type == Type::Void {
        LLVMBuildRetVoid(ctx.builder);
    } else if !is_block_terminated(LLVMGetInsertBlock(ctx.builder)) {
        return err(f.span.end, ErrorCode::MissingReturn, format!("Missing return statement at end of function {}", f.sig.name));
    }

    ctx.pop_stack_frame();

    if current_bb != ptr::null_mut() {
        LLVMPositionBuilderAtEnd(ctx.builder, current_bb);
    }

    Ok(fi)
}

unsafe fn gen_external_function(ctx: &mut Context, f: &ExternalFunction) -> CompileResult<()>
{
    if ctx.has_function(&f.sig.name) {
        return err(f.span.start, ErrorCode::RedefinitionOfFunction, format!("External function {} is already defined", f.sig.name));
    }

    let fi = try!(gen_function_sig(ctx, &f.sig, true, true, &f.span));
    ctx.add_function(fi);
    Ok(())
}

unsafe fn gen_block(ctx: &mut Context, b: &Block) -> CompileResult<()>
{
    for s in &b.statements {
        try!(gen_statement(ctx, s));
    }
    Ok(())
}

unsafe fn gen_while(ctx: &mut Context, f: &While) -> CompileResult<()>
{
    let func = ctx.get_current_function();
    let loop_cond_bb = LLVMAppendBasicBlockInContext(ctx.context, func, cstr("loop_cond"));
    let loop_body_bb = LLVMAppendBasicBlockInContext(ctx.context, func, cstr("loop_body"));
    let post_loop_bb = LLVMAppendBasicBlockInContext(ctx.context, func, cstr("loop_done"));

    LLVMBuildBr(ctx.builder, loop_cond_bb);
    LLVMPositionBuilderAtEnd(ctx.builder, loop_cond_bb);
    let cond = try!(gen_expression(ctx, &f.cond));
    LLVMBuildCondBr(ctx.builder, cond.load(), loop_body_bb, post_loop_bb);
    LLVMPositionBuilderAtEnd(ctx.builder, loop_body_bb);

    try!(gen_block(ctx, &f.block));

    LLVMBuildBr(ctx.builder, loop_cond_bb);
    LLVMPositionBuilderAtEnd(ctx.builder, post_loop_bb);
    Ok(())
}

unsafe fn gen_if(ctx: &mut Context, f: &If) -> CompileResult<()>
{
    let func = ctx.get_current_function();
    let if_bb = LLVMAppendBasicBlockInContext(ctx.context, func, cstr("if_bb"));
    let mut after_if_bb = ptr::null_mut();
    let else_bb = LLVMAppendBasicBlockInContext(ctx.context, func, cstr("else_bb"));
    let cond = try!(gen_expression(ctx, &f.cond));

    LLVMBuildCondBr(ctx.builder, cond.load(), if_bb, else_bb);
    LLVMPositionBuilderAtEnd(ctx.builder, if_bb);

    try!(gen_block(ctx, &f.if_block));
    if !is_block_terminated(LLVMGetInsertBlock(ctx.builder)) {
        if after_if_bb == ptr::null_mut() {
            after_if_bb = LLVMAppendBasicBlockInContext(ctx.context, func, cstr("after_if_bb"));
        }
        LLVMBuildBr(ctx.builder, after_if_bb);
    }

    match f.else_part {
        ElsePart::Block(ref else_block) => {
            LLVMPositionBuilderAtEnd(ctx.builder, else_bb);
            try!(gen_block(ctx, else_block));
            if !is_block_terminated(LLVMGetInsertBlock(ctx.builder)) {
                if after_if_bb == ptr::null_mut() {
                    after_if_bb = LLVMAppendBasicBlockInContext(ctx.context, func, cstr("after_if_bb"));
                }
                LLVMBuildBr(ctx.builder, after_if_bb);
            }
        },
        ElsePart::Empty => {
            LLVMPositionBuilderAtEnd(ctx.builder, else_bb);
            if after_if_bb == ptr::null_mut() {
                after_if_bb = LLVMAppendBasicBlockInContext(ctx.context, func, cstr("after_if_bb"));
            }
            LLVMBuildBr(ctx.builder, after_if_bb);
        },
        ElsePart::If(ref next_if) => {
            LLVMPositionBuilderAtEnd(ctx.builder, else_bb);
            try!(gen_if(ctx, next_if));
            if !is_block_terminated(LLVMGetInsertBlock(ctx.builder)) {
                if after_if_bb == ptr::null_mut() {
                    after_if_bb = LLVMAppendBasicBlockInContext(ctx.context, func, cstr("after_if_bb"));
                }
                LLVMBuildBr(ctx.builder, after_if_bb);
            }
        }
    }

    LLVMPositionBuilderAtEnd(ctx.builder, after_if_bb);
    Ok(())
}

unsafe fn return_type(func: LLVMValueRef) -> LLVMTypeRef
{
    LLVMGetReturnType(LLVMGetElementType(LLVMTypeOf(func)))
}

unsafe fn gen_return(ctx: &mut Context, f: &Return) -> CompileResult<()>
{
    let ret = try!(gen_expression(ctx, &f.expr)).load();
    let builder = ctx.builder;
    let ret_type =  LLVMTypeOf(ret);
    let func_type = return_type(ctx.get_current_function());
    if ret_type != func_type {
        err(f.span.start, ErrorCode::TypeError,
            format!("Attempting to return type '{}' expecting '{}'", type_name(ret_type), type_name(func_type)))
    } else {
        LLVMBuildRet(builder, ret);
        Ok(())
    }
}

fn check_trait_impls(ctx: &mut Context, s: &Struct) -> CompileResult<()>
{
    for t in &s.impls
    {
        match *t
        {
            Type::Complex(ref name) => {
                if let Some(tr) = ctx.get_trait(&name) {
                    if !tr.is_implemented_by(s) {
                        return err(s.span.start, ErrorCode::TraitNotImplemented, format!("Struct {} does not implement {} properly", s.name, name));
                    }
                } else {
                    return err(s.span.start, ErrorCode::UnknownType, format!("Unknown trait {}", name));
                }
            }
            _ => {
                return Err(type_error(s.span.start, format!("Invalid impl {}", t)));
            },
        }
    }

    Ok(())
}

unsafe fn gen_struct(ctx: &mut Context, s: &Struct) -> CompileResult<()>
{
    if let Some(_) = ctx.get_complex_type(&s.name) {
        return err(s.span.start, ErrorCode::RedefinitionOfStruct, format!("Struct {} is already defined", s.name));
    }

    try!(check_trait_impls(ctx, s));

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
            return err(v.span.start, ErrorCode::TypeError,
                format!("Unable to determine type of member '{}' of struct '{}'", v.name, s.name));
        }
    }

    let struct_type = Rc::new(StructType{
        name: if ctx.in_global_context() {ctx.prepend_namespace(&s.name)} else {s.name.clone()},
        typ: LLVMStructTypeInContext(ctx.context, element_types.as_mut_ptr(), s.variables.len() as u32, 0),
        members: members,
        public: s.public,
    });

    ctx.add_complex_type(struct_type);

    for f in &s.functions {
        let func = try!(gen_function(ctx, f));
        ctx.add_function(func)
    }

    Ok(())
}

#[allow(unused_variables)]
fn gen_union(ctx: &mut Context, f: &Union) -> CompileResult<()>
{
     err(Pos::new(0, 0), ErrorCode::UnexpectedEOF, format!("NYI"))
}

#[allow(unused_variables)]
fn gen_match(ctx: &mut Context, f: &Match) -> CompileResult<()>
{
     err(Pos::new(0, 0), ErrorCode::UnexpectedEOF, format!("NYI"))
}

fn gen_trait(ctx: &mut Context, t: &Trait) -> CompileResult<()>
{
    if let Some(_) = ctx.get_trait(&t.name) {
        return err(t.span.start, ErrorCode::RedefinitionOfTrait, format!("Trait {} already exists", t.name));
    }
    let mut new_trait = t.clone();
    if ctx.in_global_context() {
        new_trait.name = ctx.prepend_namespace(&new_trait.name);
    }
    ctx.add_trait(Rc::new(new_trait));
    Ok(())
}

unsafe fn gen_statement(ctx: &mut Context, stmt: &Statement) -> CompileResult<()>
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
        Statement::Trait(ref t) => gen_trait(ctx, t),
    }
}

pub unsafe fn gen_module(ctx: &mut Context, module: &Module) -> CompileResult<()>
{
    try!(gen_block(ctx, &module.block));
    Ok(())
}