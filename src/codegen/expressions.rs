use std::ptr;
use std::rc::Rc;
use std::os::raw::c_uint;

use libc;
use llvm::core::*;
use llvm::prelude::*;
use llvm::*;

use ast::*;
use parser::Operator;
use codegen::{cstr, Context, ValueRef, Array};
use codegen::symboltable::{FunctionInstance};
use compileerror::{CompileResult, CompileError, ErrorCode, err, unknown_name};
use span::{Span};


unsafe fn is_floating_point(ctx: LLVMContextRef, tr: LLVMTypeRef) -> bool
{
    tr == LLVMDoubleTypeInContext(ctx)
}

pub unsafe fn const_int(ctx: &Context, v: u64) -> LLVMValueRef
{
    LLVMConstInt(LLVMInt64TypeInContext(ctx.context), v, 0)
}

unsafe fn gen_integer(ctx: &Context, v: u64) -> CompileResult<ValueRef>
{
    Ok(ValueRef::Const(const_int(ctx, v)))
}

unsafe fn gen_bool(ctx: &Context, v: bool) -> CompileResult<ValueRef>
{
    Ok(ValueRef::Const(LLVMConstInt(LLVMInt1TypeInContext(ctx.context), if v {1} else {0}, 0)))
}

unsafe fn gen_float(ctx: &Context, num: &str, span: &Span) -> CompileResult<ValueRef>
{
    match num.parse::<f64>()
    {
        Ok(f) => Ok(ValueRef::Const(LLVMConstReal(LLVMDoubleTypeInContext(ctx.context), f))),
        Err(_) => err(span, ErrorCode::InvalidFloatingPoint, format!("{} is not a valid floating point number", num))
    }
}

unsafe fn gen_const_string_literal(ctx: &Context, s: &str) -> CompileResult<ValueRef>
{
    Ok(ValueRef::Const(LLVMConstStringInContext(ctx.context, s.as_ptr() as *const i8, s.len() as u32, 1)))
}

unsafe fn gen_string_literal(ctx: &Context, s: &str, span: &Span) -> CompileResult<ValueRef>
{
    let glob = LLVMAddGlobal(ctx.module, LLVMArrayType(LLVMInt8TypeInContext(ctx.context), s.len() as c_uint),  cstr("string"));

    LLVMSetLinkage(glob, LLVMLinkage::LLVMInternalLinkage);
    LLVMSetGlobalConstant(glob, 1);
    let string_data = try!(gen_const_string_literal(ctx, s));
    LLVMSetInitializer(glob, string_data.get());

    let typ = ctx.resolve_type(&string_type());
    let array = Array::new(ctx.alloc(typ, "string"), Type::Char);

    let length_ptr = array.get_length_ptr(ctx);
    try!(length_ptr.store_direct(ctx, const_int(ctx, s.len() as u64), &span));

    let offset_ptr = array.get_offset_ptr(ctx);
    try!(offset_ptr.store_direct(ctx, const_int(ctx, 0), &span));

    let data_ptr = array.get_data_ptr(ctx);

    let mut index_expr = vec![const_int(ctx, 0), const_int(ctx, 0)];
    let first_element = LLVMBuildGEP(ctx.builder, glob, index_expr.as_mut_ptr(), 2, cstr("first_element"));
    try!(data_ptr.store_direct(ctx, first_element, &span));

    Ok(ValueRef::Array(array))
}

unsafe fn gen_unary_op(ctx: &mut Context, op: &UnaryOp) -> CompileResult<ValueRef>
{
    let e_val = try!(gen_expression(ctx, &op.expression));
    match op.operator
    {
        Operator::Sub => {
            Ok(ValueRef::Const(LLVMBuildNeg(ctx.builder, e_val.load(ctx.builder), cstr("neg"))))
        },
        Operator::Not => {
            Ok(ValueRef::Const(LLVMBuildNot(ctx.builder, e_val.load(ctx.builder), cstr("not"))))
        },
        _ => err(&op.span, ErrorCode::InvalidUnaryOperator, format!("Operator {} is not a unary operator", op.operator)),
    }
}



unsafe fn gen_binary_op(ctx: &mut Context, op: &BinaryOp) -> CompileResult<ValueRef>
{
    let left_val = try!(gen_expression(ctx, &op.left));
    let right_val = try!(gen_expression(ctx, &op.right));

    match (op.left.get_type(), op.right.get_type())
    {
        (Type::Int, Type::Int) => {
            let l = left_val.load(ctx.builder);
            let r = right_val.load(ctx.builder);

            Ok(ValueRef::Const(match op.operator
            {
                Operator::Add => LLVMBuildAdd(ctx.builder, l, r, cstr("add")),
                Operator::Sub => LLVMBuildSub(ctx.builder, l, r, cstr("sub")),
                Operator::Mul => LLVMBuildMul(ctx.builder, l, r, cstr("mul")),
                Operator::Div => LLVMBuildUDiv(ctx.builder, l, r, cstr("div")),
                Operator::Mod => LLVMBuildURem(ctx.builder, l, r, cstr("mod")),
                Operator::LessThan => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntSLT, l, r, cstr("cmp")),
                Operator::LessThanEquals => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntSLE, l, r, cstr("cmp")),
                Operator::GreaterThan => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntSGT, l, r, cstr("cmp")),
                Operator::GreaterThanEquals => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntSGE, l, r, cstr("cmp")),
                Operator::Equals => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntEQ, l, r, cstr("cmp")),
                Operator::NotEquals => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntNE, l, r, cstr("cmp")),
                _ => return err(&op.span, ErrorCode::InvalidBinaryOperator, format!("Operator {} is not supported on integers", op.operator)),
            }))
        }

        (Type::Float, Type::Float) => {
            let l = left_val.load(ctx.builder);
            let r = right_val.load(ctx.builder);

            Ok(ValueRef::Const(match op.operator
            {
                Operator::Add => LLVMBuildFAdd(ctx.builder, l, r, cstr("add")),
                Operator::Sub => LLVMBuildFSub(ctx.builder, l, r, cstr("sub")),
                Operator::Mul => LLVMBuildFMul(ctx.builder, l, r, cstr("mul")),
                Operator::Div => LLVMBuildFDiv(ctx.builder, l, r, cstr("div")),
                Operator::Mod => LLVMBuildFRem(ctx.builder, l, r, cstr("mod")),
                Operator::LessThan => LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealOLT, l, r, cstr("cmp")),
                Operator::LessThanEquals => LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealOLE, l, r, cstr("cmp")),
                Operator::GreaterThan => LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealOGT, l, r, cstr("cmp")),
                Operator::GreaterThanEquals => LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealOGE, l, r, cstr("cmp")),
                Operator::Equals => LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealOEQ, l, r, cstr("cmp")),
                Operator::NotEquals => LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealONE, l, r, cstr("cmp")),
                _ => return err(&op.span, ErrorCode::InvalidBinaryOperator, format!("Operator {} is not supported on floats", op.operator)),
            }))
        },

        (Type::Bool, Type::Bool) => {
            let l = left_val.load(ctx.builder);
            let r = right_val.load(ctx.builder);

            Ok(ValueRef::Const(match op.operator
            {
                Operator::And => LLVMBuildAnd(ctx.builder, l, r, cstr("and")),
                Operator::Or => LLVMBuildOr(ctx.builder, left_val.load(ctx.builder), right_val.load(ctx.builder), cstr("or")),
                _ => return err(&op.span, ErrorCode::InvalidBinaryOperator, format!("Operator {} is not supported on bools", op.operator)),
            }))
        },

        (Type::Array(_), Type::Array(_)) => {

            if let (ValueRef::Array(ref ar), ValueRef::Array(ref br)) = (left_val, right_val)
            {
                match op.operator
                {
                    Operator::Add => {
                        /*let a_length = ar.get_length_ptr(ctx).load(ctx.builder);
                        let b_length = br.get_length_ptr(ctx).load(ctx.builder);
                        let new_size = LLVMBuildAdd(ctx.builder, a_length, b_length, cstr("new_size"));

                        let array = Array::new()
                        */
                        panic!("NYI");
                    },

                    Operator::Equals | Operator::NotEquals => {
                        let dst = ctx.alloc(ctx.resolve_type(&Type::Bool), "dst");
                        let func = ctx.get_current_function();
                        let on_eq = LLVMAppendBasicBlockInContext(ctx.context, func, cstr("on_eq"));
                        let on_not_eq = LLVMAppendBasicBlockInContext(ctx.context, func, cstr("on_not_eq"));
                        let after_eq = LLVMAppendBasicBlockInContext(ctx.context, func, cstr("after_eq"));
                        try!(gen_equals_seq(ctx, ar, br, on_eq, on_not_eq));

                        LLVMPositionBuilderAtEnd(ctx.builder, on_eq);
                        LLVMBuildStore(ctx.builder, const_int(ctx, 1), dst);
                        LLVMBuildBr(ctx.builder, after_eq);

                        LLVMPositionBuilderAtEnd(ctx.builder, on_not_eq);
                        LLVMBuildStore(ctx.builder, const_int(ctx, 0), dst);
                        LLVMBuildBr(ctx.builder, after_eq);

                        LLVMPositionBuilderAtEnd(ctx.builder, after_eq);
                        Ok(ValueRef::Const(
                            if op.operator == Operator::Equals {dst} else {LLVMBuildNot(ctx.builder, dst, cstr("not"))}
                        ))
                    }

                    _ => err(&op.span, ErrorCode::InvalidBinaryOperator, format!("Operator {} is not supported on arrays", op.operator)),
                }
            }
            else
            {
                panic!("Internal Compiler Error: Expecting array ValueRef's here");
            }

        }

        _ => err(&op.span, ErrorCode::InvalidBinaryOperator, format!("Operator {} is not a binary operator", op.operator)),
    }

}

unsafe fn make_function_instance(ctx: &Context, sig: &FunctionSignature) -> CompileResult<FunctionInstance>
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

    Ok(FunctionInstance{
        name: sig.name.clone(),
        args: arg_types_with_passing_mode,
        return_type: ret_type,
        function: ptr::null_mut(),
        sig: sig.clone(),
    })
}


pub unsafe fn gen_function_sig(ctx: &Context, sig: &FunctionSignature) -> CompileResult<FunctionInstance>
{
    let mut fi = try!(make_function_instance(ctx, sig));
    let mut arg_types: Vec<LLVMTypeRef> = fi.args.iter().map(|&(typ, _)| typ).collect();
    let function_type = LLVMFunctionType(fi.return_type, arg_types.as_mut_ptr(), arg_types.len() as libc::c_uint, 0);
    fi.function = LLVMAddFunction(ctx.module, cstr(&sig.name), function_type);
    Ok(fi)
}

pub unsafe fn gen_function_ptr(ctx: &Context, func_ptr: LLVMValueRef, sig: FunctionSignature) -> CompileResult<FunctionInstance>
{
    let mut fi = try!(make_function_instance(ctx, &sig));
    fi.function = func_ptr;
    Ok(fi)
}

pub unsafe fn gen_function(ctx: &mut Context, signature: &FunctionSignature, body: &Expression) -> CompileResult<ValueRef>
{
    let fi = try!(ctx.get_function(&signature.name).ok_or(
        CompileError::new(&signature.span, ErrorCode::UnknownName, format!("Unknown function {}", signature.name))));
    let bb = LLVMAppendBasicBlockInContext(ctx.context, fi.function, cstr("entry"));
    let current_bb = LLVMGetInsertBlock(ctx.builder);
    LLVMPositionBuilderAtEnd(ctx.builder, bb);

    ctx.push_stack(fi.function);

    for (i, arg) in signature.args.iter().enumerate() {
        let var = LLVMGetParam(fi.function, i as libc::c_uint);
        let (_, mode) = fi.args[i];
        match mode
        {
            ArgumentPassingMode::ByPtr => {
                match arg.typ
                {
                    Type::Func(ref ft) => {
                        let func_sig = anon_sig(&arg.name, &ft.return_type, &ft.args);
                        let fi = try!(gen_function_ptr(ctx, var, func_sig));
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

    let ret = if signature.return_type.return_by_ptr()
    {
        let ret_arg = LLVMGetParam(fi.function, signature.args.len() as libc::c_uint);
        let mut vr = ValueRef::new(ret_arg, &signature.return_type);
        try!(gen_expression_store(ctx, body, &mut vr));
        LLVMBuildRetVoid(ctx.builder);
        vr
    }
    else
    {
        let ret = try!(gen_expression(ctx, body));
        LLVMBuildRet(ctx.builder, ret.load(ctx.builder));
        ret
    };

    ctx.pop_stack();

    if current_bb != ptr::null_mut() {
        LLVMPositionBuilderAtEnd(ctx.builder, current_bb);
    }

    Ok(ret)
}

unsafe fn gen_lambda_store(ctx: &mut Context, l: &Lambda, ptr: &mut ValueRef) -> CompileResult<()>
{
    let fi = try!(gen_function_sig(ctx, &l.sig));
    let ret = fi.function;
    ctx.add_function(Rc::new(fi));
    try!(gen_function(ctx, &l.sig, &l.expr));
    *ptr = ValueRef::Ptr(ret);
    Ok(())
}

unsafe fn gen_lambda(ctx: &mut Context, l: &Lambda) -> CompileResult<ValueRef>
{
    let fi = try!(gen_function_sig(ctx, &l.sig));
    let ret = fi.function;
    ctx.add_function(Rc::new(fi));
    try!(gen_function(ctx, &l.sig, &l.expr));
    Ok(ValueRef::Ptr(ret))
}

unsafe fn gen_call_args(ctx: &mut Context, c: &Call, func: &FunctionInstance) -> CompileResult<Vec<LLVMValueRef>>
{
    let mut arg_vals = Vec::with_capacity(func.args.len());
    for (ref arg, passing_mode) in c.args.iter().zip(func.args.iter().map(|&(_, passing_mode)| passing_mode.clone()))
    {
        let a = try!(gen_expression(ctx, &arg));
        match passing_mode
        {
            ArgumentPassingMode::ByValue => arg_vals.push(a.load(ctx.builder)),
            ArgumentPassingMode::ByPtr => arg_vals.push(a.get()),
        }
    }
    Ok(arg_vals)
}

unsafe fn gen_call_store(ctx: &mut Context, c: &Call, ptr: &ValueRef) -> CompileResult<()>
{
    let func = try!(
        ctx.get_function(&c.callee.name).ok_or(
            CompileError::new(&c.span, ErrorCode::UnknownName, format!("Unknown function {}", c.callee.name)))
    );

    let mut arg_vals = try!(gen_call_args(ctx, c, &func));
    if func.sig.return_type.return_by_ptr()
    {
        arg_vals.push(ptr.get());
        LLVMBuildCall(ctx.builder, func.function, arg_vals.as_mut_ptr(), arg_vals.len() as libc::c_uint, cstr(""));
        Ok(())
    }
    else
    {
        let call_ret = LLVMBuildCall(ctx.builder, func.function, arg_vals.as_mut_ptr(), arg_vals.len() as libc::c_uint, cstr(""));
        ptr.store(ctx, ValueRef::Const(call_ret), &c.span)
    }
}

unsafe fn gen_call(ctx: &mut Context, c: &Call) -> CompileResult<ValueRef>
{
    let func = try!(
        ctx.get_function(&c.callee.name).ok_or(
            CompileError::new(&c.span, ErrorCode::UnknownName, format!("Unknown function {}", c.callee.name)))
    );

    if func.sig.return_type.return_by_ptr()
    {
        let vr = ValueRef::alloc(ctx, &func.sig.return_type);
        try!(gen_call_store(ctx, c, &vr));
        Ok(vr)
    }
    else
    {
        let mut arg_vals = try!(gen_call_args(ctx, c, &func));
        Ok(ValueRef::Const(LLVMBuildCall(ctx.builder, func.function, arg_vals.as_mut_ptr(), arg_vals.len() as libc::c_uint, cstr(""))))
    }
}

unsafe fn gen_name_ref_store(ctx: &Context, nr: &NameRef, ptr: &mut ValueRef) -> CompileResult<()>
{
    let v = try!(gen_name_ref(ctx, nr));
    try!(ptr.store(ctx, v, &nr.span));
    Ok(())
}

unsafe fn gen_name_ref(ctx: &Context, nr: &NameRef) -> CompileResult<ValueRef>
{
    if let Some(vi) = ctx.get_variable(&nr.name) {
        Ok(vi.value.clone())
    } else if let Some(fi) = ctx.get_function(&nr.name) {
        Ok(ValueRef::Const(fi.function))
    } else {
        match nr.typ
        {
            Type::Sum(ref st) => {
                let sv = ValueRef::alloc(ctx, &nr.typ);
                let case_type_ptr = try!(sv.case_type(ctx, &nr.span));
                let idx = st.index_of(&nr.name).expect("Internal Compiler Error: cannot determine index of sum type case");
                try!(case_type_ptr.store_direct(ctx, const_int(ctx, idx as u64), &nr.span));
                Ok(sv)
            },
            Type::Enum(ref et) => {
                let idx = et.index_of(&nr.name).expect("Internal Compiler Error: cannot determine index of sum type case");
                Ok(ValueRef::Const(const_int(ctx, idx as u64)))
            },
            _ => {
                err(&nr.span, ErrorCode::UnknownName, format!("Unknown name {}", nr.name))
            }
        }
    }
}

unsafe fn gen_match_case_to_execute(
    ctx: &mut Context,
    mc: &MatchCase,
    dst: &mut ValueRef,
    match_case_bb: LLVMBasicBlockRef,
    match_end_bb: LLVMBasicBlockRef,
    next_bb: LLVMBasicBlockRef) -> CompileResult<()>
{
    LLVMPositionBuilderAtEnd(ctx.builder, match_case_bb);
    try!(gen_expression_store(ctx, &mc.to_execute, dst));
    LLVMBuildBr(ctx.builder, match_end_bb);
    LLVMPositionBuilderAtEnd(ctx.builder, next_bb);
    Ok(())
}

unsafe fn gen_sequence_match(
    ctx: &mut Context,
    ap: &ArrayPattern,
    seq: &Array,
    span: &Span,
    match_case_bb: LLVMBasicBlockRef,
    next_bb: LLVMBasicBlockRef) -> CompileResult<()>
{
    let head = seq.head(ctx);
    ctx.add_variable(&ap.head, head);
    let tail = try!(seq.tail(ctx, span));
    ctx.add_variable(&ap.tail, tail);

    let cond = LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntSGT, seq.gen_length(ctx).get(), const_int(ctx, 0), cstr("cmp"));
    LLVMBuildCondBr(ctx.builder, cond, match_case_bb, next_bb);
    Ok(())
}

unsafe fn gen_llvm_equals(
    ctx: &Context,
    left: LLVMValueRef,
    right: LLVMValueRef,
    on_equals_bb: LLVMBasicBlockRef,
    on_not_equals_bb: LLVMBasicBlockRef)
{
    let left_type = LLVMTypeOf(left);
    let cmp = if is_floating_point(ctx.context, left_type) {
        LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealOEQ, left, right, cstr("cmp"))
    } else {
        LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntEQ, left, right, cstr("cmp"))
    };

    LLVMBuildCondBr(ctx.builder, cmp, on_equals_bb, on_not_equals_bb);
}

unsafe fn gen_equals(ctx: &Context,left: &ValueRef, right: &ValueRef, on_equals_bb: LLVMBasicBlockRef, on_not_equals_bb: LLVMBasicBlockRef) -> CompileResult<()>
{
    match (left, right)
    {
        (&ValueRef::Const(lv), &ValueRef::Const(rv)) => Ok(gen_llvm_equals(ctx, lv, rv, on_equals_bb, on_not_equals_bb)),
        (&ValueRef::Ptr(_), &ValueRef::Ptr(_)) => Ok(gen_llvm_equals(ctx, left.load(ctx.builder), right.load(ctx.builder), on_equals_bb, on_not_equals_bb)),
        (&ValueRef::Array(ref l), &ValueRef::Array(ref r)) => gen_equals_seq(ctx, l, r, on_equals_bb, on_not_equals_bb),
        _ => err(&Span::default(), ErrorCode::TypeError, format!("Unsupported comparison"))
    }
}

unsafe fn gen_equals_seq(ctx: &Context, a: &Array, b: &Array, on_equals_bb: LLVMBasicBlockRef, on_not_equals_bb: LLVMBasicBlockRef) -> CompileResult<()>
{
    let func = ctx.get_current_function();
    let after_equal_length_bb = LLVMAppendBasicBlockInContext(ctx.context, func, cstr("after_equal_length_bb"));
    let for_cond_bb = LLVMAppendBasicBlockInContext(ctx.context, func, cstr("for_cond_bb"));
    let for_body_bb = LLVMAppendBasicBlockInContext(ctx.context, func, cstr("for_body_bb"));
    let after_element_cmp_bb = LLVMAppendBasicBlockInContext(ctx.context, func, cstr("after_element_cmp_bb"));

    // First check if sequences have the same length
    let cond = LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntEQ, a.gen_length(ctx).get(), b.gen_length(ctx).get(), cstr("cmp"));
    LLVMBuildCondBr(ctx.builder, cond, after_equal_length_bb, on_not_equals_bb);
    LLVMPositionBuilderAtEnd(ctx.builder, after_equal_length_bb);

    // Then loop over each element and check equality
    let counter = ValueRef::alloc(ctx, &Type::Int);
    try!(counter.store_direct(ctx, const_int(ctx, 0), &Span::default()));
    LLVMBuildBr(ctx.builder, for_cond_bb);

    LLVMPositionBuilderAtEnd(ctx.builder, for_cond_bb);
    let cond = LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntSLT, counter.load(ctx.builder), a.gen_length(ctx).get(), cstr("cmp"));
    LLVMBuildCondBr(ctx.builder, cond, for_body_bb, on_equals_bb);
    LLVMPositionBuilderAtEnd(ctx.builder, for_body_bb);

    let index = counter.load(ctx.builder);
    let a_element = a.get_element(ctx, index);
    let b_element = b.get_element(ctx, index);
    try!(gen_equals(ctx, &a_element, &b_element, after_element_cmp_bb, on_not_equals_bb));

    LLVMPositionBuilderAtEnd(ctx.builder, after_element_cmp_bb);

    // Increment counter, and jump back to for_cond_bb
    let index = LLVMBuildAdd(ctx.builder, index, const_int(ctx, 1), cstr("inc_index"));
    try!(counter.store_direct(ctx, index, &Span::default()));
    LLVMBuildBr(ctx.builder, for_cond_bb);
    Ok(())
}


unsafe fn gen_name_pattern_match(
    ctx: &mut Context,
    mc: &MatchCase,
    target: &ValueRef,
    match_end_bb: LLVMBasicBlockRef,
    match_case_bb: LLVMBasicBlockRef,
    next_bb: LLVMBasicBlockRef,
    dst: &mut ValueRef,
    nr: &NameRef) -> CompileResult<()>
{
    match nr.typ
    {
        Type::Enum(ref et) => {
            let idx = et.index_of(&nr.name).expect("Internal Compiler Error: cannot determine index of sum type case");
            let cv = const_int(ctx, idx as u64);
            let cond = LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntEQ, target.load(ctx.builder), cv, cstr("cmp"));
            LLVMBuildCondBr(ctx.builder, cond, match_case_bb, next_bb);
        },
        Type::Sum(ref st) => {
            let idx = st.index_of(&nr.name).expect("Internal Compiler Error: cannot determine index of sum type case");
            let case_type_ptr = try!(target.case_type(ctx, &nr.span));
            let cv = const_int(ctx, idx as u64);
            let cond = LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntEQ, case_type_ptr.load(ctx.builder), cv, cstr("cmp"));
            LLVMBuildCondBr(ctx.builder, cond, match_case_bb, next_bb);
        },
        _ => {
            if nr.name == "_" {
                LLVMBuildBr(ctx.builder, match_case_bb);
            } else {
                return err(&mc.span, ErrorCode::TypeError, format!("Expression is not a valid match pattern"));
            }
        }
    }

    gen_match_case_to_execute(ctx, mc, dst, match_case_bb, match_end_bb, next_bb)
}

unsafe fn gen_struct_pattern_match(
    ctx: &mut Context,
    mc: &MatchCase,
    target: &ValueRef,
    match_end_bb: LLVMBasicBlockRef,
    match_case_bb: LLVMBasicBlockRef,
    next_bb: LLVMBasicBlockRef,
    dst: &mut ValueRef,
    p: &StructPattern) -> CompileResult<()>
{
    ctx.push_stack(ptr::null_mut());

    let add_bindings = |var: &ValueRef, ctx: &mut Context| -> CompileResult<()> {
        for (idx, b) in p.bindings.iter().enumerate() {
            if b != "_" {
                let member_ptr = try!(var.member(ctx, idx, &p.span));
                ctx.add_variable(&b, member_ptr);
            }
        }
        Ok(())
    };

    match p.typ
    {
        Type::Struct(_) => {
            LLVMBuildBr(ctx.builder, match_case_bb);
            LLVMPositionBuilderAtEnd(ctx.builder, match_case_bb);
            try!(add_bindings(target, ctx));
        },
        Type::Sum(ref st) => {
            let case_type_ptr = try!(target.case_type(ctx, &p.span));
            let idx = st.index_of(&p.name).expect("Internal Compiler Error: cannot determine index of sum type case");
            let cond = LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntEQ, case_type_ptr.load(ctx.builder), const_int(ctx, idx as u64), cstr("cmp"));
            LLVMBuildCondBr(ctx.builder, cond, match_case_bb, next_bb);
            LLVMPositionBuilderAtEnd(ctx.builder, match_case_bb);
            let struct_ptr = try!(target.case_struct(ctx, idx, &p.span));
            try!(add_bindings(&struct_ptr, ctx));
        },
        _ => return err(&mc.span, ErrorCode::TypeError, format!("Expression is not a valid match pattern")),
    }

    try!(gen_match_case_to_execute(ctx, mc, dst, match_case_bb, match_end_bb, next_bb));
    ctx.pop_stack();
    Ok(())
}

unsafe fn gen_match_case(
    ctx: &mut Context,
    mc: &MatchCase,
    target: &ValueRef,
    func: LLVMValueRef,
    match_end_bb: LLVMBasicBlockRef,
    dst: &mut ValueRef) -> CompileResult<()>
{
    let match_case_bb = LLVMAppendBasicBlockInContext(ctx.context, func, cstr("match_case_bb"));
    let next_bb = LLVMAppendBasicBlockInContext(ctx.context, func, cstr("next_bb"));

    match mc.pattern
    {
        Pattern::Literal(Literal::Int(_, v)) => {
            let iv = try!(gen_integer(ctx, v));
            let cond = LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntEQ, target.load(ctx.builder), iv.load(ctx.builder), cstr("cmp"));
            LLVMBuildCondBr(ctx.builder, cond, match_case_bb, next_bb);
            gen_match_case_to_execute(ctx, mc, dst, match_case_bb, match_end_bb, next_bb)
        },

        Pattern::Literal(Literal::Float(ref span, ref v)) => {
            let iv = try!(gen_float(ctx, &v, span));
            let cond = LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealOEQ, target.load(ctx.builder), iv.load(ctx.builder), cstr("cmp"));
            LLVMBuildCondBr(ctx.builder, cond, match_case_bb, next_bb);
            gen_match_case_to_execute(ctx, mc, dst, match_case_bb, match_end_bb, next_bb)
        },

        Pattern::Literal(Literal::Bool(_, v)) => {
            let iv = try!(gen_bool(ctx, v));
            let cond = LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntEQ, target.load(ctx.builder), iv.load(ctx.builder), cstr("cmp"));
            LLVMBuildCondBr(ctx.builder, cond, match_case_bb, next_bb);
            gen_match_case_to_execute(ctx, mc, dst, match_case_bb, match_end_bb, next_bb)
        },

        Pattern::Name(ref nr) => {
            gen_name_pattern_match(ctx, mc, target, match_end_bb, match_case_bb, next_bb, dst, nr)
        },

        Pattern::EmptyArray(_) => {
            match target
            {
                &ValueRef::Array(ref arr) => {
                    let length_ptr = arr.get_length_ptr(ctx);
                    let cond = LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntEQ, length_ptr.load(ctx.builder), const_int(ctx, 0), cstr("cmp"));
                    LLVMBuildCondBr(ctx.builder, cond, match_case_bb, next_bb);
                    gen_match_case_to_execute(ctx, mc, dst, match_case_bb, match_end_bb, next_bb)
                },
                _ => err(&mc.span, ErrorCode::TypeError, format!("Match expression cannot be matched with an array pattern")),
            }
        },

        Pattern::Array(ref ap) => {
            ctx.push_stack(ptr::null_mut());
            match target
            {
                &ValueRef::Array(ref arr) => try!(gen_sequence_match(ctx, ap, arr, &mc.span, match_case_bb, next_bb)),
                _ => return err(&mc.span, ErrorCode::TypeError, format!("Match expression cannot be matched with an array pattern")),
            }

            try!(gen_match_case_to_execute(ctx, mc, dst, match_case_bb, match_end_bb, next_bb));
            ctx.pop_stack();
            Ok(())
        },

        Pattern::Literal(Literal::Array(ref a)) => {
            let arr = try!(gen_array_literal(ctx, a));
            try!(gen_equals(ctx, target, &arr, match_case_bb, next_bb));
            gen_match_case_to_execute(ctx, mc, dst, match_case_bb, match_end_bb, next_bb)
        },

        Pattern::Literal(Literal::String(ref span, ref s)) => {
            let arr = try!(gen_string_literal(ctx, s, span));
            try!(gen_equals(ctx, target, &arr, match_case_bb, next_bb));
            gen_match_case_to_execute(ctx, mc, dst, match_case_bb, match_end_bb, next_bb)
        },

        Pattern::Struct(ref p) => {
            gen_struct_pattern_match(ctx, mc, target, match_end_bb, match_case_bb, next_bb, dst, p)
        }
    }
}

unsafe fn gen_match_store(ctx: &mut Context, m: &MatchExpression, dst: &mut ValueRef) -> CompileResult<()>
{
    let target = try!(gen_expression(ctx, &m.target));
    let func = ctx.get_current_function();
    let match_end_bb = LLVMAppendBasicBlockInContext(ctx.context, func, cstr("match_end_bb"));

    for mc in &m.cases
    {
        try!(gen_match_case(ctx, mc, &target, func, match_end_bb, dst));
    }

    LLVMBuildBr(ctx.builder, match_end_bb);
    LLVMPositionBuilderAtEnd(ctx.builder, match_end_bb);
    Ok(())
}

unsafe fn gen_match(ctx: &mut Context, m: &MatchExpression) -> CompileResult<ValueRef>
{
    let mut dst = ValueRef::alloc(ctx, &m.typ);
    try!(gen_match_store(ctx, m, &mut dst));
    Ok(dst)
}

unsafe fn gen_let_binding(ctx: &mut Context, b: &LetBinding) -> CompileResult<ValueRef>
{
    match b.typ
    {
        Type::Func(ref ft) => {
            let func_ptr = try!(gen_expression(ctx, &b.init));
            let func_sig = anon_sig(&b.name, &ft.return_type, &ft.args);
            let fi = try!(gen_function_ptr(ctx, func_ptr.get(), func_sig));
            let ptr = fi.function;
            ctx.add_function(Rc::new(fi));
            Ok(ValueRef::Ptr(ptr))
        },
        _ => {
            let mut vr = ValueRef::alloc(ctx, &b.typ);
            try!(gen_expression_store(ctx, &b.init, &mut vr));
            ctx.add_variable(&b.name, vr.clone());
            Ok(vr)
        }
    }
}

unsafe fn gen_let_bindings(ctx: &mut Context, l: &LetExpression) -> CompileResult<()>
{
    for b in &l.bindings{
        try!(gen_let_binding(ctx, b));
    }
    Ok(())
}

unsafe fn gen_let_store(ctx: &mut Context, l: &LetExpression, ptr: &mut ValueRef) -> CompileResult<()>
{
    ctx.push_stack(ptr::null_mut());
    try!(gen_let_bindings(ctx, l));
    try!(gen_expression_store(ctx, &l.expression, ptr));
    ctx.pop_stack();
    Ok(())
}

unsafe fn gen_let(ctx: &mut Context, l: &LetExpression) -> CompileResult<ValueRef>
{
    ctx.push_stack(ptr::null_mut());
    try!(gen_let_bindings(ctx, l));
    let result = try!(gen_expression(ctx, &l.expression));
    ctx.pop_stack();
    Ok(result)
}

unsafe fn gen_array_literal_store(ctx: &mut Context, a: &ArrayLiteral, array: &mut Array) -> CompileResult<()>
{
    try!(array.init(ctx, a.elements.len(), &a.span));
    for (idx, element) in a.elements.iter().enumerate()
    {
        let index = const_int(ctx, idx as u64);
        let el_ptr = array.get_element(ctx, index);
        let e_val = try!(gen_expression(ctx, element));
        try!(el_ptr.store(ctx, e_val, &a.span));
    }

    Ok(())
}

unsafe fn gen_array_literal(ctx: &mut Context, a: &ArrayLiteral) -> CompileResult<ValueRef>
{
    if a.elements.len() == 0 {
        let slice = try!(Array::empty(ctx, &a.span));
        return Ok(ValueRef::Array(slice));
    }

    match a.array_type
    {
        Type::Array(ref at) => {
            let mut array = try!(Array::alloc(ctx, at.element_type.clone(), a.elements.len(), &a.span));
            try!(gen_array_literal_store(ctx, a, &mut array));
            let var = ValueRef::Array(array);
            Ok(var)
        },
        _ => panic!("Array literal has the type {}", a.array_type),
    }


}

unsafe fn store(ctx: &mut Context, e: &Expression, ptr: &ValueRef) -> CompileResult<()>
{
    let v = try!(gen_expression(ctx, e));
    ptr.store(ctx, v, &e.span())
}

unsafe fn gen_struct_initializer_store(ctx: &mut Context, si: &StructInitializer, var: &ValueRef) -> CompileResult<()>
{
    let store_members = |var: &ValueRef, ctx: &mut Context| {
        for (idx, ref member_init) in si.member_initializers.iter().enumerate() {
            let mut member_ptr = try!(var.member(ctx, idx, &si.span));
            try!(gen_expression_store(ctx, member_init, &mut member_ptr));
        }
        Ok(())
    };
    match si.typ
    {
        Type::Struct(_) => {
            store_members(var, ctx)
        }
        Type::Sum(ref st) => {
            let index = st.index_of(&si.struct_name).expect("Internal Compiler Error: cannot determine index of sum type case");
            let type_ptr = try!(var.case_type(ctx, &si.span));
            try!(type_ptr.store_direct(ctx, const_int(ctx, index as u64), &si.span));
            let data_struct_ptr = try!(var.case_struct(ctx, index, &si.span));
            store_members(&data_struct_ptr, ctx)
        }
        _ => panic!("Internal Compiler Error: Invalid type {} for struct intializer", si.typ)
    }
}

unsafe fn gen_struct_initializer(ctx: &mut Context, si: &StructInitializer) -> CompileResult<ValueRef>
{
    let var = ValueRef::alloc(ctx, &si.typ);
    try!(gen_struct_initializer_store(ctx, si, &var));
    Ok(var)
}

unsafe fn gen_struct_member_access(ctx: &Context, sma: &StructMemberAccess) -> CompileResult<ValueRef>
{
    let mut var = try!(ctx.get_variable(&sma.name).ok_or(unknown_name(&sma.span, &sma.name))).value.clone();
    for idx in &sma.indices
    {
        var = try!(var.member(ctx, *idx, &sma.span));
    }

    Ok(var)
}

unsafe fn gen_block(ctx: &mut Context, block: &Block) -> CompileResult<ValueRef>
{
    ctx.push_stack(ptr::null_mut());
    let mut ret = None;
    for e in &block.expressions {
        ret = Some(try!(gen_expression(ctx, e)));
    }
    ctx.pop_stack();
    Ok(ret.expect("Block has no expressions")) // Type checker should make this impossible
}

unsafe fn gen_block_store(ctx: &mut Context, block: &Block, ptr: &mut ValueRef) -> CompileResult<()>
{
    ctx.push_stack(ptr::null_mut());
    for (idx, e) in block.expressions.iter().enumerate() {
        if idx == block.expressions.len() - 1 {
            try!(gen_expression_store(ctx, e, ptr))
        } else {
            try!(gen_expression(ctx, e));
        }
    }
    ctx.pop_stack();
    Ok(())
}

pub unsafe fn gen_expression_store(ctx: &mut Context, e: &Expression, ptr: &mut ValueRef) -> CompileResult<()>
{
    match *e
    {
        Expression::Literal(Literal::Array(ref a)) => {
            match *ptr
            {
                ValueRef::Array(ref mut array) => gen_array_literal_store(ctx, a, array),
                _ => panic!("Expecting array value here"),
            }
        },
        Expression::StructInitializer(ref si) => gen_struct_initializer_store(ctx, si, ptr),
        Expression::Call(ref c) => gen_call_store(ctx, c, ptr),
        Expression::NameRef(ref nr) => gen_name_ref_store(ctx, nr, ptr),
        Expression::Match(ref m) => gen_match_store(ctx, m, ptr),
        Expression::Lambda(ref l) => gen_lambda_store(ctx, l, ptr),
        Expression::Let(ref l) => gen_let_store(ctx, l, ptr),
        Expression::Block(ref b) => gen_block_store(ctx, b, ptr),
        Expression::If(ref i) => {
            let match_expr = i.to_match();
            gen_match_store(ctx, &match_expr, ptr)
        },
        _ => store(ctx, e, &ptr),
    }
}

pub fn gen_expression(ctx: &mut Context, e: &Expression) -> CompileResult<ValueRef>
{
    unsafe
    {
        match *e
        {
            Expression::UnaryOp(ref u) => gen_unary_op(ctx, u),
            Expression::BinaryOp(ref op) => gen_binary_op(ctx, op),
            Expression::Literal(Literal::Array(ref a)) => gen_array_literal(ctx, a),
            Expression::ArrayGenerator(ref _a) => panic!("NYI"),
            Expression::Call(ref c) => gen_call(ctx, c),
            Expression::NameRef(ref nr) => gen_name_ref(ctx, nr),
            Expression::Match(ref m) => gen_match(ctx, m),
            Expression::Lambda(ref l) => gen_lambda(ctx, l),
            Expression::Let(ref l) => gen_let(ctx, l),
            Expression::LetBindings(ref l) => {
                let mut v = None;
                for b in &l.bindings {
                    v = Some(try!(gen_let_binding(ctx, b)));
                }
                Ok(v.expect("Empting binding list"))
            },
            Expression::If(ref i) => {
                let match_expr = i.to_match();
                gen_match(ctx, &match_expr)
            },
            Expression::Block(ref b) => gen_block(ctx, b),
            Expression::Literal(Literal::Int(_, v)) => gen_integer(ctx, v),
            Expression::Literal(Literal::Float(ref span, ref v_str)) => gen_float(ctx, &v_str, span),
            Expression::Literal(Literal::String(ref span, ref s))  => gen_string_literal(ctx, s, span),
            Expression::Literal(Literal::Bool(_, v)) => gen_bool(ctx, v),
            Expression::StructInitializer(ref si) => gen_struct_initializer(ctx, si),
            Expression::StructMemberAccess(ref sma) => gen_struct_member_access(ctx, sma),
        }
    }
}
