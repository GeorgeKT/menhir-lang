use std::ptr;
use std::rc::Rc;
use std::ffi::CString;
use std::os::raw::c_uint;

use libc;
use llvm::core::*;
use llvm::prelude::*;
use llvm::*;

use ast::*;
use parser::Operator;
use codegen::{Context, ValueRef, Array};
use codegen::symboltable::{FunctionInstance};


unsafe fn is_floating_point(ctx: LLVMContextRef, tr: LLVMTypeRef) -> bool
{
    tr == LLVMDoubleTypeInContext(ctx)
}

pub unsafe fn const_int(ctx: &Context, v: u64) -> LLVMValueRef
{
    LLVMConstInt(LLVMInt64TypeInContext(ctx.context), v, 0)
}

pub unsafe fn const_bool(ctx: &Context, v: bool) -> LLVMValueRef
{
    LLVMConstInt(LLVMInt1TypeInContext(ctx.context), if v {1} else {0}, 0)
}

unsafe fn gen_integer(ctx: &Context, v: u64) -> ValueRef
{
    ValueRef::Const(const_int(ctx, v))
}

unsafe fn gen_bool(ctx: &Context, v: bool) -> ValueRef
{
    ValueRef::Const(LLVMConstInt(LLVMInt1TypeInContext(ctx.context), if v {1} else {0}, 0))
}

unsafe fn gen_char(ctx: &Context, v: u8) -> ValueRef
{
    ValueRef::Const(LLVMConstInt(LLVMInt8TypeInContext(ctx.context), v as u64, 0))
}


unsafe fn gen_float(ctx: &Context, num: &str) -> ValueRef
{
    match num.parse::<f64>()
    {
        Ok(f) => ValueRef::Const(LLVMConstReal(LLVMDoubleTypeInContext(ctx.context), f)),
        Err(_) => panic!("Internal Compiler Error: {} is not a valid floating point number", num)
    }
}

unsafe fn gen_const_string_literal(ctx: &Context, s: &str) -> ValueRef
{
    ValueRef::Const(LLVMConstStringInContext(ctx.context, s.as_ptr() as *const i8, s.len() as u32, 1))
}

unsafe fn gen_string_literal(ctx: &Context, s: &str) -> ValueRef
{
    let typ = ctx.resolve_type(&string_type());
    let array = Array::new(ctx.stack_alloc(typ, "string"), Type::Char, s.is_empty());
    gen_string_literal_store(ctx, s, &array);
    ValueRef::Array(array)
}

unsafe fn gen_string_literal_store(ctx: &Context, s: &str, array: &Array)
{
    let glob = LLVMAddGlobal(ctx.module, LLVMArrayType(LLVMInt8TypeInContext(ctx.context), s.len() as c_uint),  cstr!("string"));

    LLVMSetLinkage(glob, LLVMLinkage::LLVMInternalLinkage);
    LLVMSetGlobalConstant(glob, 1);
    let string_data = gen_const_string_literal(ctx, s);
    LLVMSetInitializer(glob, string_data.get());
    array.fill_with_string_literal(ctx, glob, s.len())
}

unsafe fn gen_unary_op(ctx: &mut Context, op: &UnaryOp) -> ValueRef
{
    let e_val = gen_expression(ctx, &op.expression);
    match op.operator
    {
        Operator::Sub => {
            ValueRef::Const(LLVMBuildNeg(ctx.builder, e_val.load(ctx.builder), cstr!("neg")))
        },
        Operator::Not => {
            ValueRef::Const(LLVMBuildNot(ctx.builder, e_val.load(ctx.builder), cstr!("not")))
        },
        _ => panic!("Internal Compiler Error: Operator {} is not a unary operator", op.operator),
    }
}



unsafe fn gen_binary_op(ctx: &mut Context, op: &BinaryOp) -> ValueRef
{
    let left_val = gen_expression(ctx, &op.left);
    let right_val = gen_expression(ctx, &op.right);

    match (op.left.get_type(), op.right.get_type())
    {
        (Type::Int, Type::Int) => {
            let l = left_val.load(ctx.builder);
            let r = right_val.load(ctx.builder);

            ValueRef::Const(match op.operator
            {
                Operator::Add => LLVMBuildAdd(ctx.builder, l, r, cstr!("add")),
                Operator::Sub => LLVMBuildSub(ctx.builder, l, r, cstr!("sub")),
                Operator::Mul => LLVMBuildMul(ctx.builder, l, r, cstr!("mul")),
                Operator::Div => LLVMBuildUDiv(ctx.builder, l, r, cstr!("div")),
                Operator::Mod => LLVMBuildURem(ctx.builder, l, r, cstr!("mod")),
                Operator::LessThan => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntSLT, l, r, cstr!("cmp")),
                Operator::LessThanEquals => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntSLE, l, r, cstr!("cmp")),
                Operator::GreaterThan => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntSGT, l, r, cstr!("cmp")),
                Operator::GreaterThanEquals => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntSGE, l, r, cstr!("cmp")),
                Operator::Equals => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntEQ, l, r, cstr!("cmp")),
                Operator::NotEquals => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntNE, l, r, cstr!("cmp")),
                _ => panic!("Internal Compiler Error: Operator {} is not supported on integers", op.operator),
            })
        }

        (Type::Float, Type::Float) => {
            let l = left_val.load(ctx.builder);
            let r = right_val.load(ctx.builder);

            ValueRef::Const(match op.operator
            {
                Operator::Add => LLVMBuildFAdd(ctx.builder, l, r, cstr!("add")),
                Operator::Sub => LLVMBuildFSub(ctx.builder, l, r, cstr!("sub")),
                Operator::Mul => LLVMBuildFMul(ctx.builder, l, r, cstr!("mul")),
                Operator::Div => LLVMBuildFDiv(ctx.builder, l, r, cstr!("div")),
                Operator::Mod => LLVMBuildFRem(ctx.builder, l, r, cstr!("mod")),
                Operator::LessThan => LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealOLT, l, r, cstr!("cmp")),
                Operator::LessThanEquals => LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealOLE, l, r, cstr!("cmp")),
                Operator::GreaterThan => LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealOGT, l, r, cstr!("cmp")),
                Operator::GreaterThanEquals => LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealOGE, l, r, cstr!("cmp")),
                Operator::Equals => LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealOEQ, l, r, cstr!("cmp")),
                Operator::NotEquals => LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealONE, l, r, cstr!("cmp")),
                _ => panic!("Internal Compiler Error: Operator {} is not supported on floats", op.operator),
            })
        },

        (Type::Bool, Type::Bool) => {
            let l = left_val.load(ctx.builder);
            let r = right_val.load(ctx.builder);

            ValueRef::Const(match op.operator
            {
                Operator::And => LLVMBuildAnd(ctx.builder, l, r, cstr!("and")),
                Operator::Or => LLVMBuildOr(ctx.builder, left_val.load(ctx.builder), right_val.load(ctx.builder), cstr!("or")),
                _ => panic!("Internal Compiler Error: Operator {} is not supported on bools", op.operator),
            })
        },

        (Type::Array(_), Type::Array(_)) => {

            if let (ValueRef::Array(ref ar), ValueRef::Array(ref br)) = (left_val, right_val)
            {
                match op.operator
                {
                    Operator::Add => {
                        ValueRef::Array(Array::concat(ctx, ar, br))
                    },

                    Operator::Equals | Operator::NotEquals => {
                        let dst = gen_array_equals(ctx, ar, br);
                        ValueRef::Const(
                            if op.operator == Operator::Equals {dst} else {LLVMBuildNot(ctx.builder, dst, cstr!("not"))}
                        )
                    }

                    _ => panic!("Internal Compiler Error: Operator {} is not supported on arrays", op.operator),
                }
            }
            else
            {
                panic!("Internal Compiler Error: Expecting array ValueRef's here");
            }

        }

        _ => panic!("Internal Compiler Error: Operator {} is not a binary operator", op.operator),
    }
}

pub unsafe fn gen_array_equals(ctx: &mut Context, ar: &Array, br: &Array) -> LLVMValueRef
{
    let dst = ctx.stack_alloc(ctx.resolve_type(&Type::Bool), "dst");
    let func = ctx.get_current_function();
    let on_eq = LLVMAppendBasicBlockInContext(ctx.context, func, cstr!("on_eq"));
    let on_not_eq = LLVMAppendBasicBlockInContext(ctx.context, func, cstr!("on_not_eq"));
    let after_eq = LLVMAppendBasicBlockInContext(ctx.context, func, cstr!("after_eq"));
    gen_equals_seq(ctx, ar, br, on_eq, on_not_eq);

    LLVMPositionBuilderAtEnd(ctx.builder, on_eq);
    LLVMBuildStore(ctx.builder, const_int(ctx, 1), dst);
    LLVMBuildBr(ctx.builder, after_eq);

    LLVMPositionBuilderAtEnd(ctx.builder, on_not_eq);
    LLVMBuildStore(ctx.builder, const_int(ctx, 0), dst);
    LLVMBuildBr(ctx.builder, after_eq);

    LLVMPositionBuilderAtEnd(ctx.builder, after_eq);
    dst
}

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


pub unsafe fn gen_function_sig(ctx: &Context, sig: &FunctionSignature) -> FunctionInstance
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

pub unsafe fn gen_function(ctx: &mut Context, signature: &FunctionSignature, body: &Expression) -> ValueRef
{
    let fi = ctx.get_function(&signature.name).expect("Internal Compiler Error: Unknown function");
    let bb = LLVMAppendBasicBlockInContext(ctx.context, fi.function, cstr!("entry"));
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

    let ret = if signature.return_type.return_by_ptr()
    {
        let ret_arg = LLVMGetParam(fi.function, signature.args.len() as libc::c_uint);
        let mut vr = ValueRef::new(ret_arg, &signature.return_type);
        gen_expression_store(ctx, body, &mut vr);
        ctx.pop_stack();
        LLVMBuildRetVoid(ctx.builder);
        vr
    }
    else
    {
        let ret = gen_expression(ctx, body);
        ctx.pop_stack();
        LLVMBuildRet(ctx.builder, ret.load(ctx.builder));
        ret
    };

    if current_bb != ptr::null_mut() {
        LLVMPositionBuilderAtEnd(ctx.builder, current_bb);
    }

    ret
}

unsafe fn gen_lambda_store(ctx: &mut Context, l: &Lambda, ptr: &mut ValueRef)
{
    let fi = gen_function_sig(ctx, &l.sig);
    let ret = fi.function;
    ctx.add_function(Rc::new(fi));
    gen_function(ctx, &l.sig, &l.expr);
    *ptr = ValueRef::Ptr(ret);
}

unsafe fn gen_lambda(ctx: &mut Context, l: &Lambda) -> ValueRef
{
    let fi = gen_function_sig(ctx, &l.sig);
    let ret = fi.function;
    ctx.add_function(Rc::new(fi));
    gen_function(ctx, &l.sig, &l.expr);
    ValueRef::Ptr(ret)
}

unsafe fn gen_call_args(ctx: &mut Context, c: &Call, func: &FunctionInstance) -> Vec<LLVMValueRef>
{
    let mut arg_vals = Vec::with_capacity(func.args.len());
    for (ref arg, passing_mode) in c.args.iter().zip(func.args.iter().map(|&(_, passing_mode)| passing_mode.clone()))
    {
        let a = gen_expression(ctx, &arg);
        match passing_mode
        {
            ArgumentPassingMode::ByValue => arg_vals.push(a.load(ctx.builder)),
            ArgumentPassingMode::ByPtr => arg_vals.push(a.get()),
        }
    }
    arg_vals
}

unsafe fn gen_call_store(ctx: &mut Context, c: &Call, ptr: &mut ValueRef)
{
    let func = ctx.get_function(&c.callee.name).expect("Internal Compiler Error: Unknown function");

    let mut arg_vals = gen_call_args(ctx, c, &func);
    if func.sig.return_type.return_by_ptr()
    {
        arg_vals.push(ptr.get());
        LLVMBuildCall(ctx.builder, func.function, arg_vals.as_mut_ptr(), arg_vals.len() as libc::c_uint, cstr!(""));
    }
    else
    {
        let call_ret = LLVMBuildCall(ctx.builder, func.function, arg_vals.as_mut_ptr(), arg_vals.len() as libc::c_uint, cstr!(""));
        ptr.store(ctx, ValueRef::Const(call_ret));
    }
}

unsafe fn gen_call(ctx: &mut Context, c: &Call) -> ValueRef
{
    let func = ctx.get_function(&c.callee.name).expect("Internal Compiler Error: Unknown function");
    if func.sig.return_type.return_by_ptr()
    {
        let mut vr = ValueRef::alloc(ctx, &func.sig.return_type);
        gen_call_store(ctx, c, &mut vr);
        vr
    }
    else
    {
        let mut arg_vals = gen_call_args(ctx, c, &func);
        ValueRef::Const(LLVMBuildCall(ctx.builder, func.function, arg_vals.as_mut_ptr(), arg_vals.len() as libc::c_uint, cstr!("")))
    }
}

unsafe fn gen_name_ref_store(ctx: &Context, nr: &NameRef, ptr: &mut ValueRef)
{
    let v = gen_name_ref(ctx, nr);
    ptr.store(ctx, v);
}

unsafe fn gen_name_ref(ctx: &Context, nr: &NameRef) -> ValueRef
{
    if let Some(vi) = ctx.get_variable(&nr.name) {
        vi.value.clone()
    } else if let Some(fi) = ctx.get_function(&nr.name) {
        ValueRef::Const(fi.function)
    } else {
        match nr.typ
        {
            Type::Sum(ref st) => {
                let sv = ValueRef::alloc(ctx, &nr.typ);
                let case_type_ptr = sv.case_type(ctx);
                let idx = st.index_of(&nr.name).expect("Internal Compiler Error: cannot determine index of sum type case");
                case_type_ptr.store_direct(ctx, const_int(ctx, idx as u64));
                sv
            },
            Type::Enum(ref et) => {
                let idx = et.index_of(&nr.name).expect("Internal Compiler Error: cannot determine index of sum type case");
                ValueRef::Const(const_int(ctx, idx as u64))
            },
            _ => {
                panic!("Internal Compiler Error: Unknown name {}", nr.name)
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
    next_bb: LLVMBasicBlockRef)
{
    LLVMPositionBuilderAtEnd(ctx.builder, match_case_bb);
    gen_expression_store(ctx, &mc.to_execute, dst);
    LLVMBuildBr(ctx.builder, match_end_bb);
    LLVMPositionBuilderAtEnd(ctx.builder, next_bb);
}

unsafe fn gen_sequence_match(
    ctx: &mut Context,
    ap: &ArrayPattern,
    seq: &Array,
    match_case_bb: LLVMBasicBlockRef,
    next_bb: LLVMBasicBlockRef)
{
    let head = seq.head(ctx);
    ctx.add_variable(&ap.head, head);
    let tail = seq.tail(ctx);
    ctx.add_variable(&ap.tail, tail);

    let cond = LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntSGT, seq.gen_length(ctx).get(), const_int(ctx, 0), cstr!("cmp"));
    LLVMBuildCondBr(ctx.builder, cond, match_case_bb, next_bb);
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
        LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealOEQ, left, right, cstr!("cmp"))
    } else {
        LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntEQ, left, right, cstr!("cmp"))
    };

    LLVMBuildCondBr(ctx.builder, cmp, on_equals_bb, on_not_equals_bb);
}

unsafe fn gen_equals(ctx: &Context,left: &ValueRef, right: &ValueRef, on_equals_bb: LLVMBasicBlockRef, on_not_equals_bb: LLVMBasicBlockRef)
{
    match (left, right)
    {
        (&ValueRef::Const(lv), &ValueRef::Const(rv)) => gen_llvm_equals(ctx, lv, rv, on_equals_bb, on_not_equals_bb),
        (&ValueRef::Ptr(_), &ValueRef::Ptr(_)) => gen_llvm_equals(ctx, left.load(ctx.builder), right.load(ctx.builder), on_equals_bb, on_not_equals_bb),
        (&ValueRef::Array(ref l), &ValueRef::Array(ref r)) => gen_equals_seq(ctx, l, r, on_equals_bb, on_not_equals_bb),
        _ => panic!("Internal Compiler Error: Unsupported comparison")
    }
}

unsafe fn gen_equals_seq(ctx: &Context, a: &Array, b: &Array, on_equals_bb: LLVMBasicBlockRef, on_not_equals_bb: LLVMBasicBlockRef)
{
    let func = ctx.get_current_function();
    let after_equal_length_bb = LLVMAppendBasicBlockInContext(ctx.context, func, cstr!("after_equal_length_bb"));
    let for_cond_bb = LLVMAppendBasicBlockInContext(ctx.context, func, cstr!("for_cond_bb"));
    let for_body_bb = LLVMAppendBasicBlockInContext(ctx.context, func, cstr!("for_body_bb"));
    let after_element_cmp_bb = LLVMAppendBasicBlockInContext(ctx.context, func, cstr!("after_element_cmp_bb"));

    // First check if sequences have the same length
    let cond = LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntEQ, a.gen_length(ctx).get(), b.gen_length(ctx).get(), cstr!("cmp"));
    LLVMBuildCondBr(ctx.builder, cond, after_equal_length_bb, on_not_equals_bb);
    LLVMPositionBuilderAtEnd(ctx.builder, after_equal_length_bb);

    // Then loop over each element and check equality
    let counter = ValueRef::alloc(ctx, &Type::Int);
    counter.store_direct(ctx, const_int(ctx, 0));
    LLVMBuildBr(ctx.builder, for_cond_bb);

    LLVMPositionBuilderAtEnd(ctx.builder, for_cond_bb);
    let cond = LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntSLT, counter.load(ctx.builder), a.gen_length(ctx).get(), cstr!("cmp"));
    LLVMBuildCondBr(ctx.builder, cond, for_body_bb, on_equals_bb);
    LLVMPositionBuilderAtEnd(ctx.builder, for_body_bb);

    let index = counter.load(ctx.builder);
    let a_element = a.get_element(ctx, index);
    let b_element = b.get_element(ctx, index);
    gen_equals(ctx, &a_element, &b_element, after_element_cmp_bb, on_not_equals_bb);

    LLVMPositionBuilderAtEnd(ctx.builder, after_element_cmp_bb);

    // Increment counter, and jump back to for_cond_bb
    let index = LLVMBuildAdd(ctx.builder, index, const_int(ctx, 1), cstr!("inc_index"));
    counter.store_direct(ctx, index);
    LLVMBuildBr(ctx.builder, for_cond_bb);
}


unsafe fn gen_name_pattern_match(
    ctx: &mut Context,
    mc: &MatchCase,
    target: &ValueRef,
    match_end_bb: LLVMBasicBlockRef,
    match_case_bb: LLVMBasicBlockRef,
    next_bb: LLVMBasicBlockRef,
    dst: &mut ValueRef,
    nr: &NameRef)
{
    match nr.typ
    {
        Type::Enum(ref et) => {
            let idx = et.index_of(&nr.name).expect("Internal Compiler Error: cannot determine index of sum type case");
            let cv = const_int(ctx, idx as u64);
            let cond = LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntEQ, target.load(ctx.builder), cv, cstr!("cmp"));
            LLVMBuildCondBr(ctx.builder, cond, match_case_bb, next_bb);
        },
        Type::Sum(ref st) => {
            let idx = st.index_of(&nr.name).expect("Internal Compiler Error: cannot determine index of sum type case");
            let case_type_ptr = target.case_type(ctx);
            let cv = const_int(ctx, idx as u64);
            let cond = LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntEQ, case_type_ptr.load(ctx.builder), cv, cstr!("cmp"));
            LLVMBuildCondBr(ctx.builder, cond, match_case_bb, next_bb);
        },
        _ => {
            panic!("Internal Compiler Error: Expression is not a valid match pattern");
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
    p: &StructPattern)
{
    ctx.push_stack(ptr::null_mut());

    let add_bindings = |var: &ValueRef, ctx: &mut Context| {
        for (idx, b) in p.bindings.iter().enumerate() {
            if b != "_" {
                let member_ptr = var.member(ctx, &MemberAccessType::StructMember(idx));
                ctx.add_variable(&b, member_ptr);
            }
        }
    };

    match p.typ
    {
        Type::Struct(_) => {
            LLVMBuildBr(ctx.builder, match_case_bb);
            LLVMPositionBuilderAtEnd(ctx.builder, match_case_bb);
            add_bindings(target, ctx);
        },
        Type::Sum(ref st) => {
            let case_type_ptr = target.case_type(ctx);
            let idx = st.index_of(&p.name).expect("Internal Compiler Error: cannot determine index of sum type case");
            let cond = LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntEQ, case_type_ptr.load(ctx.builder), const_int(ctx, idx as u64), cstr!("cmp"));
            LLVMBuildCondBr(ctx.builder, cond, match_case_bb, next_bb);
            LLVMPositionBuilderAtEnd(ctx.builder, match_case_bb);
            let struct_ptr = target.case_struct(ctx, idx);
            add_bindings(&struct_ptr, ctx);
        },
        _ => panic!("Internal Compiler Error: Expression is not a valid match pattern"),
    }

    gen_match_case_to_execute(ctx, mc, dst, match_case_bb, match_end_bb, next_bb);
    ctx.pop_stack();
}

unsafe fn gen_match_case(
    ctx: &mut Context,
    mc: &MatchCase,
    target: &ValueRef,
    func: LLVMValueRef,
    match_end_bb: LLVMBasicBlockRef,
    dst: &mut ValueRef)
{
    let match_case_bb = LLVMAppendBasicBlockInContext(ctx.context, func, cstr!("match_case_bb"));
    let next_bb = LLVMAppendBasicBlockInContext(ctx.context, func, cstr!("next_bb"));

    match mc.pattern
    {
        Pattern::Literal(Literal::Int(_, v)) => {
            let iv = gen_integer(ctx, v);
            let cond = LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntEQ, target.load(ctx.builder), iv.load(ctx.builder), cstr!("cmp"));
            LLVMBuildCondBr(ctx.builder, cond, match_case_bb, next_bb);
            gen_match_case_to_execute(ctx, mc, dst, match_case_bb, match_end_bb, next_bb)
        },

        Pattern::Literal(Literal::Float(_, ref v)) => {
            let iv = gen_float(ctx, &v);
            let cond = LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealOEQ, target.load(ctx.builder), iv.load(ctx.builder), cstr!("cmp"));
            LLVMBuildCondBr(ctx.builder, cond, match_case_bb, next_bb);
            gen_match_case_to_execute(ctx, mc, dst, match_case_bb, match_end_bb, next_bb)
        },

        Pattern::Literal(Literal::Bool(_, v)) => {
            let iv = gen_bool(ctx, v);
            let cond = LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntEQ, target.load(ctx.builder), iv.load(ctx.builder), cstr!("cmp"));
            LLVMBuildCondBr(ctx.builder, cond, match_case_bb, next_bb);
            gen_match_case_to_execute(ctx, mc, dst, match_case_bb, match_end_bb, next_bb)
        },

        Pattern::Literal(Literal::Char(_, v)) => {
            let iv = gen_char(ctx, v);
            let cond = LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntEQ, target.load(ctx.builder), iv.load(ctx.builder), cstr!("cmp"));
            LLVMBuildCondBr(ctx.builder, cond, match_case_bb, next_bb);
            gen_match_case_to_execute(ctx, mc, dst, match_case_bb, match_end_bb, next_bb)
        },

        Pattern::Name(ref nr) => {
            gen_name_pattern_match(ctx, mc, target, match_end_bb, match_case_bb, next_bb, dst, nr)
        },

        Pattern::Any(_) => {
            LLVMBuildBr(ctx.builder, match_case_bb);
            gen_match_case_to_execute(ctx, mc, dst, match_case_bb, match_end_bb, next_bb)
        },

        Pattern::EmptyArray(_) => {
            match target
            {
                &ValueRef::Array(ref arr) => {
                    let length_ptr = arr.get_length_ptr(ctx);
                    let cond = LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntEQ, length_ptr.load(ctx.builder), const_int(ctx, 0), cstr!("cmp"));
                    LLVMBuildCondBr(ctx.builder, cond, match_case_bb, next_bb);
                    gen_match_case_to_execute(ctx, mc, dst, match_case_bb, match_end_bb, next_bb)
                },
                _ => panic!("Internal Compiler Error: Match expression cannot be matched with an array pattern"),
            }
        },

        Pattern::Array(ref ap) => {
            ctx.push_stack(ptr::null_mut());
            match target
            {
                &ValueRef::Array(ref arr) => gen_sequence_match(ctx, ap, arr, match_case_bb, next_bb),
                _ => panic!("Internal Compiler Error: Match expression cannot be matched with an array pattern"),
            }

            gen_match_case_to_execute(ctx, mc, dst, match_case_bb, match_end_bb, next_bb);
            ctx.pop_stack();
        },

        Pattern::Literal(Literal::Array(ref a)) => {
            let arr = gen_array_literal(ctx, a);
            gen_equals(ctx, target, &arr, match_case_bb, next_bb);
            gen_match_case_to_execute(ctx, mc, dst, match_case_bb, match_end_bb, next_bb);
        },

        Pattern::Literal(Literal::String(_, ref s)) => {
            let arr = gen_string_literal(ctx, s);
            gen_equals(ctx, target, &arr, match_case_bb, next_bb);
            gen_match_case_to_execute(ctx, mc, dst, match_case_bb, match_end_bb, next_bb);
        },

        Pattern::Struct(ref p) => {
            gen_struct_pattern_match(ctx, mc, target, match_end_bb, match_case_bb, next_bb, dst, p);
        }
    }
}

unsafe fn gen_match_store(ctx: &mut Context, m: &MatchExpression, dst: &mut ValueRef)
{
    let target = gen_expression(ctx, &m.target);
    let func = ctx.get_current_function();
    let match_end_bb = LLVMAppendBasicBlockInContext(ctx.context, func, cstr!("match_end_bb"));

    for mc in &m.cases
    {
        gen_match_case(ctx, mc, &target, func, match_end_bb, dst);
    }

    LLVMBuildBr(ctx.builder, match_end_bb);
    LLVMPositionBuilderAtEnd(ctx.builder, match_end_bb);
}

unsafe fn gen_match(ctx: &mut Context, m: &MatchExpression) -> ValueRef
{
    let mut dst = ValueRef::alloc(ctx, &m.typ);
    gen_match_store(ctx, m, &mut dst);
    dst
}

unsafe fn gen_let_binding(ctx: &mut Context, b: &LetBinding) -> ValueRef
{
    match b.typ
    {
        Type::Func(ref ft) => {
            let func_ptr = gen_expression(ctx, &b.init);
            let func_sig = anon_sig(&b.name, &ft.return_type, &ft.args);
            let fi = gen_function_ptr(ctx, func_ptr.get(), func_sig);
            let ptr = fi.function;
            ctx.add_function(Rc::new(fi));
            ValueRef::Ptr(ptr)
        },
        _ => {
            let mut vr = ValueRef::alloc(ctx, &b.typ);
            gen_expression_store(ctx, &b.init, &mut vr);
            ctx.add_variable(&b.name, vr.clone());
            vr
        }
    }
}

unsafe fn gen_let_bindings(ctx: &mut Context, l: &LetExpression)
{
    for b in &l.bindings{
        gen_let_binding(ctx, b);
    }
}

unsafe fn gen_let_store(ctx: &mut Context, l: &LetExpression, ptr: &mut ValueRef)
{
    ctx.push_stack(ptr::null_mut());
    gen_let_bindings(ctx, l);
    gen_expression_store(ctx, &l.expression, ptr);
    ctx.pop_stack();
}

unsafe fn gen_let(ctx: &mut Context, l: &LetExpression) -> ValueRef
{
    ctx.push_stack(ptr::null_mut());
    gen_let_bindings(ctx, l);
    let result = gen_expression(ctx, &l.expression);
    ctx.pop_stack();
    result
}

unsafe fn gen_array_literal_store(ctx: &mut Context, a: &ArrayLiteral, array: &mut Array)
{
    let len = const_int(ctx, a.elements.len() as u64);
    array.init(ctx, len);
    for (idx, element) in a.elements.iter().enumerate()
    {
        let index = const_int(ctx, idx as u64);
        let mut el_ptr = array.get_element(ctx, index);
        let e_val = gen_expression(ctx, element);
        el_ptr.store(ctx, e_val);
    }
}

unsafe fn gen_array_literal(ctx: &mut Context, a: &ArrayLiteral) -> ValueRef
{
    if a.elements.len() == 0 {
        let slice = Array::empty(ctx);
        return ValueRef::Array(slice);
    }

    match a.array_type
    {
        Type::Array(ref at) => {
            let array_type = ctx.resolve_type(&a.array_type);
            let mut array = Array::alloc(ctx, array_type, at.element_type.clone(), a.elements.len());
            gen_array_literal_store(ctx, a, &mut array);
            ValueRef::Array(array)
        },
        _ => panic!("Array literal has the type {}", a.array_type),
    }
}

unsafe fn store(ctx: &mut Context, e: &Expression, ptr: &mut ValueRef)
{
    let v = gen_expression(ctx, e);
    ptr.store(ctx, v);
}

unsafe fn gen_struct_initializer_store(ctx: &mut Context, si: &StructInitializer, var: &ValueRef)
{
    let store_members = |var: &ValueRef, ctx: &mut Context| {
        for (idx, ref member_init) in si.member_initializers.iter().enumerate() {
            let mut member_ptr = var.member(ctx, &MemberAccessType::StructMember(idx));
            gen_expression_store(ctx, member_init, &mut member_ptr);
        }
    };
    match si.typ
    {
        Type::Struct(_) => {
            store_members(var, ctx);
        }
        Type::Sum(ref st) => {
            let index = st.index_of(&si.struct_name).expect("Internal Compiler Error: cannot determine index of sum type case");
            let type_ptr = var.case_type(ctx);
            type_ptr.store_direct(ctx, const_int(ctx, index as u64));
            let data_struct_ptr = var.case_struct(ctx, index);
            store_members(&data_struct_ptr, ctx);
        }
        _ => panic!("Internal Compiler Error: Invalid type {} for struct intializer", si.typ)
    }
}

unsafe fn gen_struct_initializer(ctx: &mut Context, si: &StructInitializer) -> ValueRef
{
    let var = ValueRef::alloc(ctx, &si.typ);
    gen_struct_initializer_store(ctx, si, &var);
    var
}

unsafe fn gen_member_access(ctx: &Context, sma: &MemberAccess) -> ValueRef
{
    let mut var = ctx.get_variable(&sma.name).expect("Internal Compiler Error: Unknown variable").value.clone();
    for at in &sma.access_types
    {
        var = var.member(ctx, at);
    }
    var
}

unsafe fn gen_block(ctx: &mut Context, block: &Block) -> ValueRef
{
    ctx.push_stack(ptr::null_mut());
    let mut ret = None;
    for e in &block.expressions {
        ret = Some(gen_expression(ctx, e));
    }
    ctx.pop_stack();
    ret.expect("Block has no expressions")
}

unsafe fn gen_block_store(ctx: &mut Context, block: &Block, ptr: &mut ValueRef)
{
    ctx.push_stack(ptr::null_mut());
    for (idx, e) in block.expressions.iter().enumerate() {
        if idx == block.expressions.len() - 1 {
            gen_expression_store(ctx, e, ptr);
        } else {
            gen_expression(ctx, e);
        }
    }
    ctx.pop_stack();
}

pub unsafe fn gen_expression_store(ctx: &mut Context, e: &Expression, ptr: &mut ValueRef)
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
            gen_match_store(ctx, &match_expr, ptr);
        },
        _ => {
            store(ctx, e, ptr)
        },
    }
}

pub fn gen_expression(ctx: &mut Context, e: &Expression) -> ValueRef
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
                    v = Some(gen_let_binding(ctx, b));
                }
                v.expect("Empting binding list")
            },
            Expression::If(ref i) => {
                let match_expr = i.to_match();
                gen_match(ctx, &match_expr)
            },
            Expression::Block(ref b) => gen_block(ctx, b),
            Expression::Literal(Literal::Int(_, v)) => gen_integer(ctx, v),
            Expression::Literal(Literal::Float(_, ref v_str)) => gen_float(ctx, &v_str),
            Expression::Literal(Literal::String(_, ref s))  => gen_string_literal(ctx, s),
            Expression::Literal(Literal::Bool(_, v)) => gen_bool(ctx, v),
            Expression::Literal(Literal::Char(_, v)) => gen_char(ctx, v),
            Expression::StructInitializer(ref si) => gen_struct_initializer(ctx, si),
            Expression::MemberAccess(ref sma) => gen_member_access(ctx, sma),
        }
    }
}
