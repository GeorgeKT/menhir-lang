use std::ptr;
use std::rc::Rc;
use std::ffi::CString;
use libc;
use llvm::core::*;
use llvm::prelude::*;
use llvm::*;

use ast::*;
use codegen::*;
use llrep::*;
use parser::Operator;

pub unsafe fn const_int(ctx: &Context, v: u64) -> LLVMValueRef
{
    LLVMConstInt(LLVMInt64TypeInContext(ctx.context), v, 0)
}

pub unsafe fn const_bool(ctx: &Context, v: bool) -> LLVMValueRef
{
    LLVMConstInt(LLVMInt1TypeInContext(ctx.context), if v {1} else {0}, 0)
}

unsafe fn gen_float(ctx: &Context, num: &str, dst: &ValueRef)
{
    match num.parse::<f64>()
    {
        Ok(f) => dst.store_direct(ctx, LLVMConstReal(LLVMDoubleTypeInContext(ctx.context), f)),
        Err(_) => panic!("Internal Compiler Error: {} is not a valid floating point number", num)
    }
}

unsafe fn gen_array_literal_store(ctx: &Context, elements: &Vec<LLVar>, array: &mut Array)
{
    let len = const_int(ctx, elements.len() as u64);
    array.init(ctx, len);
    for (idx, element) in elements.iter().enumerate()
    {
        let element_var = ctx.get_variable(&element.name).expect("Unknown variable").value.load(ctx.builder);
        let index = const_int(ctx, idx as u64);
        let el_ptr = array.get_element(ctx, index);
        el_ptr.store_direct(ctx, element_var);
    }
}


unsafe fn gen_literal(ctx: &Context, lit: &LLLiteral, dst: &mut ValueRef)
{
    match *lit
    {
        LLLiteral::Int(v) => dst.store_direct(ctx, const_int(ctx, v)),
        LLLiteral::Float(ref v) => gen_float(ctx, v, dst),
        LLLiteral::Char(v) => dst.store_direct(ctx, LLVMConstInt(LLVMInt8TypeInContext(ctx.context), v as u64, 0)),
        LLLiteral::String(ref v) => {
            panic!("NYI");
        },
        LLLiteral::Bool(v) => dst.store_direct(ctx, const_bool(ctx, v)),
        LLLiteral::Array(ref vars) => {
            if let ValueRef::Array(ref mut array) = *dst {
                gen_array_literal_store(ctx, vars, array);
            } else {
                panic!("Internal Compiler Error: Expecting array ValueRef");
            }
        },
    }
}

unsafe fn gen_int_bin_op(ctx: &Context, l: LLVMValueRef, r: LLVMValueRef, op: Operator) -> LLVMValueRef
{
    match op
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
        _ => panic!("Internal Compiler Error: Operator {} is not supported on integers", op),
    }
}

unsafe fn gen_float_bin_op(ctx: &Context, l: LLVMValueRef, r: LLVMValueRef, op: Operator) -> LLVMValueRef
{
    match op
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
        _ => panic!("Internal Compiler Error: Operator {} is not supported on floats", op),
    }
}

unsafe fn gen_bool_bin_op(ctx: &Context, l: LLVMValueRef, r: LLVMValueRef, op: Operator) -> LLVMValueRef
{
    match op
    {
        Operator::And => LLVMBuildAnd(ctx.builder, l, r, cstr!("and")),
        Operator::Or => LLVMBuildOr(ctx.builder, l, r, cstr!("or")),
        _ => panic!("Internal Compiler Error: Operator {} is not supported on bools", op),
    }
}

unsafe fn gen_bin_op(ctx: &Context, left: &LLVar, right: &LLVar, op: Operator, dst: &ValueRef)
{
    let l = ctx.get_variable(&left.name).expect("Unknown variable").value.load(ctx.builder);
    let r = ctx.get_variable(&right.name).expect("Unknown variable").value.load(ctx.builder);
    match left.typ
    {
        Type::Int =>  dst.store_direct(ctx, gen_int_bin_op(ctx, l, r, op)),
        Type::Float =>  dst.store_direct(ctx, gen_float_bin_op(ctx, l, r, op)),
        Type::Bool => dst.store_direct(ctx, gen_bool_bin_op(ctx, l, r, op)),
        _ => panic!("NYI"),
    }
}

unsafe fn gen_unary_op(ctx: &Context,  target: &LLVar, op: Operator, dst: &ValueRef)
{
    let t = ctx.get_variable(&target.name).expect("Unknown variable").value.load(ctx.builder);
    match op
    {
        Operator::Sub => {
            dst.store_direct(ctx, LLVMBuildNeg(ctx.builder, t, cstr!("neg")));
        },
        Operator::Not => {
            dst.store_direct(ctx, LLVMBuildNot(ctx.builder, t, cstr!("not")));
        },
        _ => panic!("Internal Compiler Error: Operator {} is not a unary operator", op),
    }
}

unsafe fn gen_load(ctx: &Context, name: &str, typ: &Type, dst: &mut ValueRef)
{
    if let Some(vi) = ctx.get_variable(&name) {
        dst.store(ctx, &vi.value)
    } else if let Some(fi) = ctx.get_function(&name) {
        *dst = ValueRef::new(fi.function, typ);
    } else {
        match *typ
        {
            Type::Sum(ref st) => {
                let case_type_ptr = dst.case_type(ctx);
                let idx = st.index_of(&name).expect("Internal Compiler Error: cannot determine index of sum type case");
                case_type_ptr.store_direct(ctx, const_int(ctx, idx as u64));
            },
            Type::Enum(ref et) => {
                let idx = et.index_of(&name).expect("Internal Compiler Error: cannot determine index of sum type case");
                dst.store_direct(ctx, const_int(ctx, idx as u64));
            },
            _ => {
                panic!("Internal Compiler Error: Unknown name {}", name)
            }
        }
    }
}


unsafe fn gen_call_args(ctx: &Context, args: &Vec<LLVar>, func: &FunctionInstance) -> Vec<LLVMValueRef>
{
    let mut arg_vals = Vec::with_capacity(func.args.len());
    for arg in args.iter()
    {
        let var = ctx.get_variable(&arg.name).expect("Unknown variable");
        if arg.typ.pass_by_ptr() {
            arg_vals.push(var.value.get())
        } else {
            arg_vals.push(var.value.load(ctx.builder))
        }
    }
    arg_vals
}

unsafe fn gen_call(ctx: &Context, name: &str, args: &Vec<LLVar>, dst: &ValueRef)
{
    let func = ctx.get_function(&name).expect("Internal Compiler Error: Unknown function");
    let mut arg_vals = gen_call_args(ctx, args, &func);
    let ret = LLVMBuildCall(ctx.builder, func.function, arg_vals.as_mut_ptr(), arg_vals.len() as libc::c_uint, cstr!("ret"));
    if func.sig.return_type.return_by_ptr()
    {
        panic!("NYI");
    }
    else
    {
        dst.store_direct(ctx, ret);
    }
}

unsafe fn gen_expr(ctx: &Context, typ: &Type, expr: &LLExpr, dst: &mut ValueRef)
{
    match *expr
    {
        LLExpr::Literal(ref l) => gen_literal(ctx, l, dst),
        LLExpr::Ref(ref v) => panic!("NYI"),
        LLExpr::Add(ref a, ref b) => gen_bin_op(ctx, a, b, Operator::Add, dst),
        LLExpr::Sub(ref a, ref b) => gen_bin_op(ctx, a, b, Operator::Sub, dst),
        LLExpr::Mul(ref a, ref b) => gen_bin_op(ctx, a, b, Operator::Mul, dst),
        LLExpr::Div(ref a, ref b) => gen_bin_op(ctx, a, b, Operator::Div, dst),
        LLExpr::Mod(ref a, ref b) => gen_bin_op(ctx, a, b, Operator::Mod, dst),
        LLExpr::And(ref a, ref b) => gen_bin_op(ctx, a, b, Operator::And, dst),
        LLExpr::Or(ref a, ref b) => gen_bin_op(ctx, a, b, Operator::Or, dst),
        LLExpr::LT(ref a, ref b) => gen_bin_op(ctx, a, b, Operator::LessThan, dst),
        LLExpr::LTE(ref a, ref b) => gen_bin_op(ctx, a, b, Operator::LessThanEquals, dst),
        LLExpr::GT(ref a, ref b) => gen_bin_op(ctx, a, b, Operator::GreaterThan, dst),
        LLExpr::GTE(ref a, ref b) => gen_bin_op(ctx, a, b, Operator::GreaterThanEquals, dst),
        LLExpr::EQ(ref a, ref b) => gen_bin_op(ctx, a, b, Operator::Equals, dst),
        LLExpr::NEQ(ref a, ref b) => gen_bin_op(ctx, a, b, Operator::NotEquals, dst),
        LLExpr::USub(ref v) => gen_unary_op(ctx, v, Operator::Sub, dst),
        LLExpr::Not(ref v) => gen_unary_op(ctx, v, Operator::Not, dst),
        LLExpr::Load(ref name) => gen_load(ctx, name, typ, dst),
        LLExpr::Call{ref name, ref args} => gen_call(ctx, name, args, dst),
    }
}

pub unsafe fn gen_instruction(ctx: &mut Context, instr: &LLInstruction)
{
    match *instr
    {
        LLInstruction::Set{ref var, ref expr} => {
            let mut v = ValueRef::new(
                ctx.stack_alloc(ctx.resolve_type(&var.typ), &var.name),
                &var.typ
            );
            gen_expr(ctx, &var.typ, expr, &mut v);
            ctx.add_variable(&var.name, v.clone());
        },

        LLInstruction::Return(ref var) => {
            let ret = ctx.get_variable(&var.name).expect("Unknown variable");
            LLVMBuildRet(ctx.builder, ret.value.load(ctx.builder));
        },

        LLInstruction::ReturnVoid => {
            LLVMBuildRetVoid(ctx.builder);
        },

        LLInstruction::StartScope => {
            ctx.push_stack(ptr::null_mut());
        },

        LLInstruction::EndScope{ref ret_var} => {
            let var = ctx.get_variable(&ret_var.name).expect("Unknown variable");
            ctx.pop_stack();
            ctx.add_variable_instance(var);
        },

        LLInstruction::Bind{ref name, ref var} => {
            let var = ctx.get_variable(&var.name).expect("Unknown variable");
            ctx.add_variable(name, var.value.clone());
        },
    }
}
