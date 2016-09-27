use std::ptr;
use std::rc::Rc;
use std::ffi::CString;
use std::collections::HashMap;
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

unsafe fn const_float(ctx: &Context, num: &str) -> LLVMValueRef
{
    match num.parse::<f64>()
    {
        Ok(f) => LLVMConstReal(LLVMDoubleTypeInContext(ctx.context), f),
        Err(_) => panic!("Internal Compiler Error: {} is not a valid floating point number", num)
    }
}

unsafe fn gen_array_literal(ctx: &Context, elements: &Vec<LLVar>, array: &Array)
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

unsafe fn gen_const_string_literal(ctx: &Context, s: &str) -> ValueRef
{
    ValueRef::Const(LLVMConstStringInContext(ctx.context, s.as_ptr() as *const i8, s.len() as u32, 1))
}

unsafe fn gen_string_literal(ctx: &Context, s: &str, array: &Array)
{
    let glob = LLVMAddGlobal(ctx.module, LLVMArrayType(LLVMInt8TypeInContext(ctx.context), s.len() as libc::c_uint),  cstr!("string"));

    LLVMSetLinkage(glob, LLVMLinkage::LLVMInternalLinkage);
    LLVMSetGlobalConstant(glob, 1);
    let string_data = gen_const_string_literal(ctx, s);
    LLVMSetInitializer(glob, string_data.get());
    array.fill_with_string_literal(ctx, glob, s.len())
}

unsafe fn gen_literal(ctx: &mut Context, dst: &LLVar, lit: &LLLiteral)
{
    let dst_vr = get_value_ref(ctx, dst);
    match *lit
    {
        LLLiteral::Int(v) => dst_vr.store_direct(ctx, const_int(ctx, v)),
        LLLiteral::Float(ref v) => dst_vr.store_direct(ctx, const_float(ctx, v)),
        LLLiteral::Char(v) => dst_vr.store_direct(ctx, LLVMConstInt(LLVMInt8TypeInContext(ctx.context), v as u64, 0)),
        LLLiteral::Bool(v) => dst_vr.store_direct(ctx, const_bool(ctx, v)),

        LLLiteral::String(ref v) => {
            if let ValueRef::Array(ref array) = dst_vr {
                gen_string_literal(ctx, v, array)
            } else {
                panic!("Internal Compiler Error: Expecting array ValueRef");
            }
        },

        LLLiteral::Array(ref vars) => {
            if let ValueRef::Array(ref array) = dst_vr {
                gen_array_literal(ctx, vars, array);
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
        Operator::Equals => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntEQ, l, r, cstr!("cmp")),
        Operator::NotEquals => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntNE, l, r, cstr!("cmp")),
        _ => panic!("Internal Compiler Error: Operator {} is not supported on bools", op),
    }
}

unsafe fn gen_array_bin_op(ctx: &mut Context, dst: &LLVar, left: &LLVar, right: &LLVar, op: Operator)
{
    match op {
        Operator::Add => {
            let l = &ctx.get_variable(&left.name).expect("Unknown variable").value;
            let r = &ctx.get_variable(&right.name).expect("Unknown variable").value;
            let dst = get_value_ref(ctx, dst);
            match (l, r, &dst)
            {
                (&ValueRef::Array(ref ar), &ValueRef::Array(ref br), &ValueRef::Array(ref dr)) => {
                    Array::concat_store(ctx, ar, br, dr);
                }
                _ => panic!("Internal Compiler Error: Operator {} expects two arrays", op),
            }
        },
        _ => panic!("Internal Compiler Error: Operator {} is not supported on arrays", op),
    }
}

unsafe fn gen_bin_op(ctx: &mut Context, dst: &LLVar, left: &LLVar, right: &LLVar, op: Operator)
{
    if let Type::Array(_) = dst.typ {
        return gen_array_bin_op(ctx, dst, left, right, op);
    }

    let l = ctx.get_variable(&left.name).expect("Unknown variable").value.load(ctx.builder);
    let r = ctx.get_variable(&right.name).expect("Unknown variable").value.load(ctx.builder);
    let dst_vr = get_value_ref(ctx, dst);
    dst_vr.store_direct(ctx, match dst.typ
    {
        Type::Int =>  {
            gen_int_bin_op(ctx, l, r, op)
        },
        Type::Float => {
            gen_float_bin_op(ctx, l, r, op)
        },
        Type::Bool => {
            gen_bool_bin_op(ctx, l, r, op)
        }
        _ => panic!("Internal Compiler Error: Binary operator {} not support on type {}", op, dst.typ),
    });
}

unsafe fn gen_unary_op(ctx: &mut Context, dst: &LLVar, target: &LLVar, op: Operator)
{
    let t = ctx.get_variable(&target.name).expect("Unknown variable").value.load(ctx.builder);
    let dst = get_value_ref(ctx, dst);
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

/*
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
*/

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

unsafe fn gen_call(ctx: &mut Context, dst: &LLVar, name: &str, args: &Vec<LLVar>)
{
    let func = ctx.get_function(&name).expect("Internal Compiler Error: Unknown function");
    let mut arg_vals = gen_call_args(ctx, args, &func);
    let dst = get_value_ref(ctx, dst);
    if func.sig.return_type.return_by_ptr()
    {
        arg_vals.push(dst.get());
        LLVMBuildCall(ctx.builder, func.function, arg_vals.as_mut_ptr(), arg_vals.len() as libc::c_uint, cstr!(""));
    }
    else
    {
        let ret = LLVMBuildCall(ctx.builder, func.function, arg_vals.as_mut_ptr(), arg_vals.len() as libc::c_uint, cstr!("ret"));
        dst.store_direct(ctx, ret);
    }
}

unsafe fn gen_struct_member(ctx: &mut Context, dst: &LLVar, obj: &LLVar, index: usize)
{
    let struct_object = ctx.get_variable(&obj.name).expect("Unknown variable");
    let member_ptr = struct_object.value.member(ctx, &MemberAccessType::StructMember(index));
    ctx.add_variable(&dst.name, member_ptr);
}

unsafe fn gen_array_property(ctx: &mut Context, dst: &LLVar, array: &LLVar, property: ArrayProperty)
{
    let array_object = &ctx.get_variable(&array.name).expect("Unknown variable").value;
    let prop_ptr = array_object.member(ctx, &MemberAccessType::ArrayProperty(property));
    ctx.add_variable(&dst.name, prop_ptr);
}

unsafe fn get_value_ref(ctx: &mut Context, var: &LLVar) -> ValueRef
{
    if let Some(ref vr) = ctx.get_variable(&var.name) {
        vr.value.clone()
    } else {
        let vr = ValueRef::alloc(ctx, &var.typ);
        ctx.add_variable(&var.name, vr.clone());
        vr
    }
}

unsafe fn gen_expr(ctx: &mut Context, dst: &LLVar, expr: &LLExpr)
{
    match *expr
    {
        LLExpr::Literal(ref l) => gen_literal(ctx, dst, l),
        LLExpr::Add(ref a, ref b) => gen_bin_op(ctx, dst, a, b, Operator::Add),
        LLExpr::Sub(ref a, ref b) => gen_bin_op(ctx, dst, a, b, Operator::Sub),
        LLExpr::Mul(ref a, ref b) => gen_bin_op(ctx, dst, a, b, Operator::Mul),
        LLExpr::Div(ref a, ref b) => gen_bin_op(ctx, dst, a, b, Operator::Div),
        LLExpr::Mod(ref a, ref b) => gen_bin_op(ctx, dst, a, b, Operator::Mod),
        LLExpr::And(ref a, ref b) => gen_bin_op(ctx, dst, a, b, Operator::And),
        LLExpr::Or(ref a, ref b) => gen_bin_op(ctx, dst, a, b, Operator::Or),
        LLExpr::LT(ref a, ref b) => gen_bin_op(ctx, dst, a, b, Operator::LessThan),
        LLExpr::LTE(ref a, ref b) => gen_bin_op(ctx, dst, a, b, Operator::LessThanEquals),
        LLExpr::GT(ref a, ref b) => gen_bin_op(ctx, dst, a, b, Operator::GreaterThan),
        LLExpr::GTE(ref a, ref b) => gen_bin_op(ctx, dst, a, b, Operator::GreaterThanEquals),
        LLExpr::EQ(ref a, ref b) => gen_bin_op(ctx, dst, a, b, Operator::Equals),
        LLExpr::NEQ(ref a, ref b) => gen_bin_op(ctx, dst, a, b, Operator::NotEquals),
        LLExpr::USub(ref v) => gen_unary_op(ctx, dst, v, Operator::Sub),
        LLExpr::Not(ref v) => gen_unary_op(ctx, dst, v, Operator::Not),
        LLExpr::Call(ref name, ref args) => gen_call(ctx, dst, name, args),
        LLExpr::StructMember(ref obj, index) => gen_struct_member(ctx, dst, obj, index),
        LLExpr::ArrayProperty(ref array, ref property) => gen_array_property(ctx, dst, array, property.clone()),
        LLExpr::ArrayHead(ref array) => panic!("NYI"),
        LLExpr::ArrayTail(ref array) => panic!("NYI"),
        LLExpr::SumTypeIndex(ref obj) => panic!("NYI"),
        LLExpr::SumTypeStruct(ref obj, index) => panic!("NYI"),
    }
}

pub unsafe fn gen_instruction(ctx: &mut Context, instr: &LLInstruction, blocks: &HashMap<LLBasicBlockRef, LLVMBasicBlockRef>)
{
    match *instr
    {
        LLInstruction::NOP => {},

        LLInstruction::Set(ref s) => {
            gen_expr(ctx, &s.var, &s.expr);
        },

        LLInstruction::SetStructMember(ref s) => {
            let struct_object = ctx.get_variable(&s.obj.name).expect("Unknown variable obj");
            let member_ptr = struct_object.value.member(ctx, &MemberAccessType::StructMember(s.member_index));
            member_ptr.store(ctx, &ctx.get_variable(&s.value.name).expect("Unknown variable value").value);
        },

        LLInstruction::StackAlloc(ref var) => {
            let v = ValueRef::new(
                ctx.stack_alloc(ctx.resolve_type(&var.typ), &var.name),
                &var.typ
            );
            ctx.add_variable(&var.name, v);
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

        LLInstruction::EndScope(ref ret_var) => {
            let var = ctx.get_variable(&ret_var.name).expect("Unknown variable");
            ctx.pop_stack();
            ctx.add_variable_instance(var);
        },

        LLInstruction::Bind(ref b) => {
            let var = ctx.get_variable(&b.var.name).expect("Unknown variable");
            ctx.add_variable(&b.name, var.value.clone());
        },

        LLInstruction::Branch(ref bb) => {
            let llvm_bb = blocks.get(bb).expect("Unknown basic block");
            LLVMBuildBr(ctx.builder, *llvm_bb);
        },

        LLInstruction::BranchIf(ref b) => {
            let on_true_bb = blocks.get(&b.on_true).expect("Unknown basic block");
            let on_false_bb = blocks.get(&b.on_false).expect("Unknown basic block");
            let cond = ctx.get_variable(&b.cond.name).expect("Unknown variable");
            LLVMBuildCondBr(ctx.builder, cond.value.load(ctx.builder), *on_true_bb, *on_false_bb);
        },
    }
}
