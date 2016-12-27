mod compiler;
mod function;
mod instruction;
mod interpreter;

use std::fmt;
use ast::*;
use parser::Operator;
pub use self::compiler::*;
pub use self::function::*;
pub use self::instruction::*;
pub use self::interpreter::*;

pub struct ByteCodeModule
{
    pub name: String,
    pub functions: Vec<ByteCodeFunction>,
}

impl fmt::Display for ByteCodeModule
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error>
    {
        for func in &self.functions {
            func.fmt(f)?;
            writeln!(f, " ")?;
        }
        Ok(())
    }
}

/*
fn add_set(func: &mut ByteCodeFunction, expr: ByteCodeExpression, dst: &Var)
{
    func.add(set_instr(dst, expr));
}

fn make_var(func: &mut ByteCodeFunction, expr: ByteCodeExpression, typ: Type) -> Var
{
    let var = func.new_var(typ);
    add_set(func, expr, &var);
    var
}

fn add_lit(func: &mut ByteCodeFunction, lit: ByteCodeLiteral, dst: &Var)
{
    add_set(func, ByteCodeExpression::Literal(lit), dst);
}

fn make_lit(func: &mut ByteCodeFunction, lit: ByteCodeLiteral, typ: Type) -> Var
{
    let var = func.new_var(typ);
    add_set(func, ByteCodeExpression::Literal(lit), &var);
    var
}

fn bind(func: &mut ByteCodeFunction, name: &str, var: &Var)
{
    func.add(bind_instr(name, var))
}



fn add_binding(func: &mut ByteCodeFunction, b: &LetBinding)
{
    match b.binding_type
    {
        LetBindingType::Name(ref name) => {
            let dst = stack_alloc(func, &b.typ, Some(name));
            func.push_destination(Some(dst));
            expr_to_bc(func, &b.init);
            func.pop_destination();
        },

        LetBindingType::Struct(ref s) => {
            let dst = stack_alloc(func, &b.typ, None);
            func.push_destination(Some(dst.clone()));
            expr_to_bc(func, &b.init);
            add_struct_pattern_bindings(s, &dst, func);
            func.pop_destination();
        },
    }
}

fn let_to_bc(func: &mut ByteCodeFunction, l: &LetExpression) -> Option<Var>
{
    let dst = get_dst(func, &l.typ);
    func.push_scope();
    for b in &l.bindings{
        add_binding(func, b);
    }

    func.push_destination(Some(dst.clone()));
    to_bc(func, &l.expression);
    func.pop_destination();
    func.pop_scope();
    Some(dst)
}


fn add_array_len(func: &mut ByteCodeFunction, array: Var, dst: &Var)
{
    let expr = ByteCodeExpression::ArrayProperty(array, ArrayProperty::Len);
    add_set(func, expr, &dst);
}

fn make_array_len(func: &mut ByteCodeFunction, array: Var) -> Var
{
    let var = func.new_var(Type::Int);
    add_array_len(func, array, &var);
    var
}

fn member_access_to_bc(func: &mut ByteCodeFunction, sma: &MemberAccess, dst: &Var)
{
    func.push_destination(None);
    let var = to_bc(func, &sma.left);
    func.pop_destination();

    let var_typ = if let Type::Pointer(ref inner) = var.typ {
        &inner
    } else {
        &var.typ
    };

    match (var_typ, &sma.right)
    {
        (&Type::Struct(_), &MemberAccessType::Name(ref field)) => {
            let expr = ByteCodeExpression::StructMember(var.clone(), field.index);
            add_set(func, expr, dst);
        },

        (&Type::Array(_), &MemberAccessType::ArrayProperty(ArrayProperty::Len)) => {
            add_array_len(func, var.clone(), dst);
        },

        _ => {
            panic!("Internal Compiler Error: Invalid member access")
        },
    }
}


fn name_pattern_match_to_bc(
    func: &mut ByteCodeFunction,
    mc: &MatchCase,
    target: &Var,
    match_end_bb: BasicBlockRef,
    match_case_bb: BasicBlockRef,
    next_bb: BasicBlockRef,
    nr: &NameRef)
{
    match nr.typ
    {
        Type::Enum(ref et) => {
            let idx = et.index_of(&nr.name).expect("Internal Compiler Error: cannot determine index of sum type case");
            let cv = make_lit(func, ByteCodeLiteral::Int(idx as u64), Type::Int);
            let cond = make_var(func, ByteCodeExpression::BinaryOp(Operator::Equals, target.clone(), cv), Type::Bool);
            func.add(branch_if_instr(&cond, match_case_bb, next_bb));
        },
        Type::Sum(ref st) => {
            let idx = st.index_of(&nr.name).expect("Internal Compiler Error: cannot determine index of sum type case");
            let cv = make_lit(func, ByteCodeLiteral::Int(idx as u64), Type::Int);
            let sum_type_index = make_var(func, ByteCodeExpression::SumTypeIndex(target.clone()), Type::Int);
            let cond = make_var(func,  ByteCodeExpression::BinaryOp(Operator::Equals, sum_type_index, cv), Type::Bool);
            func.add(branch_if_instr(&cond, match_case_bb, next_bb));
        },
        _ => {
            panic!("Internal Compiler Error: Expression is not a valid match pattern");
        }
    }

    match_case_body_to_bc(func, mc, match_case_bb, match_end_bb, next_bb);
}


fn match_case_body_to_bc(
    func: &mut ByteCodeFunction,
    mc: &MatchCase,
    match_case_bb: BasicBlockRef,
    match_end_bb: BasicBlockRef,
    next_bb: BasicBlockRef)
{
    func.set_current_bb(match_case_bb);
    expr_to_bc(func, &mc.to_execute);
    func.add(Instruction::Branch(match_end_bb));
    func.set_current_bb(next_bb);
}

fn array_pattern_match_to_bc(
    func: &mut ByteCodeFunction,
    ap: &ArrayPattern,
    seq: &Var,
    match_case_bb: BasicBlockRef,
    next_bb: BasicBlockRef)
{
    let head = make_var(func, ByteCodeExpression::ArrayHead(seq.clone()), seq.typ.get_element_type().expect("Invalid array type"));
    bind(func, &ap.head, &head);
    let tail = make_var(func, ByteCodeExpression::ArrayTail(seq.clone()), seq.typ.clone());
    bind(func, &ap.tail, &tail);

    let length = make_array_len(func, seq.clone());
    let zero = make_lit(func, ByteCodeLiteral::Int(0), Type::Int);
    let cond = make_var(func, ByteCodeExpression::BinaryOp(Operator::GreaterThan, length, zero), Type::Bool);
    func.add(branch_if_instr(&cond, match_case_bb, next_bb));
}

fn add_struct_pattern_bindings(p: &StructPattern, struct_var: &Var, func: &mut ByteCodeFunction)
{
    for (idx, b) in p.bindings.iter().enumerate() {
        if b != "_" {
            let expr = ByteCodeExpression::StructMember(struct_var.clone(), idx);
            let member_ptr = make_var(func, expr, p.types[idx].clone());
            bind(func, b, &member_ptr);
        }
    }
}

fn struct_pattern_match_to_bc(
    func: &mut ByteCodeFunction,
    mc: &MatchCase,
    target: &Var,
    match_end_bb: BasicBlockRef,
    match_case_bb: BasicBlockRef,
    next_bb: BasicBlockRef,
    p: &StructPattern)
{
    func.push_destination(None);

    match p.typ
    {
        Type::Struct(_) => {
            func.add(Instruction::Branch(match_case_bb));
            func.set_current_bb(match_case_bb);
            add_struct_pattern_bindings(p, target, func);
        },
        Type::Sum(ref st) => {
            let target_sum_type_index = make_var(func, ByteCodeExpression::SumTypeIndex(target.clone()), Type::Int);
            let idx = st.index_of(&p.name).expect("Internal Compiler Error: cannot determine index of sum type case");
            let sum_type_index = make_lit(func, ByteCodeLiteral::Int(idx as u64), Type::Int);
            let cond = make_var(func, ByteCodeExpression::BinaryOp(Operator::Equals, target_sum_type_index, sum_type_index), Type::Bool);
            func.add(branch_if_instr(&cond, match_case_bb, next_bb));

            func.set_current_bb(match_case_bb);
            let struct_ptr = make_var(func, ByteCodeExpression::SumTypeStruct(target.clone(), idx), st.cases[idx].typ.clone());
            add_struct_pattern_bindings(p, &struct_ptr, func);
        },
        _ => panic!("Internal Compiler Error: Expression is not a valid match pattern"),
    }

    func.pop_destination();
    match_case_body_to_bc(func, mc, match_case_bb, match_end_bb, next_bb);
}

fn match_case_to_bc(func: &mut ByteCodeFunction, mc: &MatchCase, target: &Var, match_end_bb: BasicBlockRef)
{
    let match_case_bb = func.create_basic_block();
    func.add_basic_block(match_case_bb);
    let next_bb = func.create_basic_block();
    func.add_basic_block(next_bb);

    let add_literal_case = |func: &mut ByteCodeFunction, lit: ByteCodeLiteral, typ: Type| {
        func.push_destination(None);
        let iv = make_lit(func, lit, typ);
        let cond = make_var(func, ByteCodeExpression::BinaryOp(Operator::Equals, iv, target.clone()), Type::Bool);
        func.add(branch_if_instr(&cond, match_case_bb, next_bb));
        func.pop_destination();
        match_case_body_to_bc(func, mc, match_case_bb, match_end_bb, next_bb);
    };

    match mc.pattern
    {
        Pattern::Literal(Literal::Int(_, v)) => {
            add_literal_case(func, ByteCodeLiteral::Int(v), Type::Int);
        },

        Pattern::Literal(Literal::Float(_, ref v)) => {
            add_literal_case(func, ByteCodeLiteral::Float(v.clone()), Type::Float);
        },

        Pattern::Literal(Literal::Bool(_, v)) => {
            add_literal_case(func, ByteCodeLiteral::Bool(v), Type::Bool);
        },

        Pattern::Literal(Literal::Char(_, v)) => {
            add_literal_case(func, ByteCodeLiteral::Char(v), Type::Char);
        },

        Pattern::Name(ref nr) => {
            name_pattern_match_to_bc(func, mc, target, match_end_bb, match_case_bb, next_bb, nr)
        },

        Pattern::Any(_) => {
            func.add(Instruction::Branch(match_case_bb));
            match_case_body_to_bc(func, mc, match_case_bb, match_end_bb, next_bb);
        },

        Pattern::EmptyArray(_) => {
            match target.typ
            {
                Type::Array(_) => {
                    func.push_destination(None);
                    let length = make_array_len(func, target.clone());
                    let zero = make_lit(func, ByteCodeLiteral::Int(0), Type::Int);
                    let cond = make_var(func, ByteCodeExpression::BinaryOp(Operator::Equals, length, zero), Type::Bool);
                    func.add(branch_if_instr(&cond, match_case_bb, next_bb));
                    func.pop_destination();
                    match_case_body_to_bc(func, mc, match_case_bb, match_end_bb, next_bb);
                },
                _ => panic!("Internal Compiler Error: Match expression cannot be matched with an array pattern"),
            }
        },

        Pattern::Array(ref ap) => {
            match target.typ
            {
                Type::Array(_) => {
                    func.push_destination(None);
                    array_pattern_match_to_bc(func, ap, target, match_case_bb, next_bb);
                    func.pop_destination();
                },
                _ => panic!("Internal Compiler Error: Match expression cannot be matched with an array pattern"),
            }

            match_case_body_to_bc(func, mc, match_case_bb, match_end_bb, next_bb);
        },

        Pattern::Literal(Literal::Array(ref a)) => {
            func.push_destination(None);
            let arr = func.new_var(a.array_type.clone());
            array_lit_to_bc(func, a, &arr);
            let cond = make_var(func, ByteCodeExpression::BinaryOp(Operator::Equals, arr, target.clone()), Type::Bool);
            func.add(branch_if_instr(&cond, match_case_bb, next_bb));
            func.pop_destination();
            match_case_body_to_bc(func, mc, match_case_bb, match_end_bb, next_bb);
        },

        Pattern::Literal(Literal::String(_, ref s)) => {
            func.push_destination(None);
            let arr = make_lit(func, ByteCodeLiteral::String(s.clone()), string_type());
            let cond = make_var(func, ByteCodeExpression::BinaryOp(Operator::Equals, arr, target.clone()), Type::Bool);
            func.add(branch_if_instr(&cond, match_case_bb, next_bb));
            func.pop_destination();
            match_case_body_to_bc(func, mc, match_case_bb, match_end_bb, next_bb);
        },

        Pattern::Struct(ref p) => {
            struct_pattern_match_to_bc(func, mc, target, match_end_bb, match_case_bb, next_bb, p);
        }
    }
}

fn match_to_bc(func: &mut ByteCodeFunction, m: &MatchExpression) -> Var
{
    func.push_destination(None);
    let target_var = to_bc(func, &m.target);
    func.pop_destination();
    let match_end_bb = func.create_basic_block();

    let dst = get_dst(func, &m.typ);
    func.push_scope();
    func.push_destination(Some(dst.clone()));
    for mc in &m.cases {
        match_case_to_bc(func, mc, &target_var, match_end_bb);
    }
    func.pop_destination();

    func.add(Instruction::Branch(match_end_bb));
    func.add_basic_block(match_end_bb);
    func.set_current_bb(match_end_bb);
    func.pop_scope();
    dst
}


fn name_ref_to_bc(func: &mut ByteCodeFunction, nr: &NameRef) -> Option<Var>
{
    let add_name_ref = |func: &mut ByteCodeFunction, nr: &NameRef| {
        let v = Var::named(&nr.name, nr.typ.clone());
        match func.get_destination()
        {
            Some(var) => {
                assert!(var.typ == v.typ);
                add_set(func, ByteCodeExpression::Ref(v), &var);
                Some(var)
            },
            None => Some(v),
        }
    };

    match nr.typ
    {
        Type::Sum(ref st) => {
            if let Some(idx) = st.index_of(&nr.name) {
                let dst = get_dst(func, &nr.typ);
                add_set(func, ByteCodeExpression::SumTypeCase(idx), &dst);
                Some(dst)
            } else {
                add_name_ref(func, nr)
            }
        },
        Type::Enum(ref et) => {
            if let Some(idx) = et.index_of(&nr.name) {
                // enums are integers
                let dst = get_dst(func, &nr.typ);
                add_lit(func, ByteCodeLiteral::Int(idx as u64), &dst);
                Some(dst)
            } else {
                add_name_ref(func, nr)
            }
        },
        _ => {
            add_name_ref(func, nr)
        }
    }
}





*/
