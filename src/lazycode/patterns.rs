use crate::ast::{
    ptr_type, slice_type, ArrayLiteral, ArrayPattern, BinaryOperator, Literal, NameRef, OptionalPattern, Pattern,
    StructPattern, SumTypeCaseIndexOf, Type,
};
use crate::target::Target;

use super::compiler::expr_to_bc;
use super::function::ByteCodeFunction;
use super::instruction::{branch_if_instr, branch_instr, BasicBlockRef};
use super::operand::{Constant, Operand};
use super::ByteCodeModule;

fn literal_match_to_bc(
    func: &mut ByteCodeFunction,
    match_target: &Operand,
    lit: Operand,
    match_case_bb: BasicBlockRef,
    next_bb: BasicBlockRef,
) {
    let cond = Operand::binary(BinaryOperator::Equals, lit, match_target.clone(), Type::Bool);
    func.add(branch_if_instr(cond, match_case_bb, next_bb));
}

fn name_pattern_match_to_bc(
    func: &mut ByteCodeFunction,
    match_target: &Operand,
    match_case_bb: BasicBlockRef,
    next_bb: BasicBlockRef,
    nr: &NameRef,
    target_machine: &Target,
) {
    match &nr.typ {
        Type::Enum(et) => {
            let idx = et
                .index_of(&nr.name)
                .expect("Internal Compiler Error: cannot determine index of sum type case");
            let cond = Operand::binary(
                BinaryOperator::Equals,
                match_target.clone(),
                Operand::const_uint(idx as u64, target_machine.int_size),
                Type::Bool,
            );
            func.add(branch_if_instr(cond, match_case_bb, next_bb));
        }
        Type::Sum(st) => {
            let idx = st
                .index_of(&nr.name)
                .expect("Internal Compiler Error: cannot determine index of sum type case");
            let sum_type_index = Operand::sti(match_target.clone(), target_machine.int_size);
            let cond = Operand::binary(
                BinaryOperator::Equals,
                sum_type_index,
                Operand::const_uint(idx as u64, target_machine.int_size),
                Type::Bool,
            );
            func.add(branch_if_instr(cond, match_case_bb, next_bb));
        }
        _ => {
            panic!("Internal Compiler Error: Expression is not a valid match pattern");
        }
    }
}

fn binding_pattern_match_to_bc(
    func: &mut ByteCodeFunction,
    match_target: &Operand,
    match_case_bb: BasicBlockRef,
    nr: &NameRef,
) {
    func.declare(&nr.name, Some(match_target.clone()), nr.typ.clone());
    func.add(branch_instr(match_case_bb));
}

fn empty_array_pattern_to_bc(
    func: &mut ByteCodeFunction,
    match_target: &Operand,
    match_case_bb: BasicBlockRef,
    next_bb: BasicBlockRef,
    target_machine: &Target,
) {
    match match_target.get_type(target_machine.int_size) {
        Type::Array(_) | Type::Slice(_) => {
            let len = Operand::len(match_target.clone(), target_machine.int_size);
            let cond = Operand::binary(
                BinaryOperator::Equals,
                len,
                Operand::const_uint(0, target_machine.int_size),
                Type::Bool,
            );
            func.add(branch_if_instr(cond, match_case_bb, next_bb));
        }
        _ => panic!("Internal Compiler Error: Match expression cannot be matched with an empty array pattern"),
    }
}

fn array_pattern_match_to_bc(
    func: &mut ByteCodeFunction,
    ap: &ArrayPattern,
    seq: &Operand,
    match_case_bb: BasicBlockRef,
    next_bb: BasicBlockRef,
    target: &Target,
) {
    let head_type = seq
        .get_type(target.int_size)
        .get_element_type()
        .expect("Invalid array type");
    let head = Operand::member(
        seq.clone(),
        Operand::const_uint(0, target.int_size),
        ptr_type(head_type.clone()),
    );

    let _head = func.declare(&ap.head, Some(head), head_type.clone());

    let tail_type = slice_type(head_type.clone());
    let seq_len = Operand::len(seq.clone(), target.int_size);
    let tail_len = Operand::binary(
        BinaryOperator::Sub,
        seq_len.clone(),
        Operand::const_uint(1, target.int_size),
        target.native_uint_type.clone(),
    );
    let tail = Operand::slice(
        Operand::member(
            seq.clone(),
            Operand::const_uint(1, target.int_size),
            ptr_type(head_type),
        ),
        tail_len,
        tail_type.clone(),
    );

    func.declare(&ap.tail, Some(tail), tail_type);

    let cond = Operand::binary(
        BinaryOperator::GreaterThan,
        seq_len,
        Operand::const_uint(0, target.int_size),
        Type::Bool,
    );
    func.add(branch_if_instr(cond, match_case_bb, next_bb));
}

fn add_struct_pattern_bindings(p: &StructPattern, struct_var: &Operand, func: &mut ByteCodeFunction, target: &Target) {
    for (idx, b) in p.bindings.iter().enumerate() {
        if b.name == "_" {
            continue;
        }

        let member = Operand::member(
            struct_var.clone(),
            Operand::const_uint(idx as u64, target.int_size),
            ptr_type(b.typ.clone()),
        );
        func.declare(&b.name, Some(member), b.typ.clone());
    }
}

fn struct_pattern_match_to_bc(
    func: &mut ByteCodeFunction,
    match_target: &Operand,
    match_case_bb: BasicBlockRef,
    next_bb: BasicBlockRef,
    p: &StructPattern,
    target_machine: &Target,
) {
    match &p.typ {
        Type::Struct(_) => {
            func.add(branch_instr(match_case_bb));
            func.set_current_bb(match_case_bb);

            func.push_scope();
            add_struct_pattern_bindings(p, match_target, func, target_machine);
        }
        Type::Sum(st) => {
            let target_sum_type_index = Operand::sti(match_target.clone(), target_machine.int_size);
            let idx = st
                .index_of(&p.name)
                .expect("Internal Compiler Error: cannot determine index of sum type case");
            let cond = Operand::binary(
                BinaryOperator::Equals,
                target_sum_type_index,
                Operand::const_uint(idx as u64, target_machine.int_size),
                Type::Bool,
            );
            func.add(branch_if_instr(cond, match_case_bb, next_bb));

            func.set_current_bb(match_case_bb);

            func.push_scope();
            let struct_ptr = Operand::member(
                match_target.clone(),
                Operand::const_uint(idx as u64, target_machine.int_size),
                ptr_type(st.cases[idx].typ.clone()),
            );
            add_struct_pattern_bindings(p, &struct_ptr, func, target_machine);
        }
        _ => panic!("Internal Compiler Error: Expression is not a valid match pattern"),
    }
}

fn optional_pattern_match_to_bc(
    func: &mut ByteCodeFunction,
    match_target: &Operand,
    match_case_bb: BasicBlockRef,
    next_bb: BasicBlockRef,
    o: &OptionalPattern,
    target: &Target,
) {
    let sti = Operand::sti(match_target.clone(), target.int_size);
    let cond = Operand::binary(
        BinaryOperator::Equals,
        sti,
        Operand::const_uint(0, target.int_size),
        Type::Bool,
    );
    func.add(branch_if_instr(cond, match_case_bb, next_bb));

    func.set_current_bb(match_case_bb);
    let data = Operand::member(
        match_target.clone(),
        Operand::const_uint(0, target.int_size),
        ptr_type(o.inner_type.clone()),
    );
    func.declare(&o.binding, Some(data), ptr_type(o.inner_type.clone()));
}

fn nil_pattern_match_to_bc(
    func: &mut ByteCodeFunction,
    target: &Operand,
    match_case_bb: BasicBlockRef,
    next_bb: BasicBlockRef,
    target_machine: &Target,
) {
    let sti = Operand::sti(target.clone(), target_machine.int_size);
    let cond = Operand::binary(
        BinaryOperator::Equals,
        sti,
        Operand::const_uint(0, target_machine.int_size),
        Type::Bool,
    );
    func.add(branch_if_instr(cond, next_bb, match_case_bb));
}

fn result_pattern_match_to_bc(
    bc_mod: &mut ByteCodeModule,
    func: &mut ByteCodeFunction,
    target: &Operand,
    match_end_bb: BasicBlockRef,
    match_case_bb: BasicBlockRef,
    next_bb: BasicBlockRef,
    inner_type: &Type,
    inner_pattern: &Pattern,
    is_ok: bool,
    target_machine: &Target,
) {
    let index = if is_ok { 0 } else { 1 };
    let bind_bb = func.create_basic_block();

    let sti = Operand::sti(target.clone(), target_machine.int_size);
    let idx = Operand::const_int(index, target_machine.int_size);
    let cond = Operand::binary(BinaryOperator::Equals, sti, idx.clone(), Type::Bool);
    func.add(branch_if_instr(cond, bind_bb, next_bb));

    func.set_current_bb(bind_bb);
    let inner = Operand::member(target.clone(), idx, ptr_type(inner_type.clone()));
    pattern_to_bc(
        bc_mod,
        func,
        inner_pattern,
        &inner,
        match_case_bb,
        match_end_bb,
        next_bb,
        target_machine,
    );
}

fn array_lit_to_bc(
    bc_mod: &mut ByteCodeModule,
    func: &mut ByteCodeFunction,
    a: &ArrayLiteral,
    target: &Target,
) -> Operand {
    let mut members = Vec::new();
    for element in a.elements.iter() {
        let v = expr_to_bc(bc_mod, func, element, target);
        members.push(v);
    }
    Operand::Array {
        members,
        typ: a.array_type.clone(),
    }
}

pub fn pattern_to_bc(
    bc_mod: &mut ByteCodeModule,
    func: &mut ByteCodeFunction,
    pattern: &Pattern,
    match_target: &Operand,
    match_case_bb: BasicBlockRef,
    match_end_bb: BasicBlockRef,
    next_bb: BasicBlockRef,
    target_machine: &Target,
) {
    match pattern {
        Pattern::Literal(Literal::Int(_, v, int_size)) => {
            literal_match_to_bc(
                func,
                match_target,
                Operand::const_int(*v, *int_size),
                match_case_bb,
                next_bb,
            );
        }

        Pattern::Literal(Literal::UInt(_, v, int_size)) => {
            literal_match_to_bc(
                func,
                match_target,
                Operand::const_uint(*v, *int_size),
                match_case_bb,
                next_bb,
            );
        }

        Pattern::Literal(Literal::Float(_, v, float_size)) => {
            literal_match_to_bc(
                func,
                match_target,
                Operand::const_float(v.parse().expect("ICE: invalid float"), *float_size),
                match_case_bb,
                next_bb,
            );
        }

        Pattern::Literal(Literal::Bool(_, v)) => {
            literal_match_to_bc(func, match_target, Operand::const_bool(*v), match_case_bb, next_bb);
        }

        Pattern::Literal(Literal::Char(_, v)) => {
            literal_match_to_bc(func, match_target, Operand::const_char(*v), match_case_bb, next_bb);
        }

        Pattern::Literal(Literal::NullPtr(_, inner_type)) => {
            literal_match_to_bc(
                func,
                match_target,
                Operand::Constant {
                    value: Constant::NullPtr(inner_type.clone()),
                },
                match_case_bb,
                next_bb,
            );
        }

        Pattern::Name(nr) => name_pattern_match_to_bc(func, match_target, match_case_bb, next_bb, nr, target_machine),

        Pattern::Binding(nr) => binding_pattern_match_to_bc(func, match_target, match_case_bb, nr),

        Pattern::Any(_) => {
            func.add(branch_instr(match_case_bb));
        }

        Pattern::EmptyArray(_) => empty_array_pattern_to_bc(func, match_target, match_case_bb, next_bb, target_machine),
        Pattern::Array(ap) => match &match_target.get_type(target_machine.int_size) {
            Type::Array(_) | Type::Slice(_) => {
                array_pattern_match_to_bc(func, ap, match_target, match_case_bb, next_bb, target_machine);
            }
            _ => panic!("Internal Compiler Error: Match expression cannot be matched with an array pattern"),
        },

        Pattern::Literal(Literal::Array(a)) => {
            let arr = array_lit_to_bc(bc_mod, func, a, target_machine);
            let cond = Operand::binary(BinaryOperator::Equals, arr, match_target.clone(), Type::Bool);
            func.add(branch_if_instr(cond, match_case_bb, next_bb));
        }

        Pattern::Literal(Literal::String(_, s)) => {
            let cond = Operand::binary(
                BinaryOperator::Equals,
                Operand::const_string(&s[..]),
                match_target.clone(),
                Type::Bool,
            );
            func.add(branch_if_instr(cond, match_case_bb, next_bb));
        }

        Pattern::Struct(p) => struct_pattern_match_to_bc(func, match_target, match_case_bb, next_bb, p, target_machine),

        Pattern::Nil(_) => nil_pattern_match_to_bc(func, match_target, match_case_bb, next_bb, target_machine),

        Pattern::Optional(o) => {
            optional_pattern_match_to_bc(func, match_target, match_case_bb, next_bb, o, target_machine)
        }

        Pattern::Ok(o) => result_pattern_match_to_bc(
            bc_mod,
            func,
            match_target,
            match_end_bb,
            match_case_bb,
            next_bb,
            &o.inner_type,
            &o.inner,
            true,
            target_machine,
        ),

        Pattern::Error(e) => result_pattern_match_to_bc(
            bc_mod,
            func,
            match_target,
            match_end_bb,
            match_case_bb,
            next_bb,
            &e.inner_type,
            &e.inner,
            false,
            target_machine,
        ),
    }
}
