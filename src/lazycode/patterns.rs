use crate::ast::{
    ptr_type, slice_type, ArrayLiteral, ArrayPattern, BinaryOperator, Literal, NameRef, OptionalPattern, Pattern,
    StructPattern, StructPatternBindingMode, SumTypeCaseIndexOf, Type,
};
use crate::target::Target;

use super::compiler::expr_to_bc;
use super::instruction::{branch_if_instr, branch_instr, Label};
use super::operand::{Constant, Operand};
use super::scope::Scope;
use super::ByteCodeModule;

fn literal_match_to_bc(scope: &mut Scope, match_target: Operand, lit: Operand, match_case_bb: Label, next_bb: Label) {
    let cond = Operand::binary(BinaryOperator::Equals, lit, match_target, Type::Bool);
    scope.add(branch_if_instr(cond, match_case_bb, next_bb));
}

fn name_pattern_match_to_bc(
    scope: &mut Scope,
    match_target: Operand,
    match_case_bb: Label,
    next_bb: Label,
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
                match_target,
                Operand::const_uint(idx as u64, target_machine.int_size),
                Type::Bool,
            );
            scope.add(branch_if_instr(cond, match_case_bb, next_bb));
        }
        Type::Sum(st) => {
            let idx = st
                .index_of(&nr.name)
                .expect("Internal Compiler Error: cannot determine index of sum type case");
            let sum_type_index = Operand::sti(match_target, target_machine.int_size);
            let cond = Operand::binary(
                BinaryOperator::Equals,
                sum_type_index,
                Operand::const_uint(idx as u64, target_machine.int_size),
                Type::Bool,
            );
            scope.add(branch_if_instr(cond, match_case_bb, next_bb));
        }
        _ => {
            panic!("Internal Compiler Error: Expression is not a valid match pattern");
        }
    }
}

fn binding_pattern_match_to_bc(scope: &mut Scope, match_target: Operand, match_case_bb: Label, nr: &NameRef) {
    scope.alias(&nr.name, match_target);
    scope.add(branch_instr(match_case_bb));
}

fn empty_array_pattern_to_bc(
    scope: &mut Scope,
    match_target: Operand,
    match_case_bb: Label,
    next_bb: Label,
    target_machine: &Target,
) {
    match match_target.get_type() {
        Type::Array(_) | Type::Slice(_) => {
            let len = Operand::len(match_target, target_machine.int_size);
            let cond = Operand::binary(
                BinaryOperator::Equals,
                len,
                Operand::const_uint(0, target_machine.int_size),
                Type::Bool,
            );
            scope.add(branch_if_instr(cond, match_case_bb, next_bb));
        }
        _ => panic!("Internal Compiler Error: Match expression cannot be matched with an empty array pattern"),
    }
}

fn array_pattern_match_to_bc(
    scope: &mut Scope,
    ap: &ArrayPattern,
    seq: Operand,
    match_case_bb: Label,
    next_bb: Label,
    target: &Target,
) {
    let on_true = scope.label();
    let cond = Operand::binary(
        BinaryOperator::GreaterThan,
        Operand::len(seq.safe_clone(), target.int_size),
        Operand::const_uint(0, target.int_size),
        Type::Bool,
    );
    scope.add(branch_if_instr(cond, on_true, next_bb));
    scope.start_label(on_true);

    let head_type = seq
        .get_type()
        .get_element_type()
        .expect("Invalid array type");
    let head = Operand::member(
        seq.safe_clone(),
        Operand::const_uint(0, target.int_size),
        head_type.clone(),
    );

    let _head = scope.alias(&ap.head, head);

    let tail_type = slice_type(head_type.clone());
    let tail_end = Operand::len(seq.safe_clone(), target.int_size);
    let tail = Operand::slice(
        seq.safe_clone(),
        Operand::Range {
            start: Box::new(Operand::const_uint(1, target.int_size)),
            end: Box::new(tail_end),
            typ: Type::Range(target.int_size),
        },
        tail_type.clone(),
    );

    scope.alias(&ap.tail, tail);
    scope.add(branch_instr(match_case_bb));
}

fn add_struct_pattern_bindings(p: &StructPattern, struct_var: Operand, scope: &mut Scope, target: &Target) {
    for (idx, b) in p.bindings.iter().enumerate() {
        if b.name == "_" {
            continue;
        }

        let member = if b.typ.pass_by_value() && b.mode == StructPatternBindingMode::Value {
            Operand::member(
                struct_var.safe_clone(),
                Operand::const_uint(idx as u64, target.int_size),
                b.typ.clone(),
            )
        } else {
            Operand::member_ptr(
                struct_var.safe_clone(),
                Operand::const_uint(idx as u64, target.int_size),
                ptr_type(b.typ.clone()),
            )
        };
        scope.alias(&b.name, member);
    }
}

fn struct_pattern_match_to_bc(
    scope: &mut Scope,
    match_target: Operand,
    match_case_bb: Label,
    next_bb: Label,
    p: &StructPattern,
    target_machine: &Target,
) {
    match &p.typ {
        Type::Struct(_) => {
            let bindings = scope.label();
            scope.add(branch_instr(bindings));
            scope.start_label(bindings);

            add_struct_pattern_bindings(p, match_target, scope, target_machine);
            scope.add(branch_instr(match_case_bb));
        }
        Type::Sum(st) => {
            let target_sum_type_index = Operand::sti(match_target.safe_clone(), target_machine.int_size);
            let idx = st
                .index_of(&p.name)
                .expect("Internal Compiler Error: cannot determine index of sum type case");
            let cond = Operand::binary(
                BinaryOperator::Equals,
                target_sum_type_index,
                Operand::const_uint(idx as u64, target_machine.int_size),
                Type::Bool,
            );

            let bindings = scope.label();
            scope.add(branch_if_instr(cond, bindings, next_bb));
            scope.start_label(bindings);

            let case_type = ptr_type(
                st.cases[idx]
                    .typ
                    .clone()
                    .expect("ICE: expecting struct case"),
            );
            let struct_ptr = Operand::member_ptr(
                match_target,
                Operand::const_uint(idx as u64, target_machine.int_size),
                case_type.clone(),
            );
            let struct_ptr = scope.to_var("$struct_ptr", struct_ptr);
            add_struct_pattern_bindings(p, struct_ptr, scope, target_machine);
            scope.add(branch_instr(match_case_bb));
        }
        _ => panic!("ICE: Expression is not a valid match pattern"),
    }
}

fn optional_pattern_match_to_bc(
    scope: &mut Scope,
    match_target: Operand,
    match_case_bb: Label,
    next_bb: Label,
    o: &OptionalPattern,
    target: &Target,
) {
    let sti = Operand::sti(match_target.safe_clone(), target.int_size);
    let cond = Operand::binary(
        BinaryOperator::Equals,
        sti,
        Operand::const_uint(0, target.int_size),
        Type::Bool,
    );

    let bindings = scope.label();
    scope.add(branch_if_instr(cond, bindings, next_bb));
    scope.start_label(bindings);

    if o.inner_type.pass_by_value() {
        let data = Operand::member(
            match_target,
            Operand::const_uint(0, target.int_size),
            o.inner_type.clone(),
        );
        scope.alias(&o.binding, data);
    } else {
        let data = Operand::member_ptr(
            match_target,
            Operand::const_uint(0, target.int_size),
            ptr_type(o.inner_type.clone()),
        );
        scope.alias(&o.binding, data);
    }
    scope.add(branch_instr(match_case_bb));
}

fn nil_pattern_match_to_bc(
    scope: &mut Scope,
    target: Operand,
    match_case_bb: Label,
    next_bb: Label,
    target_machine: &Target,
) {
    let sti = Operand::sti(target, target_machine.int_size);
    let cond = Operand::binary(
        BinaryOperator::Equals,
        sti,
        Operand::const_uint(0, target_machine.int_size),
        Type::Bool,
    );
    scope.add(branch_if_instr(cond, next_bb, match_case_bb));
}

fn result_pattern_match_to_bc(
    bc_mod: &mut ByteCodeModule,
    scope: &mut Scope,
    target: Operand,
    match_end_bb: Label,
    match_case_bb: Label,
    next_bb: Label,
    inner_type: &Type,
    inner_pattern: &Pattern,
    is_ok: bool,
    target_machine: &Target,
) {
    let index = if is_ok { 0 } else { 1 };
    let bind_bb = scope.label();

    let sti = Operand::sti(target.safe_clone(), target_machine.int_size);
    let idx = Operand::const_int(index, target_machine.int_size);
    let cond = Operand::binary(BinaryOperator::Equals, sti, idx.safe_clone(), Type::Bool);
    scope.add(branch_if_instr(cond, bind_bb, next_bb));

    scope.start_label(bind_bb);
    let member = Operand::member_ptr(target, idx, ptr_type(inner_type.clone()));
    let inner = scope.to_var(if is_ok { "$ok_inner" } else { "$err_inner" }, member);
    pattern_to_bc(
        bc_mod,
        scope,
        inner_pattern,
        inner,
        match_case_bb,
        match_end_bb,
        next_bb,
        target_machine,
    );
}

fn array_lit_to_bc(bc_mod: &mut ByteCodeModule, scope: &mut Scope, a: &ArrayLiteral, target: &Target) -> Operand {
    let mut members = Vec::new();
    for element in a.elements.iter() {
        let v = expr_to_bc(bc_mod, scope, element, target);
        members.push(v);
    }
    Operand::Array {
        members,
        typ: a.array_type.clone(),
    }
}

pub fn pattern_to_bc(
    bc_mod: &mut ByteCodeModule,
    scope: &mut Scope,
    pattern: &Pattern,
    match_target: Operand,
    match_case_bb: Label,
    match_end_bb: Label,
    next_bb: Label,
    target_machine: &Target,
) {
    match pattern {
        Pattern::Literal(Literal::Int(_, v, int_size)) => {
            literal_match_to_bc(
                scope,
                match_target,
                Operand::const_int(*v, *int_size),
                match_case_bb,
                next_bb,
            );
        }

        Pattern::Literal(Literal::UInt(_, v, int_size)) => {
            literal_match_to_bc(
                scope,
                match_target,
                Operand::const_uint(*v, *int_size),
                match_case_bb,
                next_bb,
            );
        }

        Pattern::Literal(Literal::Float(_, v, float_size)) => {
            literal_match_to_bc(
                scope,
                match_target,
                Operand::const_float(v.parse().expect("ICE: invalid float"), *float_size),
                match_case_bb,
                next_bb,
            );
        }

        Pattern::Literal(Literal::Bool(_, v)) => {
            literal_match_to_bc(scope, match_target, Operand::const_bool(*v), match_case_bb, next_bb);
        }

        Pattern::Literal(Literal::Char(_, v)) => {
            literal_match_to_bc(scope, match_target, Operand::const_char(*v), match_case_bb, next_bb);
        }

        Pattern::Literal(Literal::NullPtr(_, inner_type)) => {
            literal_match_to_bc(
                scope,
                match_target,
                Operand::Constant {
                    value: Constant::NullPtr(inner_type.clone()),
                },
                match_case_bb,
                next_bb,
            );
        }

        Pattern::Name(nr) => name_pattern_match_to_bc(scope, match_target, match_case_bb, next_bb, nr, target_machine),

        Pattern::Binding(nr) => binding_pattern_match_to_bc(scope, match_target, match_case_bb, nr),

        Pattern::Any(_) => {
            scope.add(branch_instr(match_case_bb));
        }

        Pattern::EmptyArray(_) => {
            empty_array_pattern_to_bc(scope, match_target, match_case_bb, next_bb, target_machine)
        }
        Pattern::Array(ap) => match &match_target.get_type() {
            Type::Array(_) | Type::Slice(_) => {
                array_pattern_match_to_bc(scope, ap, match_target, match_case_bb, next_bb, target_machine);
            }
            _ => panic!("Internal Compiler Error: Match expression cannot be matched with an array pattern"),
        },

        Pattern::Literal(Literal::Array(a)) => {
            let arr = array_lit_to_bc(bc_mod, scope, a, target_machine);
            let cond = Operand::binary(BinaryOperator::Equals, arr, match_target, Type::Bool);
            scope.add(branch_if_instr(cond, match_case_bb, next_bb));
        }

        Pattern::Literal(Literal::String(_, s)) => {
            let cond = Operand::binary(
                BinaryOperator::Equals,
                Operand::const_string(&s[..]),
                match_target,
                Type::Bool,
            );
            scope.add(branch_if_instr(cond, match_case_bb, next_bb));
        }

        Pattern::Struct(p) => {
            struct_pattern_match_to_bc(scope, match_target, match_case_bb, next_bb, p, target_machine)
        }

        Pattern::Nil(_) => nil_pattern_match_to_bc(scope, match_target, match_case_bb, next_bb, target_machine),

        Pattern::Optional(o) => {
            optional_pattern_match_to_bc(scope, match_target, match_case_bb, next_bb, o, target_machine)
        }

        Pattern::Ok(o) => result_pattern_match_to_bc(
            bc_mod,
            scope,
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
            scope,
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
