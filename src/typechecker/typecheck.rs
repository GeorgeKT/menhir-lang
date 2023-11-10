use super::genericmapper::fill_in_generics;
use super::instantiate::make_concrete;
use super::instantiategenerics::instantiate_generics;
use super::matchchecker::check_match_is_exhaustive;
use super::typecheckercontext::{ImportSymbolResolver, TypeCheckerContext};
use super::typeresolver::{resolve_type, resolve_types, TypeResolved};
use crate::ast::*;
use crate::compileerror::{
    type_error, type_error_result, unknown_name, unknown_type_result, CompileError, CompileResult,
};
use crate::span::Span;
use crate::target::Target;
use std::ops::Deref;

#[derive(Debug)]
enum TypeCheckAction {
    Valid(Type),
    ReplaceBy(Expression),
}

impl TypeCheckAction {
    pub fn unwrap(self) -> Type {
        match self {
            TypeCheckAction::Valid(t) => t,
            _ => panic!("Expecting a real type here, and not a replace by expression !"),
        }
    }
}

type TypeCheckResult = CompileResult<TypeCheckAction>;

fn valid(typ: Type) -> TypeCheckResult {
    Ok(TypeCheckAction::Valid(typ))
}

fn replace_by(e: Expression) -> TypeCheckResult {
    Ok(TypeCheckAction::ReplaceBy(e))
}

fn convert_type(
    ctx: &mut TypeCheckerContext,
    dst_type: &Type,
    src_type: &Type,
    expr: &mut Expression,
    target: &Target,
) -> CompileResult<()> {
    if *dst_type == *src_type {
        return Ok(());
    }

    let mut converted = false;
    if let Some(new_expression) = dst_type.convert(src_type, expr) {
        *expr = new_expression;
        converted = true;
    }

    if let Expression::Literal(ref mut lit) = *expr {
        if let Some(new_lit) = lit.try_convert(dst_type) {
            *lit = new_lit;
            converted = true;
        }
    }

    if converted {
        assert_eq!(type_check_expression(ctx, expr, None, target)?, *dst_type);
        Ok(())
    } else {
        type_error_result(
            &expr.span(),
            format!(
                "Expecting an expression of type {} or something convertible to, but found one of type {}",
                dst_type, src_type
            ),
        )
    }
}

fn type_check_unary_op(ctx: &mut TypeCheckerContext, u: &mut UnaryOp, target: &Target) -> TypeCheckResult {
    match u.operator {
        UnaryOperator::Sub => {
            let e_type = type_check_expression(ctx, &mut u.expression, None, target)?;
            if e_type.is_generic() {
                u.typ = e_type.clone();
                return valid(e_type);
            }

            if !e_type.is_numeric() {
                type_error_result(
                    &u.span,
                    format!("Unary operator {} expects a numeric expression", u.operator),
                )
            } else {
                u.typ = e_type.clone();
                valid(e_type)
            }
        }

        UnaryOperator::Not => {
            type_check_with_conversion(ctx, &mut u.expression, &Type::Bool, target)?;
            u.typ = Type::Bool;
            valid(Type::Bool)
        }
    }
}

fn type_check_with_conversion(
    ctx: &mut TypeCheckerContext,
    e: &mut Expression,
    expected_type: &Type,
    target: &Target,
) -> CompileResult<()> {
    let typ = type_check_expression(ctx, e, None, target)?;
    convert_type(ctx, expected_type, &typ, e, target)
}

fn basic_bin_op_checks(
    ctx: &mut TypeCheckerContext,
    b: &mut BinaryOp,
    left_type: Type,
    right_type: Type,
    target: &Target,
) -> CompileResult<()> {
    if left_type != right_type {
        let result = type_check_with_conversion(ctx, &mut b.right, &left_type, target).or(type_check_with_conversion(
            ctx,
            &mut b.left,
            &right_type,
            target,
        ));

        if result.is_err() {
            return type_error_result(
                &b.span,
                format!(
                    "Operator {} expects operands of the same type (left type: {}, right type: {})",
                    b.operator, left_type, right_type
                ),
            );
        }
    }

    let new_left_type = b.left.get_type(target.int_size);
    if !new_left_type.is_binary_operator_supported(b.operator) {
        type_error_result(
            &b.span,
            format!("Operator {} is not supported on {}", b.operator, new_left_type),
        )
    } else {
        Ok(())
    }
}

fn type_check_binary_op(ctx: &mut TypeCheckerContext, b: &mut BinaryOp, target: &Target) -> TypeCheckResult {
    let left_type = type_check_expression(ctx, &mut b.left, None, target)?;
    let right_type = type_check_expression(ctx, &mut b.right, None, target)?;
    if left_type.is_generic() || right_type.is_generic() {
        return valid(left_type);
    }

    match b.operator {
        BinaryOperator::Add | BinaryOperator::Sub | BinaryOperator::Mul | BinaryOperator::Div | BinaryOperator::Mod => {
            basic_bin_op_checks(ctx, b, left_type, right_type, target)?;
            b.typ = b.left.get_type(target.int_size);
            valid(b.typ.clone())
        }

        BinaryOperator::LessThan
        | BinaryOperator::GreaterThan
        | BinaryOperator::LessThanEquals
        | BinaryOperator::GreaterThanEquals => {
            basic_bin_op_checks(ctx, b, left_type, right_type, target)?;
            b.typ = Type::Bool;
            valid(Type::Bool)
        }

        BinaryOperator::And => {
            type_check_with_conversion(ctx, &mut b.left, &Type::Bool, target)?;
            type_check_with_conversion(ctx, &mut b.right, &Type::Bool, target)?;
            b.typ = Type::Bool;
            valid(Type::Bool)
        }

        BinaryOperator::Or => {
            if left_type.is_optional_of(&right_type) {
                b.typ = right_type.clone();
                valid(right_type)
            } else {
                type_check_with_conversion(ctx, &mut b.left, &Type::Bool, target)?;
                type_check_with_conversion(ctx, &mut b.right, &Type::Bool, target)?;
                b.typ = Type::Bool;
                valid(Type::Bool)
            }
        }
        BinaryOperator::Equals | BinaryOperator::NotEquals => {
            if left_type.is_optional_of(&Type::Unknown) && right_type.is_optional_of(&Type::Unknown) {
                return if b.operator == BinaryOperator::Equals {
                    replace_by(Expression::Literal(Literal::Bool(b.span.clone(), true)))
                } else {
                    replace_by(Expression::Literal(Literal::Bool(b.span.clone(), false)))
                };
            } else if left_type.is_optional() && right_type.is_optional_of(&Type::Unknown) {
                type_check_with_conversion(ctx, &mut b.right, &left_type, target)?;
            } else if right_type.is_optional() && left_type.is_optional_of(&Type::Unknown) {
                type_check_with_conversion(ctx, &mut b.left, &right_type, target)?;
            } else {
                basic_bin_op_checks(ctx, b, left_type, right_type, target)?;
            }
            b.typ = Type::Bool;
            valid(Type::Bool)
        }
        _ => type_error_result(&b.span, format!("Operator {} is not a binary operator", b.operator)),
    }
}

fn type_check_array_literal(ctx: &mut TypeCheckerContext, a: &mut ArrayLiteral, target: &Target) -> TypeCheckResult {
    if a.elements.is_empty() {
        a.array_type = array_type(target.native_uint_type.clone(), 0);
        return valid(a.array_type.clone());
    }

    let mut array_element_type = Type::Unknown;
    for e in &mut a.elements {
        let t = type_check_expression(ctx, e, None, target)?;
        if array_element_type == Type::Unknown {
            array_element_type = t;
        } else if array_element_type != t {
            return type_error_result(&e.span(), "Array elements must have the same type");
        }
    }

    let array_type = array_type(array_element_type, a.elements.len());
    if a.array_type == Type::Unknown {
        a.array_type = array_type;
    } else if a.array_type != array_type {
        return type_error_result(
            &a.span,
            format!("Array has type {}, but elements have type {}", a.array_type, array_type),
        );
    }

    valid(a.array_type.clone())
}

fn resolve_generic_args_in_call(
    ctx: &mut TypeCheckerContext,
    ft: &FuncType,
    c: &mut Call,
    target: &Target,
) -> CompileResult<Vec<Type>> {
    let mut arg_types = Vec::with_capacity(c.args.len());
    let mut count = c.generic_args.len();
    loop {
        arg_types.clear();
        for (arg, expected_arg_type) in c.args.iter_mut().zip(ft.args.iter()) {
            let expected_arg_type = make_concrete(ctx, &c.generic_args, expected_arg_type, &arg.span())?;
            let arg_type = type_check_expression(ctx, arg, Some(&expected_arg_type), target)?;
            let arg_type = make_concrete(ctx, &c.generic_args, &arg_type, &arg.span())?;

            if expected_arg_type.is_generic() {
                fill_in_generics(ctx, &arg_type, &expected_arg_type, &mut c.generic_args, &arg.span())?;
            }
            arg_types.push(arg_type);
        }

        if c.generic_args.len() == count {
            break;
        }
        count = c.generic_args.len();
    }

    Ok(arg_types)
}

fn type_check_call(ctx: &mut TypeCheckerContext, c: &mut Call, target: &Target) -> TypeCheckResult {
    let resolved = ctx
        .resolve(&c.callee.name)
        .ok_or_else(|| unknown_name(&c.callee.span, format!("Unknown call {}", c.callee.name)))?;

    c.callee.name = resolved.name;
    if let Type::Func(ref ft) = resolved.typ {
        if ft.args.len() != c.args.len() {
            return type_error_result(
                &c.span,
                format!(
                    "Attempting to call {} with {} arguments, but it needs {}",
                    c.callee.name,
                    c.args.len(),
                    ft.args.len()
                ),
            );
        }

        let arg_types = resolve_generic_args_in_call(ctx, ft, c, target)?;
        for (idx, arg) in c.args.iter_mut().enumerate() {
            let expected_arg_type = make_concrete(ctx, &c.generic_args, &ft.args[idx], &arg.span())?;
            let arg_type = &arg_types[idx];
            convert_type(ctx, &expected_arg_type, arg_type, arg, target)?;
        }

        if ft.return_type.is_generic() {
            c.return_type = make_concrete(ctx, &c.generic_args, &ft.return_type, &c.span)?;
            return valid(c.return_type.clone());
        }
        c.return_type = ft.return_type.clone();
        valid(ft.return_type.clone())
    } else {
        type_error_result(&c.span, format!("{} is not callable", c.callee.name))
    }
}

pub fn type_check_function(ctx: &mut TypeCheckerContext, fun: &mut Function, target: &Target) -> CompileResult<()> {
    ctx.enter_scope(Some(fun.sig.return_type.clone()));
    for arg in &mut fun.sig.args {
        ctx.add(Symbol::new(
            &arg.name,
            &arg.typ,
            arg.mutable,
            &arg.span,
            SymbolType::Normal,
        ))?;
    }

    let et = match type_check_expression(ctx, &mut fun.expression, Some(&fun.sig.return_type), target) {
        Err(CompileError::UnknownType(ref name, ref expected_type)) => {
            update_binding_type(ctx, &mut fun.expression, name, expected_type, target)?;
            type_check_expression(ctx, &mut fun.expression, Some(&fun.sig.return_type), target)?
        }
        Err(e) => return Err(e),
        Ok(typ) => typ,
    };

    ctx.exit_scope();
    if et != fun.sig.return_type {
        if let Some(expression) = fun.sig.return_type.convert(&et, &fun.expression) {
            fun.expression = expression;
        } else {
            return type_error_result(
                &fun.span,
                format!(
                    "Function {} has return type {}, but it is returning an expression of type {}",
                    fun.sig.name, fun.sig.return_type, et
                ),
            );
        }
    }

    fun.type_checked = true;
    Ok(())
}

fn is_result_mutable(ctx: &TypeCheckerContext, e: &Expression) -> bool {
    match *e {
        Expression::Dereference(ref d) => is_result_mutable(ctx, &d.inner),
        Expression::AddressOf(ref a) => is_result_mutable(ctx, &a.inner),

        Expression::NameRef(ref nr) => ctx.resolve(&nr.name).map(|rn| rn.mutable).unwrap_or(false),

        Expression::MemberAccess(ref ma) => is_result_mutable(ctx, &ma.left),

        _ => false,
    }
}

fn type_check_match(ctx: &mut TypeCheckerContext, m: &mut MatchExpression, target: &Target) -> TypeCheckResult {
    let target_type = type_check_expression(ctx, &mut m.target, None, target)?;
    let target_is_mutable = is_result_mutable(ctx, &m.target);
    let mut return_type = Type::Unknown;

    for c in &mut m.cases {
        let infer_case_type = |ctx: &mut TypeCheckerContext, e: &mut Expression, return_type: &Type| {
            let tt = type_check_expression(ctx, e, None, target)?;
            if *return_type != Type::Unknown && *return_type != tt {
                type_error_result(&e.span(), "Expressions in match statements must return the same type")
            } else {
                Ok(tt)
            }
        };

        let match_span = c.pattern.span();
        let case_type = match c.pattern {
            Pattern::EmptyArray(ref ap) => {
                if !target_type.is_sequence() {
                    return type_error_result(
                        &ap.span,
                        format!(
                            "Attempting to pattern match an expression of type {}, with an empty array",
                            target_type
                        ),
                    );
                }
                infer_case_type(ctx, &mut c.to_execute, &return_type)?
            }

            Pattern::Array(ref ap) => {
                if !target_type.is_sequence() {
                    return type_error_result(
                        &ap.span,
                        format!(
                            "Attempting to pattern match an expression of type {}, with an array",
                            target_type
                        ),
                    );
                }

                let element_type = target_type
                    .get_element_type()
                    .expect("target_type is not an array type");

                ctx.enter_scope(None);
                ctx.add(Symbol::new(
                    &ap.head,
                    &element_type,
                    false,
                    &ap.span,
                    SymbolType::Normal,
                ))?;
                ctx.add(Symbol::new(
                    &ap.tail,
                    &slice_type(element_type.clone()),
                    false,
                    &ap.span,
                    SymbolType::Normal,
                ))?;
                let ct = infer_case_type(ctx, &mut c.to_execute, &return_type)?;
                ctx.exit_scope();
                ct
            }

            Pattern::Name(ref mut nr) => {
                type_check_name(ctx, nr, Some(&target_type))?;
                if nr.typ != target_type {
                    return type_error_result(
                        &match_span,
                        format!(
                            "Cannot pattern match an expression of type {} with an expression of type {}",
                            target_type, nr.typ
                        ),
                    );
                }

                match nr.typ {
                    Type::Sum(ref st) => {
                        let idx = st
                            .index_of(&nr.name)
                            .expect("Internal Compiler Error: cannot determine index of sum type case");
                        let case = &st.cases[idx];
                        if case.typ == target.native_uint_type {
                            infer_case_type(ctx, &mut c.to_execute, &return_type)?
                        } else {
                            return type_error_result(
                                &match_span,
                                "Invalid pattern match, match should be with an empty sum case",
                            );
                        }
                    }
                    Type::Enum(_) => infer_case_type(ctx, &mut c.to_execute, &return_type)?,
                    _ => {
                        return type_error_result(&match_span, "Invalid pattern match");
                    }
                }
            }

            Pattern::Literal(Literal::Array(ref mut al)) => {
                let m_type = type_check_array_literal(ctx, al, target)?.unwrap();
                if !target_type.is_matchable(&m_type) {
                    return type_error_result(
                        &al.span,
                        format!(
                            "Pattern match of type {}, cannot match with an expression of type {}",
                            m_type, target_type
                        ),
                    );
                }

                infer_case_type(ctx, &mut c.to_execute, &return_type)?
            }

            Pattern::Literal(ref lit) => {
                let m_type = lit.get_type();
                if !target_type.is_matchable(&m_type) {
                    return type_error_result(
                        &c.pattern.span(),
                        format!(
                            "Pattern match of type {}, cannot match with an expression of type {}",
                            m_type, target_type
                        ),
                    );
                }

                infer_case_type(ctx, &mut c.to_execute, &return_type)?
            }

            Pattern::Struct(ref mut p) => {
                ctx.enter_scope(None);
                type_check_struct_pattern(ctx, p, target_is_mutable)?;
                if p.typ != target_type {
                    return type_error_result(
                        &match_span,
                        format!(
                            "Cannot pattern match an expression of type {} with an expression of type {}",
                            target_type, p.typ
                        ),
                    );
                }

                let ct = infer_case_type(ctx, &mut c.to_execute, &return_type)?;
                ctx.exit_scope();
                ct
            }

            Pattern::Any(_) => infer_case_type(ctx, &mut c.to_execute, &return_type)?,

            Pattern::Nil(ref span) => {
                if !target_type.is_optional() {
                    return type_error_result(
                        span,
                        format!(
                            "Cannot match type {} to nil, only optionals can be matched to nil",
                            target_type
                        ),
                    );
                }

                infer_case_type(ctx, &mut c.to_execute, &return_type)?
            }

            Pattern::Optional(ref mut o) => {
                if !target_type.is_optional() {
                    return type_error_result(
                        &o.span,
                        format!("Cannot match type {} to optional pattern", target_type),
                    );
                }

                o.inner_type = target_type
                    .get_element_type()
                    .expect("Optional type expected");
                ctx.enter_scope(None);
                ctx.add(Symbol::new(
                    &o.binding,
                    &o.inner_type,
                    target_is_mutable,
                    &o.span,
                    SymbolType::Normal,
                ))?;
                let ct = infer_case_type(ctx, &mut c.to_execute, &return_type)?;
                ctx.exit_scope();
                ct
            }
        };

        if return_type == Type::Unknown {
            return_type = case_type;
        } else if return_type != case_type {
            return type_error_result(&m.span, "Cases of match statements must return the same type");
        }
    }

    m.typ = return_type.clone();
    check_match_is_exhaustive(m, &target_type)?;
    valid(return_type)
}

fn type_check_lambda_body(ctx: &mut TypeCheckerContext, m: &mut Lambda, target: &Target) -> TypeCheckResult {
    ctx.enter_scope(None);
    for arg in &mut m.sig.args {
        ctx.add(Symbol::new(&arg.name, &arg.typ, false, &arg.span, SymbolType::Normal))?;
    }

    let return_type = type_check_expression(ctx, &mut m.expr, None, target)?;
    ctx.exit_scope();
    m.set_return_type(return_type);
    valid(m.sig.typ.clone())
}

fn type_check_lambda(
    ctx: &mut TypeCheckerContext,
    m: &mut Lambda,
    type_hint: Option<&Type>,
    target: &Target,
) -> TypeCheckResult {
    match type_hint {
        Some(typ) => {
            use uuid::Uuid;
            m.sig.name = format!("lambda-{}", Uuid::new_v4()); // Add a uuid, so we don't get name clashes
            m.apply_type(typ)?;
            let infered_type = type_check_lambda_body(ctx, m, target)?.unwrap();
            if infered_type != *typ {
                return type_error_result(
                    &m.span,
                    format!(
                        "Lambda body has the wrong type, expecting {}, got {}",
                        typ, infered_type
                    ),
                );
            }

            valid(infered_type)
        }
        None => {
            if m.is_generic() {
                return valid(Type::Unknown);
            }
            type_check_lambda_body(ctx, m, target)
        }
    }
}

fn is_instantiation_of(concrete_type: &Type, generic_type: &Type) -> bool {
    if !generic_type.is_generic() {
        return *concrete_type == *generic_type;
    }

    match (concrete_type, generic_type) {
        (Type::Array(a), Type::Array(b)) => is_instantiation_of(&a.element_type, &b.element_type),
        (_, &Type::Generic(_)) => true,
        (Type::Struct(a), Type::Struct(b)) => {
            a.members.len() == b.members.len()
                && a.members
                    .iter()
                    .zip(b.members.iter())
                    .all(|(ma, mb)| is_instantiation_of(&ma.typ, &mb.typ))
        }
        (Type::Func(a), Type::Func(b)) => {
            is_instantiation_of(&a.return_type, &b.return_type)
                && a.args
                    .iter()
                    .zip(b.args.iter())
                    .all(|(ma, mb)| is_instantiation_of(ma, mb))
        }
        (Type::Sum(a), Type::Sum(b)) => a
            .cases
            .iter()
            .zip(b.cases.iter())
            .all(|(ma, mb)| is_instantiation_of(&ma.typ, &mb.typ)),
        _ => false,
    }
}

fn type_check_name(ctx: &mut TypeCheckerContext, nr: &mut NameRef, type_hint: Option<&Type>) -> TypeCheckResult {
    if nr.name == "_" {
        return valid(Type::Unknown);
    }

    if !nr.typ.is_unknown() && !nr.typ.is_generic() {
        return valid(nr.typ.clone()); // We have already determined the type
    }

    let resolved = ctx
        .resolve(&nr.name)
        .ok_or_else(|| unknown_name(&nr.span, format!("Unknown name {}", nr.name)))?;
    nr.name = resolved.name;

    if let Some(typ) = type_hint {
        if resolved.typ == Type::Unknown {
            return unknown_type_result(&nr.name, typ);
        }

        if resolved.typ == *typ {
            nr.typ = resolved.typ;
            return valid(nr.typ.clone());
        }

        if !resolved.typ.is_generic() && !typ.is_generic() && !resolved.typ.is_convertible(typ) {
            return type_error_result(
                &nr.span,
                format!(
                    "Type mismatch: expecting {}, but {} has type {}",
                    typ, nr.name, resolved.typ
                ),
            );
        }

        if resolved.typ.is_generic() && !typ.is_generic() {
            if !is_instantiation_of(typ, &resolved.typ) {
                type_error_result(
                    &nr.span,
                    format!(
                        "Type mismatch: {} is not a valid instantiation of {}",
                        typ, resolved.typ
                    ),
                )
            } else {
                nr.typ = typ.clone();
                valid(nr.typ.clone())
            }
        } else {
            nr.typ = resolved.typ;
            valid(nr.typ.clone())
        }
    } else {
        nr.typ = resolved.typ;
        valid(nr.typ.clone())
    }
}

fn add_struct_bindings(
    ctx: &mut TypeCheckerContext,
    b: &mut StructPattern,
    struct_type: &StructType,
    mutable: bool,
) -> CompileResult<()> {
    for (binding, member) in b.bindings.iter_mut().zip(struct_type.members.iter()) {
        if binding.name == "_" {
            continue;
        }

        let mutable = match binding.mode {
            StructPatternBindingMode::Value => {
                binding.typ = member.typ.clone();
                false
            }

            StructPatternBindingMode::Pointer => {
                binding.typ = ptr_type(member.typ.clone());
                mutable
            }
        };

        ctx.add(Symbol::new(
            &binding.name,
            &binding.typ,
            mutable,
            &b.span,
            SymbolType::Normal,
        ))?;
    }
    Ok(())
}

fn type_check_binding(ctx: &mut TypeCheckerContext, b: &mut Binding, target: &Target) -> TypeCheckResult {
    b.typ = type_check_expression(ctx, &mut b.init, None, target)?;

    match b.binding_type {
        BindingType::Name(ref name) => {
            ctx.add(Symbol::new(name, &b.typ, b.mutable, &b.span, SymbolType::Normal))?;
        }

        BindingType::Struct(ref mut s) => {
            s.typ = b.typ.clone();

            if let Type::Struct(ref st) = b.typ {
                if st.members.len() != s.bindings.len() {
                    return type_error_result(
                        &s.span,
                        format!(
                            "Wrong number of members in struct binding (expecting {}, found {})",
                            st.members.len(),
                            s.bindings.len()
                        ),
                    );
                }

                add_struct_bindings(ctx, s, st, false)?;
            } else {
                return type_error_result(&b.init.span(), "Expression does not return a struct type");
            }
        }
    }

    valid(b.typ.clone())
}

fn update_binding_type(
    ctx: &mut TypeCheckerContext,
    e: &mut Expression,
    name: &str,
    expected_type: &Type,
    target: &Target,
) -> CompileResult<()> {
    let mut update_binding = |e: &mut Expression| {
        if let Expression::Bindings(ref mut bl) = *e {
            for b in &mut bl.bindings {
                if let BindingType::Name(ref b_name) = b.binding_type {
                    if *b_name == *name {
                        // It's one we know, so lets try again with a proper type hint
                        b.typ = type_check_expression(ctx, &mut b.init, Some(expected_type), target)?;
                        ctx.update(Symbol::new(b_name, &b.typ, b.mutable, &b.span, SymbolType::Normal));
                        return Ok(());
                    }
                }
            }
        }
        Ok(())
    };
    e.visit_mut(&mut update_binding)
}

fn ends_with_early_return(e: &Expression) -> bool {
    match e {
        Expression::Return(_) => true,
        Expression::Block(block) => {
            matches!(block.expressions.last(), Some(Expression::Return(_)))
        }
        _ => false,
    }
}

fn type_check_if(
    ctx: &mut TypeCheckerContext,
    i: &mut IfExpression,
    type_hint: Option<&Type>,
    target: &Target,
) -> TypeCheckResult {
    type_check_with_conversion(ctx, &mut i.condition, &Type::Bool, target)?;

    let on_true_type = type_check_expression(ctx, &mut i.on_true, type_hint, target)?;
    let on_false_type = if let Some(ref mut expr) = i.on_false {
        type_check_expression(ctx, expr, type_hint, target)?
    } else {
        Type::Void
    };

    if on_true_type != on_false_type && !ends_with_early_return(&i.on_true) {
        if i.on_false.is_none() {
            type_error_result(
                &i.span,
                format!(
                    "If expressions without an else part, must return void (type of then part is {})",
                    on_true_type
                ),
            )
        } else if on_true_type.is_optional_of(&on_false_type) || on_true_type.is_optional_of(&Type::Unknown) {
            let optional_type = optional_type(on_false_type);
            if let Some(ref mut expr) = i.on_false {
                type_check_with_conversion(ctx, expr, &optional_type, target)?;
            }
            type_check_with_conversion(ctx, &mut i.on_true, &optional_type, target)?;
            i.typ = optional_type.clone();
            valid(optional_type)
        } else if on_false_type.is_optional_of(&on_true_type) || on_false_type.is_optional_of(&Type::Unknown) {
            let optional_type = optional_type(on_true_type);
            if let Some(ref mut expr) = i.on_false {
                type_check_with_conversion(ctx, expr, &optional_type, target)?;
            }
            type_check_with_conversion(ctx, &mut i.on_true, &optional_type, target)?;
            i.typ = optional_type.clone();
            valid(optional_type)
        } else {
            type_error_result(&i.span,
                format!("then and else expression of an if expression need to be of the same type, then has type {}, else has type {}", on_true_type, on_false_type)
            )
        }
    } else {
        i.typ = on_true_type;
        valid(on_false_type)
    }
}

fn type_check_struct_members_in_initializer(
    ctx: &mut TypeCheckerContext,
    st: &StructType,
    si: &mut StructInitializer,
    target: &Target,
) -> CompileResult<Type> {
    if st.members.len() != si.member_initializers.len() {
        return type_error_result(
            &si.span,
            format!(
                "Type {} has {} members, but attempting to initialize {} members",
                si.struct_name,
                st.members.len(),
                si.member_initializers.len()
            ),
        );
    }

    let mut new_members = Vec::with_capacity(st.members.len());

    for (idx, (member, mi)) in st
        .members
        .iter()
        .zip(si.member_initializers.iter_mut())
        .enumerate()
    {
        let t = type_check_expression(ctx, mi, Some(&member.typ), target)?;
        let expected_type = if member.typ.is_generic() {
            fill_in_generics(ctx, &t, &member.typ, &mut si.generic_args, &mi.span())?
        } else {
            member.typ.clone()
        };

        if t != expected_type {
            if let Some(new_mi) = expected_type.convert(&t, mi) {
                *mi = new_mi;
            } else {
                return type_error_result(
                    &mi.span(),
                    format!(
                        "Attempting to initialize member {} with type '{}', expecting an expression of type '{}'",
                        idx, t, expected_type
                    ),
                );
            }
        }

        new_members.push(struct_member(&member.name, expected_type));
    }

    Ok(struct_type(&st.name, new_members))
}

fn type_check_anonymous_struct_initializer(
    ctx: &mut TypeCheckerContext,
    si: &mut StructInitializer,
    target: &Target,
) -> TypeCheckResult {
    let mut new_members = Vec::with_capacity(si.member_initializers.len());
    for mi in &mut si.member_initializers {
        let t = type_check_expression(ctx, mi, None, target)?;
        new_members.push(struct_member("", t));
    }
    si.typ = struct_type("", new_members);
    valid(si.typ.clone())
}

fn type_check_struct_initializer(
    ctx: &mut TypeCheckerContext,
    si: &mut StructInitializer,
    target: &Target,
) -> TypeCheckResult {
    if si.struct_name.is_empty() {
        return type_check_anonymous_struct_initializer(ctx, si, target);
    }

    let resolved = ctx
        .resolve(&si.struct_name)
        .ok_or_else(|| unknown_name(&si.span, format!("Unknown struct {}", si.struct_name)))?;
    si.struct_name = resolved.name;
    match resolved.typ {
        Type::Struct(ref st) => {
            si.typ = type_check_struct_members_in_initializer(ctx, st, si, target)?;
            valid(si.typ.clone())
        }
        Type::Sum(ref st) => {
            let idx = st
                .index_of(&si.struct_name)
                .expect("Internal Compiler Error: cannot determine index of sum type case");
            let mut sum_type_cases = Vec::with_capacity(st.cases.len());
            for (i, case) in st.cases.iter().enumerate() {
                let typ = if i == idx {
                    match case.typ {
                        Type::Struct(ref s) => type_check_struct_members_in_initializer(ctx, s, si, target)?,
                        Type::Int(p) => Type::Int(p),
                        _ => return type_error_result(&si.span, "Invalid sum type case"),
                    }
                } else {
                    case.typ.clone()
                };
                sum_type_cases.push(sum_type_case(&case.name, typ))
            }

            si.typ = sum_type(&st.name, sum_type_cases);
            valid(si.typ.clone())
        }

        Type::String => {
            let string_rep = string_type_representation(target.int_size);
            type_check_struct_members_in_initializer(ctx, &string_rep, si, target)?;
            si.typ = Type::String;
            valid(Type::String)
        }

        _ => type_error_result(&si.span, format!("{} is not a struct", si.struct_name)),
    }
}

fn find_member_type(members: &[StructMember], member_name: &str, span: &Span) -> CompileResult<(usize, Type)> {
    members
        .iter()
        .enumerate()
        .find(|&(_, m)| m.name == *member_name)
        .map(|(idx, m)| (idx, m.typ.clone()))
        .ok_or_else(|| unknown_name(span, format!("Unknown struct member {}", member_name)))
}

fn member_call_to_call(left: &Expression, call: &Call, int_size: IntSize) -> Expression {
    let mut args = Vec::with_capacity(call.args.len() + 1);
    let first_arg = match left.get_type(int_size) {
        Type::Pointer(_) => left.clone(),
        _ => address_of(left.clone(), left.span()),
    };

    args.push(first_arg);
    args.extend(call.args.iter().cloned());
    Expression::Call(Box::new(Call::new(call.callee.clone(), args, call.span.clone())))
}

fn type_check_generic_member_call(
    ctx: &mut TypeCheckerContext,
    call: &mut Call,
    gt: &GenericType,
) -> CompileResult<Type> {
    let check_interface = |interface: &Type, call: &Call| {
        if let Type::Interface(ref it) = *interface {
            for func in &it.functions {
                if func.name == call.callee.name {
                    return Some(func.return_type.clone());
                }
            }
        }
        None
    };

    match *gt {
        GenericType::Any(ref name) => {
            let interface = ctx
                .resolve(name)
                .ok_or_else(|| type_error(&call.span, format!("Type {} is not an interface", name)))?;

            call.return_type = check_interface(&interface.typ, call).ok_or_else(|| {
                type_error(
                    &call.span,
                    format!(
                        "Interface {} has no member function named {}",
                        interface.name, call.callee.name
                    ),
                )
            })?;
            Ok(call.return_type.clone())
        }

        GenericType::Restricted(ref interfaces) => {
            for interface in interfaces {
                if let Some(typ) = check_interface(interface, call) {
                    call.return_type = typ.clone();
                    return Ok(typ);
                }
            }

            type_error_result(&call.span, format!("No member function named {}", call.callee.name))
        }
    }
}

fn to_static_function_call(ctx: &mut TypeCheckerContext, sma: &MemberAccess) -> Option<Call> {
    if let Expression::NameRef(ref nr) = sma.left {
        if let MemberAccessType::Call(ref call) = sma.right {
            let call_name = format!("{}.{}", nr.name, call.callee.name);
            if let Some(rn) = ctx.resolve(&call_name) {
                if let Type::Func(_) = rn.typ {
                    let name_span = nr.span.expanded(call.callee.span.end);
                    let full_span = name_span.expanded(call.span.end);
                    return Some(Call::new(
                        NameRef::new(call_name, name_span),
                        call.args.clone(),
                        full_span,
                    ));
                }
            }
        }
    }
    None
}

fn type_check_member_access(ctx: &mut TypeCheckerContext, sma: &mut MemberAccess, target: &Target) -> TypeCheckResult {
    let left_type = type_check_expression(ctx, &mut sma.left, None, target)?;
    // member access through pointer is the same as a normal member access
    let left_type_ref = if let Type::Pointer(ref inner) = left_type {
        inner.deref()
    } else {
        &left_type
    };

    if let Some(call) = to_static_function_call(ctx, sma) {
        return replace_by(Expression::Call(Box::new(call)));
    }

    let (typ, new_right) = match (&mut sma.right, left_type_ref) {
        (&mut MemberAccessType::Property(Property::Len), &Type::Slice(_))
        | (&mut MemberAccessType::Property(Property::Len), &Type::Array(_))
        | (&mut MemberAccessType::Property(Property::Len), &Type::String) => (target.native_uint_type.clone(), None),

        (&mut MemberAccessType::Property(Property::Data), &Type::String) => (ptr_type(Type::UInt(IntSize::I8)), None),

        (&mut MemberAccessType::Property(Property::Data), Type::Slice(st)) => (ptr_type(st.element_type.clone()), None),

        (&mut MemberAccessType::Name(ref mut field), Type::Struct(st)) => {
            let (member_idx, member_type) = find_member_type(&st.members, &field.name, &sma.span)?;
            field.index = member_idx;
            (member_type, None)
        }

        (&mut MemberAccessType::Name(ref mut field), &Type::Array(_))
        | (&mut MemberAccessType::Name(ref mut field), &Type::Slice(_))
        | (&mut MemberAccessType::Name(ref mut field), &Type::String) => {
            if let Some((typ, member_access_type)) = left_type.get_property_type(&field.name, target) {
                (typ, Some(member_access_type))
            } else {
                return type_error_result(
                    &sma.span,
                    format!("Type '{}' has no property named '{}'", left_type, field.name),
                );
            }
        }

        (&mut MemberAccessType::Call(ref mut call), Type::Struct(st)) => {
            let call_name = format!("{}.{}", st.name, call.callee.name);
            call.callee.name = call_name;
            return replace_by(member_call_to_call(&sma.left, call, target.int_size));
        }

        (&mut MemberAccessType::Call(ref mut call), Type::Sum(st)) => {
            let call_name = format!("{}.{}", st.name, call.callee.name);
            call.callee.name = call_name;
            return replace_by(member_call_to_call(&sma.left, call, target.int_size));
        }

        (&mut MemberAccessType::Call(ref mut call), Type::Generic(gt)) => {
            (type_check_generic_member_call(ctx, call, gt)?, None)
        }

        _ => {
            return type_error_result(
                &sma.span,
                format!("Cannot determine type of member access ({})", left_type_ref),
            );
        }
    };

    sma.typ = typ;
    if let Some(nr) = new_right {
        sma.right = nr;
    }
    valid(sma.typ.clone())
}

fn type_check_struct_pattern(
    ctx: &mut TypeCheckerContext,
    p: &mut StructPattern,
    target_is_mutable: bool,
) -> CompileResult<()> {
    if !p.typ.is_unknown() {
        return Ok(());
    }

    let resolved = ctx
        .resolve(&p.name)
        .ok_or_else(|| unknown_name(&p.span, format!("Unknown struct {}", p.name)))?;
    p.name = resolved.name.clone();
    match resolved.typ {
        Type::Sum(ref st) => {
            let idx = st
                .index_of(&p.name)
                .expect("Internal Compiler Error: cannot determine index of sum type case");
            let case = &st.cases[idx];
            match case.typ {
                Type::Struct(ref s) => {
                    if s.members.len() != p.bindings.len() {
                        type_error_result(
                            &p.span,
                            format!(
                                "Wrong number of bindings in pattern match (expecting {}, found {})",
                                s.members.len(),
                                p.bindings.len()
                            ),
                        )
                    } else {
                        add_struct_bindings(ctx, p, s, target_is_mutable)?;
                        p.typ = Type::Sum(st.clone());
                        Ok(())
                    }
                }
                _ => type_error_result(
                    &p.span,
                    "Attempting to pattern match a normal sum type case with a struct",
                ),
            }
        }

        Type::Struct(ref st) => {
            add_struct_bindings(ctx, p, st, target_is_mutable)?;
            p.typ = Type::Struct(st.clone());
            Ok(())
        }
        _ => type_error_result(
            &p.span,
            "Struct pattern is only allowed for structs and sum types containing structs",
        ),
    }
}

fn type_check_block(
    ctx: &mut TypeCheckerContext,
    b: &mut Block,
    type_hint: Option<&Type>,
    target: &Target,
) -> TypeCheckResult {
    ctx.enter_scope(None);
    let num = b.expressions.len();
    for (idx, e) in b.expressions.iter_mut().enumerate() {
        let typ = type_check_expression(ctx, e, type_hint, target)?;
        if idx == num - 1 {
            b.typ = typ;
        }
    }

    ctx.exit_scope();
    valid(b.typ.clone())
}

fn type_check_new(
    ctx: &mut TypeCheckerContext,
    n: &mut NewExpression,
    type_hint: Option<&Type>,
    target: &Target,
) -> TypeCheckResult {
    let typ = type_check_expression(ctx, &mut n.inner, type_hint, target)?;
    n.typ = ptr_type(typ);
    valid(n.typ.clone())
}

fn type_check_delete(
    ctx: &mut TypeCheckerContext,
    d: &mut DeleteExpression,
    type_hint: Option<&Type>,
    target: &Target,
) -> TypeCheckResult {
    let typ = type_check_expression(ctx, &mut d.inner, type_hint, target)?;
    match typ {
        Type::Pointer(_) => valid(Type::Void),
        _ => type_error_result(
            &d.span,
            format!(
                "delete expression expects a pointer argument, argument has type {}",
                typ
            ),
        ),
    }
}

fn type_check_array_to_slice(
    ctx: &mut TypeCheckerContext,
    ats: &mut ArrayToSlice,
    type_hint: Option<&Type>,
    target: &Target,
) -> TypeCheckResult {
    let t = type_check_expression(ctx, &mut ats.inner, type_hint, target)?;
    match t {
        Type::Array(at) => {
            ats.slice_type = slice_type(at.element_type.clone());
            valid(ats.slice_type.clone())
        }
        _ => type_error_result(&ats.span, "array to slice expression must have an array as input"),
    }
}

fn type_check_address_of(
    ctx: &mut TypeCheckerContext,
    a: &mut AddressOfExpression,
    target: &Target,
) -> TypeCheckResult {
    let t = type_check_expression(ctx, &mut a.inner, None, target)?;
    a.typ = ptr_type(t);
    valid(a.typ.clone())
}

fn type_check_dereference(
    ctx: &mut TypeCheckerContext,
    a: &mut DereferenceExpression,
    target: &Target,
) -> TypeCheckResult {
    let t = type_check_expression(ctx, &mut a.inner, None, target)?;
    if let Type::Pointer(inner) = t {
        a.typ = inner.deref().clone();
        valid(a.typ.clone())
    } else {
        type_error_result(&a.span, "Attempting to dereference a non pointer type expression")
    }
}

fn type_check_index_operation(
    ctx: &mut TypeCheckerContext,
    iop: &mut IndexOperation,
    target: &Target,
) -> CompileResult<Type> {
    let target_type = type_check_expression(ctx, &mut iop.target, None, target)?;
    let index_type = type_check_expression(ctx, &mut iop.index_expr, None, target)?;
    match index_type {
        Type::Int(_) | Type::UInt(_) => (),
        _ => {
            return type_error_result(
                &iop.span,
                format!(
                    "An expression of type {}, cannot be used to index something. Only integers are supported.",
                    index_type
                ),
            )
        }
    }

    let typ = match target_type {
        Type::Pointer(ref inner) => inner.deref().clone(),
        Type::Slice(ref st) => st.element_type.clone(),
        Type::Array(ref at) => at.element_type.clone(),
        _ => {
            return type_error_result(
                &iop.span,
                format!("Cannot an index an expression of type {}", target_type),
            )
        }
    };

    iop.typ = typ.clone();
    Ok(typ)
}

fn to_regular_assign(a: &mut Assign, int_size: IntSize) {
    let op = match a.operator {
        AssignOperator::Assign => return,
        AssignOperator::Add => BinaryOperator::Add,
        AssignOperator::Sub => BinaryOperator::Sub,
        AssignOperator::Mul => BinaryOperator::Mul,
        AssignOperator::Div => BinaryOperator::Div,
        AssignOperator::And => BinaryOperator::And,
        AssignOperator::Or => BinaryOperator::Or,
    };

    let left = match a.left {
        AssignTarget::Var(ref nr) => Expression::NameRef(nr.clone()),
        AssignTarget::MemberAccess(ref ma) => Expression::MemberAccess(Box::new(ma.clone())),
        AssignTarget::Dereference(ref d) => Expression::Dereference(Box::new(d.clone())),
        AssignTarget::IndexOperation(ref i) => Expression::IndexOperation(Box::new(i.clone())),
    };

    let right = bin_op_with_type(op, left, a.right.clone(), a.right.span(), a.right.get_type(int_size));

    a.operator = AssignOperator::Assign;
    a.right = right;
}

fn type_check_assign(ctx: &mut TypeCheckerContext, a: &mut Assign, target: &Target) -> TypeCheckResult {
    let dst_type = match a.left {
        AssignTarget::Var(ref mut nr) => {
            type_check_name(ctx, nr, None)?;
            if !ctx.resolve(&nr.name).map(|rn| rn.mutable).unwrap_or(false) {
                return type_error_result(
                    &nr.span,
                    format!("Attempting to modify non mutable variable {}", nr.name),
                );
            }
            nr.typ.clone()
        }

        AssignTarget::MemberAccess(ref mut ma) => {
            type_check_member_access(ctx, ma, target)?;
            if !is_result_mutable(ctx, &ma.left) {
                return type_error_result(&ma.span, "Attempting to modify non mutable expression");
            }
            ma.typ.clone()
        }

        AssignTarget::Dereference(ref mut d) => {
            type_check_dereference(ctx, d, target)?;
            if !is_result_mutable(ctx, &d.inner) {
                return type_error_result(&d.span, "Attempting to modify non mutable expression");
            }
            d.typ.clone()
        }

        AssignTarget::IndexOperation(ref mut iop) => type_check_index_operation(ctx, iop, target)?,
    };

    type_check_with_conversion(ctx, &mut a.right, &dst_type, target)?;
    match a.operator {
        AssignOperator::Assign => (),
        AssignOperator::Add | AssignOperator::Sub | AssignOperator::Mul | AssignOperator::Div => {
            if !dst_type.is_numeric() {
                return type_error_result(
                    &a.span,
                    format!("Operator {} is only supported on numeric types", a.operator),
                );
            }
        }

        AssignOperator::And | AssignOperator::Or => {
            if dst_type != Type::Bool {
                return type_error_result(
                    &a.span,
                    format!("Operator {} is only supported on booleans", a.operator),
                );
            }
        }
    }
    to_regular_assign(a, target.int_size); // Convert to a regular assign, for code generation
    valid(Type::Void)
}

fn type_check_while(ctx: &mut TypeCheckerContext, w: &mut WhileLoop, target: &Target) -> TypeCheckResult {
    type_check_with_conversion(ctx, &mut w.cond, &Type::Bool, target)?;
    type_check_expression(ctx, &mut w.body, None, target)?;
    valid(Type::Void)
}

fn type_check_for(ctx: &mut TypeCheckerContext, f: &mut ForLoop, target: &Target) -> TypeCheckResult {
    let typ = type_check_expression(ctx, &mut f.iterable, None, target)?;
    match typ {
        // Iterable
        Type::String | Type::Array(_) | Type::Slice(_) => {
            ctx.enter_scope(None);
            let element_type = if let Some(et) = typ.get_element_type() {
                et
            } else {
                return type_error_result(&f.span, format!("Cannot determine type of {}", f.loop_variable));
            };

            f.loop_variable_type = element_type.clone();
            ctx.add(Symbol::new(
                &f.loop_variable,
                &element_type,
                false,
                &f.span,
                SymbolType::Normal,
            ))?;
            type_check_expression(ctx, &mut f.body, None, target)?;
            valid(Type::Void)
        }
        _ => type_error_result(&f.span, format!("Cannot iterate over expressions of type {}", typ)),
    }
}

fn type_check_cast(ctx: &mut TypeCheckerContext, c: &mut TypeCast, target: &Target) -> TypeCheckResult {
    let inner_type = type_check_expression(ctx, &mut c.inner, None, target)?;
    match (inner_type, &c.destination_type) {
        (Type::Int(_), &Type::UInt(_))
        | (Type::Int(_), &Type::Float(_))
        | (Type::UInt(_), &Type::Int(_))
        | (Type::UInt(_), &Type::Float(_))
        | (Type::Float(_), &Type::Int(_))
        | (Type::Float(_), &Type::UInt(_)) => valid(c.destination_type.clone()),
        (Type::Pointer(_), Type::Pointer(to)) if *to.deref() == Type::Void => valid(c.destination_type.clone()),
        (Type::Pointer(ref from), &Type::Pointer(_)) if *from.deref() == Type::Void => {
            valid(c.destination_type.clone())
        }
        (Type::Pointer(_), &Type::Bool) => valid(Type::Bool),
        (Type::Array(ref at), Type::Pointer(to)) if at.element_type == *to.deref() => valid(c.destination_type.clone()),
        (inner_type, _) => type_error_result(
            &c.span,
            format!(
                "Cast from type {} to type {} is not allowed",
                inner_type, c.destination_type
            ),
        ),
    }
}

fn type_check_compiler_call(
    ctx: &mut TypeCheckerContext,
    cc: &mut CompilerCall,
    type_hint: Option<&Type>,
    target: &Target,
) -> TypeCheckResult {
    match *cc {
        CompilerCall::SizeOf(ref mut typ, ref span) => {
            if resolve_type(ctx, typ) == TypeResolved::No {
                type_error_result(span, format!("Unable to resolve type {}", typ))
            } else {
                valid(target.native_uint_type.clone())
            }
        }

        CompilerCall::Slice {
            ref mut data,
            ref mut len,
            ref mut typ,
            ref span,
        } => {
            let data_type = if let Some(Type::Slice(st)) = type_hint {
                let data_ptr_type = ptr_type(st.element_type.clone());
                type_check_expression(ctx, data, Some(&data_ptr_type), target)?
            } else {
                type_check_expression(ctx, data, None, target)?
            };

            type_check_with_conversion(ctx, len, &target.native_uint_type, target)?;
            if let Type::Pointer(ref inner) = data_type {
                *typ = slice_type(inner.deref().clone());
                valid(typ.clone())
            } else {
                type_error_result(
                    span,
                    format!("The first argument of @slice, must be a pointer, not a {}", data_type),
                )
            }
        }
    }
}

fn type_check_literal(
    ctx: &mut TypeCheckerContext,
    lit: &mut Literal,
    type_hint: Option<&Type>,
    target: &Target,
) -> TypeCheckResult {
    match *lit {
        Literal::Array(ref mut a) => type_check_array_literal(ctx, a, target),

        Literal::NullPtr(ref span, ref mut typ) => {
            if let Some(Type::Pointer(inner_type)) = type_hint {
                *typ = inner_type.deref().clone();
                valid(ptr_type(typ.clone()))
            } else if *typ != Type::Unknown {
                type_error_result(span, "Unable to determine type of null expression")
            } else {
                valid(ptr_type(typ.clone()))
            }
        }

        _ => {
            let typ = lit.get_type();
            match type_hint {
                None => valid(typ),
                Some(expected) if typ == *expected => valid(typ),
                Some(expected) => {
                    if let Some(new_lit) = lit.try_convert(expected) {
                        replace_by(Expression::Literal(new_lit))
                    } else {
                        valid(typ)
                    }
                }
            }
        }
    }
}

pub fn type_check_expression(
    ctx: &mut TypeCheckerContext,
    e: &mut Expression,
    type_hint: Option<&Type>,
    target: &Target,
) -> CompileResult<Type> {
    let type_check_result = match *e {
        Expression::UnaryOp(ref mut op) => type_check_unary_op(ctx, op, target),
        Expression::BinaryOp(ref mut op) => type_check_binary_op(ctx, op, target),
        Expression::Literal(ref mut lit) => type_check_literal(ctx, lit, type_hint, target),
        Expression::Call(ref mut c) => type_check_call(ctx, c, target),
        Expression::NameRef(ref mut nr) => type_check_name(ctx, nr, type_hint),
        Expression::Match(ref mut m) => type_check_match(ctx, m, target),
        Expression::Lambda(ref mut l) => type_check_lambda(ctx, l, type_hint, target),
        Expression::Bindings(ref mut l) => {
            for b in &mut l.bindings {
                type_check_binding(ctx, b, target)?;
            }
            valid(Type::Void)
        }
        Expression::If(ref mut i) => type_check_if(ctx, i, type_hint, target),
        Expression::Block(ref mut b) => type_check_block(ctx, b, type_hint, target),
        Expression::StructInitializer(ref mut si) => type_check_struct_initializer(ctx, si, target),
        Expression::MemberAccess(ref mut sma) => type_check_member_access(ctx, sma, target),
        Expression::New(ref mut n) => type_check_new(ctx, n, type_hint, target),
        Expression::Delete(ref mut d) => type_check_delete(ctx, d, type_hint, target),
        Expression::ArrayToSlice(ref mut ats) => type_check_array_to_slice(ctx, ats, type_hint, target),
        Expression::AddressOf(ref mut a) => type_check_address_of(ctx, a, target),
        Expression::Dereference(ref mut d) => type_check_dereference(ctx, d, target),
        Expression::Assign(ref mut a) => type_check_assign(ctx, a, target),
        Expression::While(ref mut w) => type_check_while(ctx, w, target),
        Expression::For(ref mut f) => type_check_for(ctx, f, target),
        Expression::Void => valid(Type::Void),
        Expression::Nil(ref mut nt) => {
            if let Some(typ) = type_hint {
                if let Type::Optional(_) = *typ {
                    nt.typ = typ.clone();
                }
            }
            valid(nt.typ.clone())
        }
        Expression::OptionalToBool(ref mut inner) => {
            let inner_type = type_check_expression(ctx, inner, None, target)?;
            if !inner_type.is_optional() {
                type_error_result(&inner.span(), "Expecting optional type")
            } else {
                valid(Type::Bool)
            }
        }
        Expression::ToOptional(ref mut t) => {
            type_check_expression(ctx, &mut t.inner, None, target)?;
            valid(t.optional_type.clone())
        }
        Expression::Cast(ref mut t) => type_check_cast(ctx, t, target),
        Expression::CompilerCall(ref mut cc) => type_check_compiler_call(ctx, cc, type_hint, target),
        Expression::IndexOperation(ref mut iop) => valid(type_check_index_operation(ctx, iop, target)?),
        Expression::Return(ref mut r) => {
            if let Some(return_type) = ctx.get_function_return_type() {
                type_check_with_conversion(ctx, &mut r.expression, &return_type, target)?;
                valid(return_type)
            } else {
                type_error_result(&r.span, "return expression outside of a function")
            }
        }
    };

    match type_check_result {
        Ok(TypeCheckAction::Valid(typ)) => Ok(typ),
        Ok(TypeCheckAction::ReplaceBy(expr)) => {
            *e = expr;
            type_check_expression(ctx, e, type_hint, target)
        }
        Err(e) => Err(e),
    }
}

pub fn type_check_module(module: &mut Module, target: &Target, imports: &ImportMap) -> CompileResult<()> {
    loop {
        let mut ctx = TypeCheckerContext::new(ImportSymbolResolver::ImportMap(imports));
        resolve_types(&mut ctx, module, target)?;

        for global in module.globals.values_mut() {
            if global.typ == Type::Unknown {
                global.typ = type_check_expression(&mut ctx, &mut global.init, None, target)?;
                ctx.add(Symbol::new(
                    &global.name,
                    &global.typ,
                    global.mutable,
                    &global.span,
                    SymbolType::Global,
                ))?;
            }
        }

        for f in module.functions.values_mut() {
            if !f.type_checked {
                type_check_function(&mut ctx, f, target)?;
            }
        }

        let count = module.functions.len();
        instantiate_generics(module, &mut ctx, imports, target)?;
        // As long as we are adding new generic functions, we need to type check the module again
        if count == module.functions.len() {
            break;
        }
    }

    module.type_checked = true;
    Ok(())
}

/*
pub fn type_check_module(module: &mut Module, target: &Target, imports: &ImportMap) -> CompileResult<()>
{
    match type_check_module2(module, target, imports) {
        Ok(()) => Ok(()),
        Err(e) => {
            println!("Dumping AST:");
            module.print(0);
            Err(e)
        }
    }
}
*/
