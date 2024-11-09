use super::destructors::create_destructors;
use super::genericmapper::fill_in_generics;
use super::instantiate::make_concrete;
use super::instantiategenerics::instantiate_generics;
use super::matchchecker::check_match_is_exhaustive;
use super::patterns::type_check_pattern_match;
use super::typecheckercontext::{ImportSymbolResolver, TypeCheckerContext};
use super::typeresolver::{resolve_type, resolve_types, TypeResolved};
use crate::ast::*;
use crate::compileerror::{
    type_error, type_error_result, unknown_name, unknown_type_result, CompileError, CompileResult,
};
use crate::span::Span;
use crate::target::Target;
use std::ops::Deref;
use std::rc::Rc;

#[derive(Debug)]
pub enum TypeCheckAction {
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

pub type TypeCheckResult = CompileResult<TypeCheckAction>;

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
) -> CompileResult<Type> {
    if *dst_type == *src_type {
        return Ok(src_type.clone());
    }

    let mut converted = false;
    if let Expression::Literal(lit) = expr {
        if let Some(new_lit) = lit.try_convert(dst_type) {
            *lit = new_lit;
            converted = true;
        }
    }

    if !converted {
        if let Some(new_expression) = dst_type.convert(src_type, expr)? {
            *expr = new_expression;
            converted = true;
        }
    }

    if converted {
        let typ = type_check_expression(ctx, expr, None, target)?;
        if &typ != dst_type {
            type_error_result(
                &expr.span(),
                format!("Type conversion from {} to {} failed", src_type, dst_type),
            )
        } else {
            Ok(typ)
        }
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

fn try_result_to_match(inner: Expression, span: Span) -> Expression {
    let typ = inner.get_type();
    let b = bindings(
        vec![binding(
            BindingType::Name("$try".into()),
            inner,
            false,
            typ,
            span.clone(),
        )],
        span.clone(),
    );
    let match_cases = vec![
        match_case(
            ok_pattern(name_pattern("$ok", span.clone()), span.clone()),
            name_expr("$ok", span.clone()),
            span.clone(),
        ),
        match_case(
            Pattern::Any(span.clone()),
            return_expr(name_expr("$try", span.clone()), span.clone()),
            span.clone(),
        ),
    ];
    let match_expr = match_expression(name_expr("$try", span.clone()), match_cases, span.clone());
    block_expr(vec![b, match_expr], span)
}

fn try_optional_to_match(inner: Expression, span: Span) -> Expression {
    let match_cases = vec![
        match_case(
            optional_pattern("$ok".into(), span.clone()),
            name_expr("$ok", span.clone()),
            span.clone(),
        ),
        match_case(
            Pattern::Nil(span.clone()),
            return_expr(nil_expr(span.clone()), span.clone()),
            span.clone(),
        ),
    ];
    match_expression(inner, match_cases, span)
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

        UnaryOperator::TryResult => {
            let e_type = type_check_expression(ctx, &mut u.expression, None, target)?;
            if let Type::Result(_rt) = e_type {
                replace_by(try_result_to_match(u.expression.clone(), u.span.clone()))
            } else {
                type_error_result(&u.span, "Postfix operator ! can only be applied to result types")
            }
        }
        UnaryOperator::TryOptional => {
            let e_type = type_check_expression(ctx, &mut u.expression, None, target)?;
            if let Type::Optional(_rt) = e_type {
                replace_by(try_optional_to_match(u.expression.clone(), u.span.clone()))
            } else {
                type_error_result(&u.span, "Postfix operator ? can only be applied to optional types")
            }
        }
    }
}

pub fn type_check_with_conversion(
    ctx: &mut TypeCheckerContext,
    e: &mut Expression,
    expected_type: &Type,
    target: &Target,
) -> CompileResult<Type> {
    let typ = type_check_expression(ctx, e, Some(expected_type), target)?;
    if ends_with_early_return(e) {
        Ok(expected_type.clone())
    } else {
        convert_type(ctx, expected_type, &typ, e, target)
    }
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

    let new_left_type = b.left.get_type();
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

    if left_type.is_pointer() {
        if !matches!(&b.operator, BinaryOperator::Add | BinaryOperator::Sub) {
            return type_error_result(
                &b.span,
                format!("Cannot do pointer arithmetic with operator {}", b.operator),
            );
        }

        if !matches!(&right_type, Type::Int(_) | Type::UInt(_)) {
            return type_error_result(&b.span, format!("Cannot do pointer arithmetic with type {right_type}"));
        }

        b.typ = left_type.clone();
        return valid(left_type);
    }

    match b.operator {
        BinaryOperator::Add | BinaryOperator::Sub | BinaryOperator::Mul | BinaryOperator::Div | BinaryOperator::Mod => {
            basic_bin_op_checks(ctx, b, left_type, right_type, target)?;
            b.typ = b.left.get_type();
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
            if left_type.is_optional_of(&right_type) || left_type.is_result_of(&right_type) {
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
                if b.operator == BinaryOperator::Equals {
                    return replace_by(unary_op(
                        UnaryOperator::Not,
                        Expression::OptionalToBool(Box::new(b.left.clone())),
                        b.span.clone(),
                    ));
                } else {
                    return replace_by(Expression::OptionalToBool(Box::new(b.left.clone())));
                };
            } else if right_type.is_optional() && left_type.is_optional_of(&Type::Unknown) {
                if b.operator == BinaryOperator::Equals {
                    return replace_by(unary_op(
                        UnaryOperator::Not,
                        Expression::OptionalToBool(Box::new(b.right.clone())),
                        b.span.clone(),
                    ));
                } else {
                    return replace_by(Expression::OptionalToBool(Box::new(b.right.clone())));
                };
            } else {
                basic_bin_op_checks(ctx, b, left_type, right_type, target)?;
            }
            b.typ = Type::Bool;
            valid(Type::Bool)
        }
        _ => type_error_result(&b.span, format!("Operator {} is not a binary operator", b.operator)),
    }
}

pub fn type_check_array_literal(
    ctx: &mut TypeCheckerContext,
    a: &mut ArrayLiteral,
    target: &Target,
) -> TypeCheckResult {
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
            let expected_arg_type = make_concrete(&c.generic_args, &expected_arg_type.typ, &arg.span())?;
            let arg_type = type_check_expression(ctx, arg, Some(&expected_arg_type), target)?;
            let arg_type = make_concrete(&c.generic_args, &arg_type, &arg.span())?;

            if expected_arg_type.is_generic() {
                fill_in_generics(&arg_type, &expected_arg_type, &mut c.generic_args, &arg.span())?;
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

    c.callee.name = resolved.name.clone();
    c.callee.typ = resolved.typ.clone();
    if let Type::Func(ft) = &resolved.typ {
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
            let expected_arg_type = make_concrete(&c.generic_args, &ft.args[idx].typ, &arg.span())?;
            let arg_type = &arg_types[idx];
            convert_type(ctx, &expected_arg_type, arg_type, arg, target)?;
        }

        if ft.return_type.is_generic() {
            c.return_type = make_concrete(&c.generic_args, &ft.return_type, &c.span)?;
            c.function_type = make_concrete(&c.generic_args, &resolved.typ, &c.span)?;
            return valid(c.return_type.clone());
        }
        c.return_type = ft.return_type.clone();
        c.function_type = Type::Func(ft.clone());
        valid(c.return_type.clone())
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
        Err(CompileError::UnknownType(name, expected_type)) => {
            update_binding_type(ctx, &mut fun.expression, &name, &expected_type, target)?;
            type_check_expression(ctx, &mut fun.expression, Some(&fun.sig.return_type), target)?
        }
        Err(e) => return Err(e),
        Ok(typ) => typ,
    };

    ctx.exit_scope();
    if et != fun.sig.return_type && !ends_with_early_return(&fun.expression) {
        if let Some(expression) = fun.sig.return_type.convert(&et, &fun.expression)? {
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
    match e {
        Expression::Dereference(d) => is_result_mutable(ctx, &d.inner),
        Expression::AddressOf(a) => is_result_mutable(ctx, &a.inner),

        Expression::NameRef(nr) => ctx.resolve(&nr.name).map(|rn| rn.mutable).unwrap_or(false),

        Expression::MemberAccess(ma) => is_result_mutable(ctx, &ma.left),

        _ => false,
    }
}

fn type_check_match(ctx: &mut TypeCheckerContext, m: &mut MatchExpression, target: &Target) -> TypeCheckResult {
    let target_type = type_check_expression(ctx, &mut m.target, None, target)?;
    let target_is_mutable = is_result_mutable(ctx, &m.target);
    let mut return_type = Type::Unknown;

    for c in &mut m.cases {
        let case_type = type_check_pattern_match(
            ctx,
            &mut c.pattern,
            &mut c.to_execute,
            &target_type,
            &return_type,
            target_is_mutable,
            target,
        )?;
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
                    .all(|(ma, mb)| is_instantiation_of(&ma.typ, &mb.typ))
        }
        (Type::Sum(a), Type::Sum(b)) => a
            .cases
            .iter()
            .zip(b.cases.iter())
            .all(|(ma, mb)| match (&ma.typ, &mb.typ) {
                (Some(mat), Some(mbt)) => is_instantiation_of(mat, mbt),
                (None, None) => true,
                _ => false,
            }),
        _ => false,
    }
}

pub fn type_check_name(ctx: &mut TypeCheckerContext, nr: &mut NameRef, type_hint: Option<&Type>) -> TypeCheckResult {
    if nr.name == "_" {
        return valid(Type::Unknown);
    }

    if !nr.typ.is_unknown() && !nr.typ.is_generic() {
        return valid(nr.typ.clone()); // We have already determined the type
    }

    let resolved = ctx
        .resolve(&nr.name)
        .ok_or_else(|| unknown_name(&nr.span, format!("Unknown name {}", nr.name)))?;
    nr.name = resolved.name.clone();
    if let Some(typ) = type_hint {
        if resolved.typ == Type::Unknown {
            return unknown_type_result(&nr.name, typ);
        }

        if resolved.typ == *typ {
            nr.typ = resolved.typ.clone();
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
            nr.typ = resolved.typ.clone();
            valid(nr.typ.clone())
        }
    } else {
        nr.typ = resolved.typ.clone();
        valid(nr.typ.clone())
    }
}

pub fn add_struct_bindings(
    ctx: &mut TypeCheckerContext,
    b: &mut StructPattern,
    struct_type: &StructType,
    mutable: bool,
) -> CompileResult<()> {
    for binding in b.bindings.iter_mut() {
        let (member_type, _) = struct_type
            .get_member_type_and_index(&binding.name)
            .ok_or_else(|| type_error(&b.span, format!("Struct has no member named {}", binding.name)))?;

        let mutable = match binding.mode {
            StructPatternBindingMode::Value => {
                binding.typ = member_type;
                false
            }

            StructPatternBindingMode::Pointer => {
                binding.typ = ptr_type(member_type);
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
    if b.typ != Type::Unknown {
        if resolve_type(ctx, &mut b.typ) == TypeResolved::No {
            return type_error_result(&b.span, format!("Cannot resolve type of binding {}", b.name()));
        }

        type_check_with_conversion(ctx, &mut b.init, &b.typ, target)?;
    } else {
        b.typ = type_check_expression(ctx, &mut b.init, None, target)?;
    }

    match &mut b.binding_type {
        BindingType::Name(name) => {
            ctx.add(Symbol::new(name, &b.typ, b.mutable, &b.span, SymbolType::Normal))?;
        }

        BindingType::Struct(s) => {
            s.typ = b.typ.clone();

            if let Type::Struct(st) = &mut b.typ {
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
        if let Expression::Bindings(bl) = e {
            for b in &mut bl.bindings {
                if let BindingType::Name(b_name) = &b.binding_type {
                    if b_name == name {
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

pub fn ends_with_early_return(e: &Expression) -> bool {
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
    let on_false_type = if let Some(expr) = &mut i.on_false {
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
            if let Some(expr) = &mut i.on_false {
                type_check_with_conversion(ctx, expr, &optional_type, target)?;
            }
            type_check_with_conversion(ctx, &mut i.on_true, &optional_type, target)?;
            i.typ = optional_type.clone();
            valid(optional_type)
        } else if on_false_type.is_optional_of(&on_true_type) || on_false_type.is_optional_of(&Type::Unknown) {
            let optional_type = optional_type(on_true_type);
            if let Some(expr) = &mut i.on_false {
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
                "Type {} has {} members, but initializing {} members",
                st.get_name(),
                st.members.len(),
                si.member_initializers.len()
            ),
        );
    }

    let mut new_members = Vec::with_capacity(st.members.len());

    for mi in si.member_initializers.iter_mut() {
        let (expected_type, idx) = st.get_member_type_and_index(&mi.name).ok_or_else(|| {
            type_error(
                &mi.initializer.span(),
                format!("Struct {} has no member {}", st.get_name(), mi.name),
            )
        })?;
        mi.member_idx = idx;

        let t = type_check_expression(ctx, &mut mi.initializer, Some(&expected_type), target)?;

        let expected_type = if expected_type.is_generic() {
            fill_in_generics(&t, &expected_type, &mut si.generic_args, &mi.initializer.span())?
        } else {
            expected_type
        };

        if t != expected_type {
            if let Some(new_mi) = expected_type.convert(&t, &mi.initializer)? {
                mi.initializer = new_mi;
            } else {
                return type_error_result(
                    &mi.initializer.span(),
                    format!(
                        "Attempting to initialize member {} with type '{}', expecting an expression of type '{}'",
                        idx, t, expected_type
                    ),
                );
            }
        }

        new_members.push(struct_member(mi.name.clone(), expected_type));
    }

    si.member_initializers
        .sort_by(|a, b| a.member_idx.cmp(&b.member_idx));

    Ok(struct_type(si.struct_name.clone(), new_members, st.implements.clone()))
}

fn type_check_anonymous_struct_initializer(
    ctx: &mut TypeCheckerContext,
    si: &mut StructInitializer,
    target: &Target,
) -> TypeCheckResult {
    let mut new_members = Vec::with_capacity(si.member_initializers.len());
    for mi in si.member_initializers.iter_mut() {
        let t = type_check_expression(ctx, &mut mi.initializer, None, target)?;
        new_members.push(struct_member(mi.name.clone(), t));
    }
    si.typ = struct_type(None, new_members, Vec::new());
    valid(si.typ.clone())
}

fn type_check_struct_initializer(
    ctx: &mut TypeCheckerContext,
    si: &mut StructInitializer,
    target: &Target,
) -> TypeCheckResult {
    let name = if let Some(name) = &si.struct_name {
        name.clone()
    } else {
        return type_check_anonymous_struct_initializer(ctx, si, target);
    };

    let resolved = ctx
        .resolve(&name)
        .ok_or_else(|| unknown_name(&si.span, format!("Unknown struct {}", name)))?;
    si.struct_name = Some(resolved.name.clone());
    si.typ = resolved.typ.clone();
    match &resolved.typ {
        Type::Struct(st) => {
            si.typ = type_check_struct_members_in_initializer(ctx, st, si, target)?;
            valid(si.typ.clone())
        }
        Type::Sum(st) => {
            let idx = st.index_of(&resolved.name).ok_or_else(|| {
                type_error(
                    &si.span,
                    format!("Cannot determine index of sum type case {}", resolved.name),
                )
            })?;
            let mut sum_type_cases = Vec::with_capacity(st.cases.len());
            for (i, case) in st.cases.iter().enumerate() {
                let typ = if i == idx {
                    match &case.typ {
                        Some(Type::Struct(s)) => Some(type_check_struct_members_in_initializer(ctx, s, si, target)?),
                        None => None,
                        _ => return type_error_result(&si.span, "Invalid sum type case"),
                    }
                } else {
                    case.typ.clone()
                };
                sum_type_cases.push(sum_type_case(&case.name, typ))
            }

            si.typ = sum_type(&st.name, sum_type_cases, st.implements.clone());
            valid(si.typ.clone())
        }

        Type::String => {
            let string_rep = string_type_representation(target.int_size);
            type_check_struct_members_in_initializer(ctx, &string_rep, si, target)?;
            si.typ = Type::String;
            valid(Type::String)
        }

        _ => type_error_result(&si.span, format!("{} is not a struct", resolved.name)),
    }
}

fn find_member_type(members: &[StructMember], member_name: &str, span: &Span) -> CompileResult<(usize, Type)> {
    members
        .iter()
        .enumerate()
        .find(|&(_, m)| m.name == member_name)
        .map(|(idx, m)| (idx, m.typ.clone()))
        .ok_or_else(|| unknown_name(span, format!("Unknown struct member {}", member_name)))
}

fn member_call_to_call(left: &Expression, call: &Call) -> Expression {
    let mut args = Vec::with_capacity(call.args.len() + 1);
    let first_arg = match left.get_type() {
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
        if let Type::Interface(it) = interface {
            for func in &it.functions {
                if func.name == call.callee.name {
                    return Some(func.return_type.clone());
                }
            }
        }
        None
    };

    match gt {
        GenericType::Any(name) => {
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

        GenericType::Restricted(interfaces) => {
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
    let (Expression::NameRef(nr), MemberAccessType::Call(call)) = (&sma.left, &sma.right) else {
        return None;
    };
    let call_name = format!("{}.{}", nr.name, call.callee.name);
    let rn = ctx.resolve(&call_name)?;
    let Type::Func(_) = &rn.typ else {
        return None;
    };

    let name_span = Span::merge(&nr.span, &call.callee.span);
    let full_span = Span::merge(&name_span, &call.span);
    Some(Call::new(
        NameRef::new(call_name, name_span),
        call.args.clone(),
        full_span,
    ))
}

fn type_check_member_access(ctx: &mut TypeCheckerContext, sma: &mut MemberAccess, target: &Target) -> TypeCheckResult {
    let left_type = type_check_expression(ctx, &mut sma.left, None, target)?;
    // member access through pointer is the same as a normal member access
    let left_type_ref = if let Type::Pointer(inner) = &left_type {
        inner.deref()
    } else {
        &left_type
    };

    if let Some(call) = to_static_function_call(ctx, sma) {
        return replace_by(Expression::Call(Box::new(call)));
    }

    let (typ, new_right) = match (&mut sma.right, &left_type_ref) {
        (MemberAccessType::Property(Property::Len), Type::Slice(_))
        | (MemberAccessType::Property(Property::Len), Type::Array(_))
        | (MemberAccessType::Property(Property::Len), Type::String) => (target.native_uint_type.clone(), None),

        (MemberAccessType::Property(Property::Data), Type::String) => (ptr_type(Type::UInt(IntSize::I8)), None),

        (MemberAccessType::Property(Property::Data), Type::Slice(st)) => (ptr_type(st.element_type.clone()), None),

        (MemberAccessType::Name(field), Type::Struct(st)) => {
            let (member_idx, member_type) = find_member_type(&st.members, &field.name, &sma.span)?;
            field.index = member_idx;
            field.typ = member_type.clone();
            (member_type, None)
        }

        (MemberAccessType::Name(field), &Type::Array(_))
        | (MemberAccessType::Name(field), &Type::Slice(_))
        | (MemberAccessType::Name(field), &Type::String) => {
            if let Some((typ, member_access_type)) = left_type.get_property_type(&field.name, target) {
                (typ, Some(member_access_type))
            } else {
                return type_error_result(
                    &sma.span,
                    format!("Type '{}' has no property named '{}'", left_type, field.name),
                );
            }
        }

        (MemberAccessType::Call(call), Type::Struct(st)) => {
            let Some(name) = &st.name else {
                return type_error_result(&call.span, "Calling member of anonymous struct is not possible");
            };
            let call_name = format!("{}.{}", name, call.callee.name);
            call.callee.name = call_name;
            return replace_by(member_call_to_call(&sma.left, call));
        }

        (MemberAccessType::Call(call), Type::Sum(st)) => {
            let call_name = format!("{}.{}", st.name, call.callee.name);
            call.callee.name = call_name;
            return replace_by(member_call_to_call(&sma.left, call));
        }

        (MemberAccessType::Call(call), Type::Generic(gt)) => (type_check_generic_member_call(ctx, call, gt)?, None),

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
            if let Expression::Return(r) = e {
                b.typ = r.expression.get_type();
            } else {
                b.typ = typ;
            }
        }
    }

    if b.deferred_expressions.is_empty() {
        ctx.add_destructors(b)?;
    }

    for e in &mut b.deferred_expressions {
        type_check_expression(ctx, e, type_hint, target)?;
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
    match d {
        DeleteExpression::Delete { inner, span } => {
            let inner_typ = type_check_expression(ctx, inner, type_hint, target)?;
            let Type::Pointer(typ) = &inner_typ else {
                return type_error_result(
                    span,
                    format!(
                        "delete expression expects a pointer argument, argument has type {}",
                        inner_typ
                    ),
                );
            };

            let Some(dc) = ctx.get_destructor_call(name_expr("$del", span.clone()), None, typ)? else {
                return valid(Type::Void);
            };

            let binding = binding_expr("$del", inner.clone(), false, inner.span());
            let new_del = Expression::Delete(Box::new(DeleteExpression::Delete {
                inner: name_expr("$del", span.clone()),
                span: span.clone(),
            }));

            let b = block(vec![binding, dc, new_del], Span::default());
            replace_by(Expression::Delete(Box::new(DeleteExpression::BlockWithDestructor {
                span: span.clone(),
                block: b,
            })))
        }
        DeleteExpression::BlockWithDestructor { block, .. } => type_check_block(ctx, block, None, target),
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
        Type::Int(_) | Type::UInt(_) | Type::Range(_) => (),
        _ => {
            return type_error_result(
                &iop.span,
                format!(
                "An expression of type {}, cannot be used to index something. Only integers and ranges are supported.",
                index_type
            ),
            )
        }
    }

    let index_not_allowed =
        || type_error_result(&iop.span, format!("Cannot index an expression of type {}", target_type));

    let typ = match &target_type {
        Type::Slice(st) => st.element_type.clone(),
        Type::Array(at) => at.element_type.clone(),
        Type::Pointer(pt) => match pt.deref() {
            Type::Slice(st) => st.element_type.clone(),
            Type::Array(at) => at.element_type.clone(),
            pt => pt.clone(),
        },
        _ => return index_not_allowed(),
    };

    if let Type::Range(_) = index_type {
        iop.typ = slice_type(typ);
        Ok(iop.typ.clone())
    } else {
        iop.typ = typ.clone();
        Ok(typ)
    }
}

fn to_regular_assign(a: &mut Assign) {
    let op = match a.operator {
        AssignOperator::Assign => return,
        AssignOperator::Add => BinaryOperator::Add,
        AssignOperator::Sub => BinaryOperator::Sub,
        AssignOperator::Mul => BinaryOperator::Mul,
        AssignOperator::Div => BinaryOperator::Div,
        AssignOperator::And => BinaryOperator::And,
        AssignOperator::Or => BinaryOperator::Or,
    };

    let left = match &a.left {
        AssignTarget::Var(nr) => Expression::NameRef(nr.clone()),
        AssignTarget::MemberAccess(ma) => Expression::MemberAccess(Box::new(ma.clone())),
        AssignTarget::Dereference(d) => Expression::Dereference(Box::new(d.clone())),
        AssignTarget::IndexOperation(i) => Expression::IndexOperation(Box::new(i.clone())),
    };

    let right = bin_op_with_type(op, left, a.right.clone(), a.right.span(), a.right.get_type());

    a.operator = AssignOperator::Assign;
    a.right = right;
}

fn type_check_assign(ctx: &mut TypeCheckerContext, a: &mut Assign, target: &Target) -> TypeCheckResult {
    let dst_type = match &mut a.left {
        AssignTarget::Var(nr) => {
            type_check_name(ctx, nr, None)?;
            if let Some(rn) = ctx.resolve(&nr.name) {
                if !rn.mutable {
                    return type_error_result(
                        &nr.span,
                        format!("Attempting to modify non mutable variable {}", nr.name),
                    );
                }
            }
            nr.typ.clone()
        }

        AssignTarget::MemberAccess(ma) => {
            type_check_member_access(ctx, ma, target)?;
            if !is_result_mutable(ctx, &ma.left) {
                return type_error_result(&ma.span, "Attempting to modify non mutable expression");
            }
            ma.typ.clone()
        }

        AssignTarget::Dereference(d) => {
            type_check_dereference(ctx, d, target)?;
            if !is_result_mutable(ctx, &d.inner) {
                return type_error_result(&d.span, "Attempting to modify non mutable expression");
            }
            d.typ.clone()
        }

        AssignTarget::IndexOperation(iop) => {
            let typ = type_check_index_operation(ctx, iop, target)?;
            if let Type::Range(_) = typ {
                return type_error_result(&iop.span, "Assigning to a ranged indexing target is not allowed");
            }
            typ
        }
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
    to_regular_assign(a); // Convert to a regular assign, for code generation
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
        Type::Range(it) => {
            let Expression::Range(r) = &f.iterable else {
                panic!("Expecting range expression here");
            };

            if r.start.is_none() || r.end.is_none() {
                return type_error_result(
                    &r.span,
                    "Range start and end must be provided if the range is used in a for loop",
                );
            }

            ctx.enter_scope(None);
            f.loop_variable_type = it.deref().clone();
            ctx.add(Symbol::new(
                &f.loop_variable,
                &f.loop_variable_type,
                false,
                &f.span,
                SymbolType::Normal,
            ))?;
            type_check_expression(ctx, &mut f.body, None, target)?;
            ctx.exit_scope();
            valid(Type::Void)
        }
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
                &f.loop_variable_type,
                false,
                &f.span,
                SymbolType::Normal,
            ))?;
            type_check_expression(ctx, &mut f.body, None, target)?;
            ctx.exit_scope();
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
        (Type::Pointer(from), &Type::Pointer(_)) if *from.deref() == Type::Void => valid(c.destination_type.clone()),
        (Type::Pointer(_), &Type::Bool) => valid(Type::Bool),
        (Type::Array(at), Type::Pointer(to)) if at.element_type == *to.deref() => valid(c.destination_type.clone()),
        (inner_type, _) => type_error_result(
            &c.span,
            format!(
                "Cast from type {} to type {} is not allowed",
                inner_type, c.destination_type
            ),
        ),
    }
}

fn type_check_compiler_call(ctx: &mut TypeCheckerContext, cc: &mut CompilerCall, target: &Target) -> TypeCheckResult {
    match cc {
        CompilerCall::SizeOf { typ, span, .. } => {
            if resolve_type(ctx, typ) == TypeResolved::No {
                type_error_result(span, format!("Unable to resolve type {}", typ))
            } else {
                valid(target.native_uint_type.clone())
            }
        }
        CompilerCall::Drop {
            obj,
            span,
            destructor_call,
            drop_flag,
        } => {
            let typ = type_check_expression(ctx, obj, None, target)?;
            let Type::Pointer(inner) = &typ else {
                return type_error_result(span, format!("@drop calls expect a pointer as argument, got a {typ}"));
            };

            match destructor_call {
                Some(ds) => {
                    type_check_call(ctx, ds, target)?;
                    valid(Type::Void)
                }
                None => {
                    let df = if let Some(df) = drop_flag {
                        Some(df.clone())
                    } else {
                        ctx.get_drop_flag(obj)
                    };

                    if let Some(ds) = ctx.get_destructor_call(obj.clone(), df, inner)? {
                        replace_by(ds)
                    } else {
                        valid(Type::Void)
                    }
                }
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
    match lit {
        Literal::Array(a) => type_check_array_literal(ctx, a, target),

        Literal::NullPtr(span, typ) => {
            if let Some(Type::Pointer(inner_type)) = type_hint {
                *typ = inner_type.ptr_of();
                valid(typ.clone())
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

fn type_check_range(ctx: &mut TypeCheckerContext, r: &mut Range, target: &Target) -> TypeCheckResult {
    let start_type = if let Some(start) = &mut r.start {
        type_check_expression(ctx, start, None, target)?
    } else {
        target.native_int_type.clone()
    };

    let end_type = if let Some(end) = &mut r.end {
        type_check_expression(ctx, end, None, target)?
    } else {
        target.native_int_type.clone()
    };

    match (&start_type, &end_type) {
        (Type::UInt(s), Type::UInt(e)) => {
            // Take biggest int IntSize
            r.typ = if s.size_in_bits() > e.size_in_bits() {
                Type::Range(Rc::new(Type::UInt(*s)))
            } else {
                Type::Range(Rc::new(Type::UInt(*e)))
            };
            valid(r.typ.clone())
        }
        (Type::Int(s), Type::Int(e)) => {
            // Take biggest int IntSize
            r.typ = if s.size_in_bits() > e.size_in_bits() {
                Type::Range(Rc::new(Type::Int(*s)))
            } else {
                Type::Range(Rc::new(Type::Int(*e)))
            };
            valid(r.typ.clone())
        }
        (Type::Int(s), Type::UInt(e)) => {
            let is = if s.size_in_bits() > e.size_in_bits() { *s } else { *e };
            let Some(start) = &mut r.start else {
                r.typ = range_type(Type::UInt(is));
                return valid(r.typ.clone());
            };

            let Some(end) = &mut r.end else {
                r.typ = range_type(Type::Int(is));
                return valid(r.typ.clone());
            };

            if let Ok(_typ) = type_check_with_conversion(ctx, end, &Type::Int(is), target) {
                r.typ = range_type(Type::Int(is));
                valid(r.typ.clone())
            } else if let Ok(_typ) = type_check_with_conversion(ctx, start, &Type::UInt(is), target) {
                r.typ = range_type(Type::UInt(is));
                valid(r.typ.clone())
            } else {
                type_error_result(
                    &r.span,
                    format!(
                        "Range start and must have the same integer type, start has {} and end has {}",
                        start_type, end_type
                    ),
                )
            }
        }
        (Type::UInt(s), Type::Int(e)) => {
            let is = if s.size_in_bits() > e.size_in_bits() { *s } else { *e };
            let Some(start) = &mut r.start else {
                r.typ = range_type(Type::Int(is));
                return valid(r.typ.clone());
            };

            let Some(end) = &mut r.end else {
                r.typ = range_type(Type::UInt(is));
                return valid(r.typ.clone());
            };

            if let Ok(_typ) = type_check_with_conversion(ctx, end, &Type::UInt(is), target) {
                r.typ = range_type(Type::UInt(is));
                valid(r.typ.clone())
            } else if let Ok(_typ) = type_check_with_conversion(ctx, start, &Type::Int(is), target) {
                r.typ = range_type(Type::Int(is));
                valid(r.typ.clone())
            } else {
                type_error_result(
                    &r.span,
                    format!(
                        "Range start and must have the same integer type, start has {} and end has {}",
                        start_type, end_type
                    ),
                )
            }
        }
        _ => type_error_result(
            &r.span,
            format!("Invalid types for the start and end of a range (got {start_type} and {end_type})"),
        ),
    }
}

fn type_check_to_optional(ctx: &mut TypeCheckerContext, t: &mut ToOptional, target: &Target) -> TypeCheckResult {
    if t.optional_type.is_unknown() {
        let typ = type_check_expression(ctx, &mut t.inner, None, target)?;
        t.optional_type = optional_type(typ);
        return valid(t.optional_type.clone());
    }

    let Type::Optional(it) = &t.optional_type else {
        return type_error_result(&t.inner.span(), "Expecting optional type");
    };
    let typ = type_check_expression(ctx, &mut t.inner, Some(it), target)?;
    if !t.optional_type.is_optional_of(&typ) {
        type_error_result(&t.inner.span(), format!("Expecting an expression of type '{}' ", it))
    } else {
        valid(t.optional_type.clone())
    }
}

fn type_check_to_ok_result(
    ctx: &mut TypeCheckerContext,
    e: &mut ToOkResult,
    type_hint: Option<&Type>,
    target: &Target,
) -> TypeCheckResult {
    if e.result_type.is_unknown() {
        let typ = type_check_expression(ctx, &mut e.inner, None, target)?;
        if let Some(Type::Result(ert)) = type_hint {
            if ert.ok_typ == typ {
                e.result_type = Type::Result(ert.clone());
                return valid(e.result_type.clone());
            } else {
                return type_error_result(
                    &e.inner.span(),
                    format!(
                        "Expecting {} for the ok type of a result, but found {}",
                        ert.ok_typ, typ
                    ),
                );
            }
        } else {
            return type_error_result(
                &e.inner.span(),
                "Cannot infer error type of result type, please provide a type hint",
            );
        }
    }

    let Type::Result(rt) = &e.result_type else {
        return type_error_result(&e.inner.span(), "Expecting result type");
    };
    let ok_typ = type_check_expression(ctx, &mut e.inner, Some(&rt.ok_typ), target)?;
    if ok_typ != rt.ok_typ {
        type_error_result(
            &e.inner.span(),
            format!("Expecting an expression of type '{}' ", e.result_type),
        )
    } else {
        valid(e.result_type.clone())
    }
}

fn type_check_to_err_result(
    ctx: &mut TypeCheckerContext,
    e: &mut ToErrResult,
    type_hint: Option<&Type>,
    target: &Target,
) -> TypeCheckResult {
    if e.result_type.is_unknown() {
        let typ = type_check_expression(ctx, &mut e.inner, None, target)?;
        if let Some(Type::Result(ert)) = type_hint {
            if ert.err_typ == typ {
                e.result_type = Type::Result(ert.clone());
                return valid(e.result_type.clone());
            } else {
                return type_error_result(
                    &e.inner.span(),
                    format!(
                        "Expecting {} for the error type of a result, but found {}",
                        ert.err_typ, typ
                    ),
                );
            }
        } else {
            return type_error_result(
                &e.inner.span(),
                "Cannot infer ok type of result type, please provide a type hint",
            );
        }
    }
    let Type::Result(rt) = &e.result_type else {
        return type_error_result(&e.inner.span(), "Expecting result type");
    };
    let err_typ = type_check_expression(ctx, &mut e.inner, Some(&rt.err_typ), target)?;
    if err_typ != rt.err_typ {
        type_error_result(
            &e.inner.span(),
            format!("Expecting an expression of type '{}' ", e.result_type),
        )
    } else {
        valid(e.result_type.clone())
    }
}

pub fn type_check_expression(
    ctx: &mut TypeCheckerContext,
    e: &mut Expression,
    type_hint: Option<&Type>,
    target: &Target,
) -> CompileResult<Type> {
    let type_check_result = match e {
        Expression::UnaryOp(op) => type_check_unary_op(ctx, op, target),
        Expression::BinaryOp(op) => type_check_binary_op(ctx, op, target),
        Expression::Literal(lit) => type_check_literal(ctx, lit, type_hint, target),
        Expression::Call(c) => type_check_call(ctx, c, target),
        Expression::NameRef(nr) => type_check_name(ctx, nr, type_hint),
        Expression::Match(m) => type_check_match(ctx, m, target),
        Expression::Lambda(l) => type_check_lambda(ctx, l, type_hint, target),
        Expression::Bindings(l) => {
            for b in &mut l.bindings {
                type_check_binding(ctx, b, target)?;
            }
            valid(Type::Void)
        }
        Expression::If(i) => type_check_if(ctx, i, type_hint, target),
        Expression::Block(b) => type_check_block(ctx, b, type_hint, target),
        Expression::StructInitializer(si) => type_check_struct_initializer(ctx, si, target),
        Expression::MemberAccess(sma) => type_check_member_access(ctx, sma, target),
        Expression::New(n) => type_check_new(ctx, n, type_hint, target),
        Expression::Delete(d) => type_check_delete(ctx, d, type_hint, target),
        Expression::ArrayToSlice(ats) => type_check_array_to_slice(ctx, ats, type_hint, target),
        Expression::AddressOf(a) => type_check_address_of(ctx, a, target),
        Expression::Dereference(d) => type_check_dereference(ctx, d, target),
        Expression::Assign(a) => type_check_assign(ctx, a, target),
        Expression::While(w) => type_check_while(ctx, w, target),
        Expression::For(f) => type_check_for(ctx, f, target),
        Expression::Void => valid(Type::Void),
        Expression::Nil(nt) => {
            if let Some(typ) = type_hint {
                if let Type::Optional(_) = *typ {
                    nt.typ = typ.clone();
                }
            }
            valid(nt.typ.clone())
        }
        Expression::OptionalToBool(inner) => {
            let inner_type = type_check_expression(ctx, inner, None, target)?;
            if !inner_type.is_optional() {
                type_error_result(&inner.span(), "Expecting optional type")
            } else {
                valid(Type::Bool)
            }
        }
        Expression::ToOptional(t) => type_check_to_optional(ctx, t, target),
        Expression::Cast(t) => type_check_cast(ctx, t, target),
        Expression::CompilerCall(cc) => type_check_compiler_call(ctx, cc, target),
        Expression::IndexOperation(iop) => valid(type_check_index_operation(ctx, iop, target)?),
        Expression::Return(r) => {
            if let Some(return_type) = ctx.get_function_return_type() {
                type_check_with_conversion(ctx, &mut r.expression, &return_type, target)?;
                valid(Type::Void)
            } else {
                type_error_result(&r.span, "return expression outside of a function")
            }
        }
        Expression::ResultToBool(e) => {
            let typ = type_check_expression(ctx, e, None, target)?;
            if !matches!(typ, Type::Result(_)) {
                type_error_result(&e.span(), "Expecting result type")
            } else {
                valid(Type::Bool)
            }
        }
        Expression::ToOkResult(e) => type_check_to_ok_result(ctx, e, type_hint, target),
        Expression::ToErrResult(e) => type_check_to_err_result(ctx, e, type_hint, target),
        Expression::Range(r) => type_check_range(ctx, r, target),
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

fn type_check_global_binding(
    ctx: &mut TypeCheckerContext,
    b: &mut GlobalBinding,
    target: &Target,
) -> CompileResult<()> {
    if b.typ != Type::Unknown {
        if resolve_type(ctx, &mut b.typ) == TypeResolved::No {
            return type_error_result(&b.span, format!("Cannot resolve type of binding {}", b.name));
        }
        type_check_with_conversion(ctx, &mut b.init, &b.typ, target)?;
    } else {
        b.typ = type_check_expression(ctx, &mut b.init, None, target)?;
    }
    Ok(())
}

fn type_check<F>(errors: &mut Vec<CompileError>, mut fun: F)
where
    F: FnMut() -> CompileResult<()>,
{
    if let Err(e) = fun() {
        errors.push(e)
    }
}

fn matches_function_signature(
    expected: &Type,
    actual: &Type,
    concrete_type: &Type,
    method_name: &str,
) -> Result<(), String> {
    fn type_matches(expected: &Type, actual: &Type, concrete_type: &Type) -> bool {
        match (expected, actual) {
            (Type::Pointer(e), Type::Pointer(a)) | (Type::Optional(e), Type::Optional(a)) => {
                type_matches(e, a, concrete_type)
            }
            (Type::Array(e), Type::Array(a)) => type_matches(&e.element_type, &a.element_type, concrete_type),
            (Type::Slice(e), Type::Slice(a)) => type_matches(&e.element_type, &a.element_type, concrete_type),
            _ => *expected == *actual || (*expected == Type::SelfType && *actual == *concrete_type),
        }
    }

    match (expected, actual) {
        (Type::Func(e), Type::Func(a)) => {
            if e.args.len() != a.args.len() {
                return Err(format!("Argument count mismatch for method {}", method_name));
            }

            if !type_matches(&e.return_type, &a.return_type, concrete_type) {
                return Err(format!("Return types do not match on method {}", method_name));
            }

            for (idx, (e_arg, a_arg)) in e.args.iter().zip(a.args.iter()).enumerate() {
                if !type_matches(&e_arg.typ, &a_arg.typ, concrete_type) {
                    return Err(format!(
                        "The type of argument {} does not match on method {}.",
                        idx, method_name
                    ));
                }
            }

            Ok(())
        }

        _ => Err(format!(
            "Cannot match function signatures of {} and {} types",
            expected, actual
        )),
    }
}

fn implements_interface(ctx: &TypeCheckerContext, concrete_type: &Type, interface: &Type) -> Result<(), String> {
    let it = if let Type::Interface(it) = interface {
        it
    } else {
        return Err(format!("{} is not an interface type", interface.name()));
    };

    let concrete_type_name = concrete_type.name();
    for func in &it.functions {
        let r = ctx
            .resolve(&format!("{}.{}", concrete_type_name, func.name))
            .ok_or_else(|| format!("No method {} found on type {}", func.name, concrete_type_name))?;

        matches_function_signature(&func.typ, &r.typ, concrete_type, &func.name)?;
    }

    Ok(())
}

fn type_check_implements(module: &Module, ctx: &TypeCheckerContext, errors: &mut Vec<CompileError>) {
    for t in module.types.values() {
        match t {
            TypeDeclaration::Interface(_) => (),
            TypeDeclaration::Struct(sd) => {
                for (impls, span) in &sd.implements {
                    if let Err(e) = implements_interface(ctx, &sd.typ, impls) {
                        errors.push(type_error(span, e));
                    }
                }
            }
            TypeDeclaration::Sum(st) => {
                for (impls, span) in &st.implements {
                    if let Err(e) = implements_interface(ctx, &st.typ, impls) {
                        errors.push(type_error(span, e));
                    }
                }
            }
        }
    }
}

pub fn type_check_module(module: &mut Module, target: &Target, imports: &ImportMap) -> CompileResult<()> {
    let mut destructors_added = false;
    loop {
        let mut errors = Vec::new();
        let mut ctx = TypeCheckerContext::new(ImportSymbolResolver::ImportMap(imports));
        type_check(&mut errors, || resolve_types(&mut ctx, module));

        for global in module.globals.values_mut() {
            type_check(&mut errors, || {
                type_check_global_binding(&mut ctx, global, target)?;
                ctx.add(Symbol::new(
                    &global.name,
                    &global.typ,
                    global.mutable,
                    &global.span,
                    SymbolType::Global,
                ))?;
                Ok(())
            });
        }

        for f in module.functions.values_mut() {
            if !f.type_checked {
                type_check(&mut errors, || type_check_function(&mut ctx, f, target))
            }
        }

        if !errors.is_empty() {
            return CompileResult::Err(CompileError::Many(errors));
        }

        let count = module.functions.len();
        instantiate_generics(module, &mut ctx, imports, target)?;
        // As long as we are adding new generic functions, we need to type check the module again
        if count == module.functions.len() {
            if !destructors_added {
                create_destructors(&mut ctx, module, target)?;
                destructors_added = true;
                //module.print(0);
            }

            if count == module.functions.len() {
                // Final step, check if types implement the things they say they do
                type_check_implements(module, &ctx, &mut errors);
                if !errors.is_empty() {
                    return CompileResult::Err(CompileError::Many(errors));
                }
                break;
            }
        }
    }

    module.type_checked = true;
    Ok(())
}
