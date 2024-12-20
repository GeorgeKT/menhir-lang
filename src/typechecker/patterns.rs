use std::{ops::Deref, rc::Rc};

use crate::{
    ast::{
        slice_type, ArrayPattern, EmptyArrayPattern, ErrorPattern, Expression, Literal, NameRef, OkPattern,
        OptionalPattern, Pattern, ResultType, StructPattern, SumTypeCaseIndexOf, Symbol, SymbolType, Type,
    },
    compileerror::{type_error_result, unknown_name, CompileResult},
    span::Span,
    target::Target,
};

use super::{
    typecheck::{
        add_struct_bindings, ends_with_early_return, type_check_array_literal, type_check_expression, type_check_name,
        type_check_with_conversion,
    },
    typecheckercontext::TypeCheckerContext,
};

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
    match &resolved.typ {
        Type::Sum(st) => {
            let idx = st
                .index_of(&p.name)
                .expect("Internal Compiler Error: cannot determine index of sum type case");
            let case = &st.cases[idx];
            match &case.typ {
                Some(Type::Struct(s)) => {
                    add_struct_bindings(ctx, p, s, target_is_mutable)?;
                    p.typ = Type::Sum(st.clone());
                    Ok(())
                }
                _ => type_error_result(
                    &p.span,
                    "Attempting to pattern match a normal sum type case with a struct",
                ),
            }
        }

        Type::Struct(st) => {
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

fn infer_matched_type(
    ctx: &mut TypeCheckerContext,
    e: &mut Expression,
    return_type: &Type,
    target: &Target,
) -> CompileResult<Type> {
    if *return_type != Type::Unknown {
        let tt = type_check_with_conversion(ctx, e, return_type, target)?;
        if &tt != return_type && !ends_with_early_return(e) {
            type_error_result(&e.span(), "Expressions in match statements must return the same type")
        } else {
            Ok(tt)
        }
    } else {
        type_check_expression(ctx, e, None, target)
    }
}

fn type_check_empty_array_pattern(
    ctx: &mut TypeCheckerContext,
    ap: &EmptyArrayPattern,
    e: &mut Expression,
    return_type: &Type,
    target_type: &Type,
    target: &Target,
) -> CompileResult<Type> {
    if !target_type.is_sequence() {
        return type_error_result(
            &ap.span,
            format!(
                "Attempting to pattern match an expression of type {}, with an empty array",
                target_type
            ),
        );
    }
    infer_matched_type(ctx, e, return_type, target)
}

fn type_check_array_pattern(
    ctx: &mut TypeCheckerContext,
    ap: &ArrayPattern,
    e: &mut Expression,
    return_type: &Type,
    target_type: &Type,
    target: &Target,
) -> CompileResult<Type> {
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
    let ct = infer_matched_type(ctx, e, return_type, target)?;
    ctx.exit_scope();
    Ok(ct)
}

fn type_check_name_pattern(
    ctx: &mut TypeCheckerContext,
    nr: &mut NameRef,
    e: &mut Expression,
    return_type: &Type,
    target_type: &Type,
    target: &Target,
) -> CompileResult<Type> {
    type_check_name(ctx, nr, Some(target_type))?;
    if &nr.typ != target_type && !target_type.is_pointer_to(&nr.typ) {
        return type_error_result(
            &nr.span,
            format!(
                "Cannot pattern match an expression of type:\n{}\nwith an expression of type:\n{}\n",
                target_type, nr.typ
            ),
        );
    }

    match &nr.typ {
        Type::Sum(st) => {
            let idx = st
                .index_of(&nr.name)
                .expect("Internal Compiler Error: cannot determine index of sum type case");
            let case = &st.cases[idx];
            if case.typ.is_none() {
                infer_matched_type(ctx, e, return_type, target)
            } else {
                type_error_result(
                    &nr.span,
                    "Invalid pattern match, match should be with an empty sum case",
                )
            }
        }
        Type::Enum(_) => infer_matched_type(ctx, e, return_type, target),
        _ => type_error_result(&nr.span, "Invalid pattern match"),
    }
}

fn type_check_literal_pattern(
    ctx: &mut TypeCheckerContext,
    lit: &mut Literal,
    e: &mut Expression,
    target_type: &Type,
    return_type: &Type,
    target: &Target,
) -> CompileResult<Type> {
    let m_type = if let Literal::Array(al) = lit {
        type_check_array_literal(ctx, al, target)?.unwrap()
    } else {
        lit.get_type()
    };

    if !target_type.is_matchable(&m_type) {
        if let Some(nlit) = lit.try_convert(target_type) {
            *lit = nlit;
        } else {
            return type_error_result(
                &lit.span(),
                format!(
                    "Pattern match of type {}, cannot match with an expression of type {}",
                    m_type, target_type
                ),
            );
        }
    }

    infer_matched_type(ctx, e, return_type, target)
}

fn type_check_struct_pattern_match(
    ctx: &mut TypeCheckerContext,
    p: &mut StructPattern,
    e: &mut Expression,
    target_type: &Type,
    return_type: &Type,
    target_is_mutable: bool,
    target: &Target,
) -> CompileResult<Type> {
    ctx.enter_scope(None);
    type_check_struct_pattern(ctx, p, target_is_mutable)?;
    if &p.typ != target_type && !target_type.is_pointer_to(&p.typ) {
        return type_error_result(
            &p.span,
            format!(
                "Cannot pattern match an expression of type:\n{}\nwith an expression of type:\n{}\n",
                target_type, p.typ
            ),
        );
    }

    let ct = infer_matched_type(ctx, e, return_type, target)?;
    ctx.exit_scope();
    Ok(ct)
}

fn type_check_nil_pattern(
    ctx: &mut TypeCheckerContext,
    span: &Span,
    e: &mut Expression,
    target_type: &Type,
    return_type: &Type,
    target: &Target,
) -> CompileResult<Type> {
    if !target_type.is_optional() {
        return type_error_result(
            span,
            format!(
                "Cannot match type {} to nil, only optionals can be matched to nil",
                target_type
            ),
        );
    }

    infer_matched_type(ctx, e, return_type, target)
}

fn type_check_optional_pattern(
    ctx: &mut TypeCheckerContext,
    o: &mut OptionalPattern,
    e: &mut Expression,
    target_type: &Type,
    return_type: &Type,
    target_is_mutable: bool,
    target: &Target,
) -> CompileResult<Type> {
    // Allow matching through pointer to optional
    let inner_type = match target_type {
        Type::Pointer(pt) => {
            if let Type::Optional(t) = pt.deref() {
                Some(t.deref().clone())
            } else {
                None
            }
        }
        Type::Optional(t) => Some(t.deref().clone()),
        _ => None,
    };

    let Some(inner_type) = inner_type else {
        return type_error_result(
            &o.span,
            format!("Cannot match type {} to optional pattern", target_type),
        );
    };

    o.inner_type = inner_type;
    ctx.enter_scope(None);
    ctx.add(Symbol::new(
        &o.binding,
        &o.inner_type,
        target_is_mutable,
        &o.span,
        SymbolType::Normal,
    ))?;
    let ct = infer_matched_type(ctx, e, return_type, target)?;
    ctx.exit_scope();
    Ok(ct)
}

fn get_result_type(typ: &Type) -> Option<Rc<ResultType>> {
    match typ {
        Type::Pointer(pt) => {
            if let Type::Result(rt) = pt.deref() {
                Some(rt.clone())
            } else {
                None
            }
        }

        Type::Result(rt) => Some(rt.clone()),

        _ => None,
    }
}

fn type_check_ok_pattern_match(
    ctx: &mut TypeCheckerContext,
    ok: &mut OkPattern,
    e: &mut Expression,
    target_type: &Type,
    return_type: &Type,
    target_is_mutable: bool,
    target: &Target,
) -> CompileResult<Type> {
    let Some(rt) = get_result_type(target_type) else {
        return type_error_result(&ok.span, format!("Cannot match type {} to result type", target_type));
    };

    let target_inner_type = rt.ok_typ.clone();
    let e_type = type_check_pattern_match(
        ctx,
        &mut ok.inner,
        e,
        &target_inner_type,
        return_type,
        target_is_mutable,
        target,
    )?;
    ok.inner_type = rt.ok_typ.clone();
    Ok(e_type)
}

fn type_check_error_pattern_match(
    ctx: &mut TypeCheckerContext,
    err: &mut ErrorPattern,
    e: &mut Expression,
    target_type: &Type,
    return_type: &Type,
    target_is_mutable: bool,
    target: &Target,
) -> CompileResult<Type> {
    let Some(rt) = get_result_type(target_type) else {
        return type_error_result(&err.span, format!("Cannot match type {} to result type", target_type));
    };

    let inner_target_type = rt.err_typ.clone();
    let e_type = type_check_pattern_match(
        ctx,
        &mut err.inner,
        e,
        &inner_target_type,
        return_type,
        target_is_mutable,
        target,
    )?;
    err.inner_type = rt.err_typ.clone();
    Ok(e_type)
}

fn type_check_binding_pattern(
    ctx: &mut TypeCheckerContext,
    nr: &mut NameRef,
    e: &mut Expression,
    target_type: &Type,
    return_type: &Type,
    target_is_mutable: bool,
    target: &Target,
) -> CompileResult<Type> {
    nr.typ = target_type.clone();
    ctx.enter_scope(None);
    ctx.add(Symbol::new(
        &nr.name,
        target_type,
        target_is_mutable,
        &nr.span,
        SymbolType::Normal,
    ))?;
    let ct = infer_matched_type(ctx, e, return_type, target)?;
    ctx.exit_scope();
    Ok(ct)
}

pub fn type_check_pattern_match(
    ctx: &mut TypeCheckerContext,
    p: &mut Pattern,
    e: &mut Expression,
    target_type: &Type,
    return_type: &Type,
    target_is_mutable: bool,
    target: &Target,
) -> CompileResult<Type> {
    match p {
        Pattern::EmptyArray(ap) => type_check_empty_array_pattern(ctx, ap, e, return_type, target_type, target),
        Pattern::Array(ap) => type_check_array_pattern(ctx, ap, e, return_type, target_type, target),
        Pattern::Name(nr) => {
            // If name does not exist it is a binding pattern
            if ctx.resolve(&nr.name).is_none() && !nr.name.contains("::") {
                let mut nr = nr.clone();
                let ct =
                    type_check_binding_pattern(ctx, &mut nr, e, target_type, return_type, target_is_mutable, target)?;
                *p = Pattern::Binding(nr);
                Ok(ct)
            } else {
                type_check_name_pattern(ctx, nr, e, return_type, target_type, target)
            }
        }
        Pattern::Literal(lit) => type_check_literal_pattern(ctx, lit, e, target_type, return_type, target),
        Pattern::Struct(p) => {
            type_check_struct_pattern_match(ctx, p, e, target_type, return_type, target_is_mutable, target)
        }
        Pattern::Any(_) => infer_matched_type(ctx, e, return_type, target),
        Pattern::Nil(span) => type_check_nil_pattern(ctx, span, e, target_type, return_type, target),
        Pattern::Optional(o) => {
            type_check_optional_pattern(ctx, o, e, target_type, return_type, target_is_mutable, target)
        }
        Pattern::Ok(ok) => type_check_ok_pattern_match(ctx, ok, e, target_type, return_type, target_is_mutable, target),
        Pattern::Error(err) => {
            type_check_error_pattern_match(ctx, err, e, target_type, return_type, target_is_mutable, target)
        }
        Pattern::Binding(nr) => {
            type_check_binding_pattern(ctx, nr, e, target_type, return_type, target_is_mutable, target)
        }
    }
}
