use std::mem;

use crate::ast::*;
use crate::compileerror::{type_error, type_error_result, CompileResult};
use crate::span::Span;

use super::typecheckercontext::TypeCheckerContext;

pub fn destructor_name(type_name: &str) -> String {
    let mut name = type_name.to_string();
    if let Some(ns) = name.rfind("::") {
        name.insert(ns + 2, '~');
        name
    } else {
        format!("~{type_name}")
    }
}

fn self_member_access(name: &str, member_idx: usize, span: &Span) -> Expression {
    Expression::MemberAccess(Box::new(MemberAccess {
        left: name_expr("self", span.clone()),
        right: MemberAccessType::Name(field(name, member_idx)),
        span: span.clone(),
        typ: Type::Unknown,
    }))
}

fn block(expressions: Vec<Expression>, deferred: Vec<Expression>, span: Span) -> Expression {
    let b = Box::new(Block {
        expressions,
        deferred_expressions: deferred,
        drop_flags: Vec::new(),
        typ: Type::Unknown,
        span,
    });
    Expression::Block(b)
}

fn get_member_destructor_calls(
    sd: &StructDeclaration,
    destructor_calls: &mut Vec<Expression>,
    ctx: &TypeCheckerContext,
) -> CompileResult<()> {
    for (idx, member) in sd.members.iter().enumerate() {
        let sfm = address_of(self_member_access(&member.name, idx, &member.span), member.span.clone());

        if let Some(call) = ctx.get_destructor_call(sfm, None, &member.typ)? {
            destructor_calls.push(call);
            break;
        }
    }
    Ok(())
}

fn create_destructor(name: &str, body: Expression, span: &Span, module: &mut Module) {
    let ds_name = destructor_name(name);
    let sig = FunctionSignature {
        name: ds_name.clone(),
        return_type: Type::Void,
        args: vec![Argument::new(
            "self",
            ptr_type(unresolved_type(name, Vec::new())),
            true,
            span.clone(),
        )],
        span: span.clone(),
        typ: Type::Unknown,
        rvo: false,
    };
    let func = Function::new(sig, true, body, span.clone());
    module.functions.insert(ds_name, func);
}

fn destructor_for_struct(sd: &StructDeclaration, ctx: &TypeCheckerContext, module: &mut Module) -> CompileResult<()> {
    let mut destructor_calls = Vec::new();
    get_member_destructor_calls(sd, &mut destructor_calls, ctx)?;

    if destructor_calls.is_empty() {
        return Ok(());
    }

    if let Some(sym) = ctx.get_destructor(&sd.typ)? {
        let func = module
            .functions
            .get_mut(&sym.name)
            .ok_or_else(|| type_error(&sym.span, format!("Cannot find destructor {}", sym.name)))?;
        if let Expression::Block(b) = &mut func.expression {
            for call in destructor_calls.into_iter().rev() {
                b.deferred_expressions.push(call);
            }
        } else {
            let e = mem::replace(&mut func.expression, Expression::Void);
            func.expression = block(vec![e], destructor_calls.into_iter().rev().collect(), sd.span.clone());
        }
        func.type_checked = false; // Force a new type check
    } else {
        let b = block(destructor_calls.into_iter().rev().collect(), vec![], sd.span.clone());
        create_destructor(&sd.name, b, &sd.span, module);
    }

    Ok(())
}

fn generate_sum_case_destructor(
    c: &SumTypeCaseDeclaration,
    ctx: &TypeCheckerContext,
) -> CompileResult<Option<MatchCase>> {
    let Some(sd) = &c.data else {
        return Ok(None);
    };

    let mut destructors = Vec::new();
    let mut bindings = Vec::new();
    for sm in &sd.members {
        let param = name_expr(&sm.name, c.span.clone());
        if let Some(ds) = ctx.get_destructor_call(param, None, &sm.typ)? {
            destructors.push(ds);
            bindings.push(StructPatternBinding {
                name: sm.name.clone(),
                typ: Type::Unknown,
                mode: if sm.typ.pass_by_value() {
                    StructPatternBindingMode::Value
                } else {
                    StructPatternBindingMode::Pointer
                },
            })
        }
    }

    if destructors.is_empty() {
        return Ok(None);
    }

    let pattern = Pattern::Struct(StructPattern {
        name: c.name.clone(),
        bindings,
        typ: Type::Unknown,
        span: c.span.clone(),
    });
    let to_execute = block(destructors, vec![], c.span.clone());
    Ok(Some(match_case(pattern, to_execute, c.span.clone())))
}

fn generate_sum_destructor_match(
    st: &SumTypeDeclaration,
    target: Expression,
    ctx: &TypeCheckerContext,
) -> CompileResult<Option<Expression>> {
    let mut cases = Vec::new();
    for c in &st.cases {
        if let Some(ca) = generate_sum_case_destructor(c, ctx)? {
            cases.push(ca);
        }
    }

    if cases.is_empty() {
        return Ok(None);
    }

    if cases.len() != st.cases.len() {
        cases.push(match_case(
            Pattern::Any(st.span.clone()),
            Expression::Void,
            st.span.clone(),
        ));
    }

    Ok(Some(Expression::Match(Box::new(MatchExpression {
        target,
        cases,
        typ: Type::Unknown,
        span: st.span.clone(),
    }))))
}

fn destructor_for_sum(st: &SumTypeDeclaration, ctx: &TypeCheckerContext, module: &mut Module) -> CompileResult<()> {
    let target = name_expr("self", st.span.clone());
    let Some(match_expr) = generate_sum_destructor_match(st, target, ctx)? else {
        return Ok(());
    };

    if let Some(sym) = ctx.get_destructor(&st.typ)? {
        let func = module
            .functions
            .get_mut(&sym.name)
            .ok_or_else(|| type_error(&sym.span, format!("Cannot find destructor {}", sym.name)))?;
        if let Expression::Block(b) = &mut func.expression {
            b.deferred_expressions.push(match_expr);
        } else {
            let e = mem::replace(&mut func.expression, Expression::Void);
            func.expression = block(vec![e], vec![match_expr], st.span.clone());
        }
        func.type_checked = false; // Force a new type check
    } else {
        let b = block(vec![match_expr], vec![], st.span.clone());
        create_destructor(&st.name, b, &st.span, module);
    }

    Ok(())
}

fn let_expr(name: &str, value: Expression, span: Span) -> Expression {
    Expression::Bindings(Box::new(BindingList {
        bindings: vec![Binding {
            mutable: false,
            binding_type: BindingType::Name(name.into()),
            init: value,
            typ: Type::Unknown,
            span: span.clone(),
        }],
        span,
    }))
}

pub fn create_destructors(ctx: &TypeCheckerContext, module: &mut Module) -> CompileResult<()> {
    let types = module.types.clone(); // Clone to avoid having to borrow module immutable and
                                      // mutable at the same time
    for td in types.values() {
        match td {
            TypeDeclaration::Interface(_) => (),
            TypeDeclaration::Struct(sd) => destructor_for_struct(sd, ctx, module)?,
            TypeDeclaration::Sum(st) => destructor_for_sum(st, ctx, module)?,
        }
    }

    for func in module.functions.values_mut() {
        if func.sig.typ.is_generic() {
            continue;
        }

        let mut recheck_types = false;
        func.expression.visit_mut(&mut |e| {
            let Expression::Delete(d) = e else {
                return Ok(());
            };

            let Some(et) = d.inner.get_type().get_pointer_element_type().cloned() else {
                return type_error_result(&d.inner.span(), "Expecting a pointer type in a delete expression");
            };

            let Some(dc) = ctx.get_destructor_call(name_expr("$del", d.span.clone()), None, &et)? else {
                return Ok(());
            };

            let binding = let_expr("$del", d.inner.clone(), d.inner.span());
            let new_del = Expression::Delete(Box::new(DeleteExpression {
                inner: name_expr("$del", d.span.clone()),
                span: d.span.clone(),
            }));

            let b = block(vec![binding, dc, new_del], vec![], d.span.clone());
            *e = b;
            recheck_types = true;
            Ok(())
        })?;
        func.type_checked = recheck_types;
    }

    //module.print(0);
    Ok(())
}
