use crate::ast::*;
use crate::compileerror::{type_error_result, CompileResult};
use crate::span::Span;
use crate::target::Target;

use super::typecheckercontext::{DestructorToCreate, TypeCheckerContext};

pub fn destructor_name(type_name: &str) -> String {
    let mut name = type_name.to_string();
    if let Some(ns) = name.rfind("::") {
        name.insert(ns + 2, '~');
        name
    } else {
        format!("~{type_name}")
    }
}

fn self_member_access(name: &str, member_idx: usize) -> Expression {
    address_of(
        Expression::MemberAccess(Box::new(MemberAccess {
            left: name_expr("self", Span::default()),
            right: MemberAccessType::Name(field(name, member_idx)),
            span: Span::default(),
            typ: Type::Unknown,
        })),
        Span::Internal,
    )
}

fn block(expressions: Vec<Expression>, deferred: Vec<Expression>) -> Expression {
    let b = Box::new(Block {
        expressions,
        deferred_expressions: deferred,
        drop_flags: Vec::new(),
        typ: Type::Unknown,
        span: Span::default(),
    });
    Expression::Block(b)
}

fn get_member_destructor_calls(
    sd: &StructType,
    destructor_calls: &mut Vec<Expression>,
    ctx: &mut TypeCheckerContext,
) -> CompileResult<()> {
    for (idx, member) in sd.members.iter().enumerate() {
        let sfm = self_member_access(&member.name, idx);
        if let Some(call) = ctx.get_destructor_call(sfm, None, &member.typ)? {
            destructor_calls.push(call);
            break;
        }
    }
    Ok(())
}

fn create_destructor(name: &str, typ: &Type, body: Expression, module: &mut Module) {
    let span = Span::default();
    let self_type = ptr_type(typ.clone());

    let sig = FunctionSignature {
        name: name.into(),
        return_type: Type::Void,
        args: vec![Argument::new("self", self_type, true, span.clone())],
        span: span.clone(),
        typ: Type::Unknown,
        rvo: false,
    };
    let func = Function::new(sig, true, body, span.clone());
    //func.print(0);
    module.functions.insert(name.into(), func);
}

fn destructor_for_struct(
    dtc: &DestructorToCreate,
    sd: &StructType,
    ctx: &mut TypeCheckerContext,
    module: &mut Module,
) -> CompileResult<()> {
    let mut destructor_calls = Vec::new();
    get_member_destructor_calls(sd, &mut destructor_calls, ctx)?;

    if destructor_calls.is_empty() {
        return Ok(());
    }

    let b = block(destructor_calls.into_iter().rev().collect(), vec![]);
    create_destructor(&dtc.name, &dtc.typ, b, module);
    Ok(())
}

fn generate_sum_case_destructor(c: &SumTypeCase, ctx: &mut TypeCheckerContext) -> CompileResult<Option<MatchCase>> {
    let Some(Type::Struct(sd)) = &c.typ else {
        return Ok(None);
    };

    let mut destructors = Vec::new();
    let mut bindings = Vec::new();
    for sm in &sd.members {
        let param = name_expr(&sm.name, Span::default());
        if let Some(ds) = ctx.get_destructor_call(param, None, &sm.typ)? {
            destructors.push(ds);
            bindings.push(StructPatternBinding {
                name: sm.name.clone(),
                typ: Type::Unknown,
                mode: StructPatternBindingMode::Pointer,
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
        span: Span::default(),
    });
    let to_execute = block(destructors, vec![]);
    Ok(Some(match_case(pattern, to_execute, Span::default())))
}

fn generate_sum_destructor_match(
    st: &SumType,
    target: Expression,
    ctx: &mut TypeCheckerContext,
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
            Pattern::Any(Span::default()),
            Expression::Void,
            Span::default(),
        ));
    }

    Ok(Some(Expression::Match(Box::new(MatchExpression {
        target,
        cases,
        typ: Type::Unknown,
        span: Span::default(),
    }))))
}

fn destructor_for_sum(
    dtc: &DestructorToCreate,
    st: &SumType,
    ctx: &mut TypeCheckerContext,
    module: &mut Module,
) -> CompileResult<()> {
    let target = name_expr("self", Span::default());
    let Some(match_expr) = generate_sum_destructor_match(st, target, ctx)? else {
        return type_error_result(
            &Span::Internal,
            format!("Cannot generate destructor for sum type {}", st.name),
        );
    };

    let b = block(vec![match_expr], vec![]);
    create_destructor(&dtc.name, &dtc.typ, b, module);
    Ok(())
}

fn destructor_for_optional(
    dtc: &DestructorToCreate,
    inner_typ: &Type,
    ctx: &mut TypeCheckerContext,
    module: &mut Module,
) -> CompileResult<()> {
    let target = name_expr("self", Span::default());
    let mut cases = Vec::new();

    let pattern = Pattern::Optional(OptionalPattern {
        binding: "$inner".into(),
        span: Span::default(),
        inner_type: inner_typ.clone(),
    });

    let Some(ds) = ctx.get_destructor_call(
        address_of(name_expr("$inner", Span::Internal), Span::Internal),
        None,
        inner_typ,
    )?
    else {
        return type_error_result(&Span::default(), format!("Missing destructor for {inner_typ}"));
    };

    cases.push(match_case(pattern, ds, Span::Internal));
    cases.push(match_case(
        Pattern::Any(Span::Internal),
        Expression::Void,
        Span::default(),
    ));

    let m = Expression::Match(Box::new(MatchExpression {
        target,
        cases,
        typ: Type::Unknown,
        span: Span::default(),
    }));

    create_destructor(&dtc.name, &dtc.typ, block(vec![m], vec![]), module);
    Ok(())
}

fn destructor_for_result(
    dtc: &DestructorToCreate,
    rt: &ResultType,
    ctx: &mut TypeCheckerContext,
    module: &mut Module,
) -> CompileResult<()> {
    let target = name_expr("self", Span::default());
    let mut cases = Vec::new();

    if let Some(ds) = ctx.get_destructor_call(
        address_of(name_expr("$ok", Span::Internal), Span::Internal),
        None,
        &rt.ok_typ,
    )? {
        let ok_pattern = Pattern::Ok(OkPattern {
            inner: Box::new(name_pattern("$ok", Span::Internal)),
            span: Span::Internal,
            inner_type: ptr_type(rt.ok_typ.clone()),
        });
        cases.push(match_case(ok_pattern, ds, Span::Internal));
    }

    if let Some(ds) = ctx.get_destructor_call(
        address_of(name_expr("$error", Span::Internal), Span::Internal),
        None,
        &rt.err_typ,
    )? {
        let err_pattern = Pattern::Error(ErrorPattern {
            inner: Box::new(name_pattern("$error", Span::Internal)),
            span: Span::Internal,
            inner_type: ptr_type(rt.err_typ.clone()),
        });
        cases.push(match_case(err_pattern, ds, Span::Internal));
    }

    if cases.is_empty() {
        return type_error_result(
            &Span::Internal,
            format!("Cannot generate destructor for result type {}", dtc.typ),
        );
    }

    if cases.len() < 2 {
        cases.push(match_case(
            Pattern::Any(Span::Internal),
            Expression::Void,
            Span::Internal,
        ));
    }

    let m = Expression::Match(Box::new(MatchExpression {
        target,
        cases,
        typ: Type::Unknown,
        span: Span::default(),
    }));

    create_destructor(&dtc.name, &dtc.typ, block(vec![m], vec![]), module);
    Ok(())
}

fn destructor_for_array(
    dtc: &DestructorToCreate,
    at: &ArrayType,
    ctx: &mut TypeCheckerContext,
    module: &mut Module,
    target: &Target,
) -> CompileResult<()> {
    let mut ds_calls = Vec::new();
    for i in 0..at.len {
        let iop = address_of(
            index_op(
                name_expr("self", Span::Internal),
                uint_expr(i as u64, Span::Internal, target.int_size),
                Span::Internal,
            ),
            Span::Internal,
        );
        let Some(ds) = ctx.get_destructor_call(iop, None, &at.element_type)? else {
            return type_error_result(
                &Span::Internal,
                format!("Cannot generate destructor for array of type {}", at.element_type),
            );
        };
        ds_calls.push(ds);
    }

    create_destructor(&dtc.name, &dtc.typ, block(ds_calls, vec![]), module);
    Ok(())
}

pub fn create_destructors(ctx: &mut TypeCheckerContext, module: &mut Module, target: &Target) -> CompileResult<()> {
    for ds in ctx.get_destructors_to_create().iter() {
        match &ds.typ {
            Type::Struct(st) => destructor_for_struct(ds, st, ctx, module)?,
            Type::Sum(st) => destructor_for_sum(ds, st, ctx, module)?,
            Type::Optional(ot) => destructor_for_optional(ds, ot, ctx, module)?,
            Type::Result(rt) => destructor_for_result(ds, rt, ctx, module)?,
            Type::Array(et) => destructor_for_array(ds, et, ctx, module, target)?,
            _ => (),
        }
    }

    //module.print(0);
    Ok(())
}
