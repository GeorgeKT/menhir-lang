use super::typecheckercontext::TypeCheckerContext;
use crate::ast::*;
use crate::compileerror::{type_error, CompileResult};
use crate::span::Span;
use std::ops::Deref;

fn check_interface_constraints(generic: &Type, concrete: &Type) -> Result<Type, String> {
    match generic {
        Type::Generic(gt) => match gt.deref() {
            GenericType::Any(_) => Ok(concrete.clone()),

            GenericType::Restricted(interfaces) => {
                for interface in interfaces {
                    if !concrete.implements_interface(interface) {
                        return Err(format!(
                            "Type {} does not implement the interface {}",
                            concrete.name(),
                            interface.name(),
                        ));
                    }
                }

                Ok(concrete.clone())
            }
        },

        _ => Ok(concrete.clone()),
    }
}

fn make_concrete_type(mapping: &GenericMapping, generic: &Type) -> Result<Type, String> {
    if !generic.is_generic() {
        return Ok(generic.clone());
    }

    if let Some(concrete) = mapping.get(generic) {
        return check_interface_constraints(generic, concrete);
    }

    let typ = match generic {
        Type::Array(at) => array_type(make_concrete_type(mapping, &at.element_type)?, at.len),

        Type::Slice(st) => slice_type(make_concrete_type(mapping, &st.element_type)?),

        Type::Func(ft) => {
            let mut args = Vec::new();
            for t in &ft.args {
                args.push(FuncArg {
                    typ: make_concrete_type(mapping, &t.typ)?,
                    mutable: t.mutable,
                });
            }

            func_type(args, make_concrete_type(mapping, &ft.return_type)?)
        }

        Type::Struct(st) => {
            let mut members = Vec::new();
            for m in &st.members {
                members.push(struct_member(m.name.clone(), make_concrete_type(mapping, &m.typ)?));
            }

            let mut implements = Vec::new();
            for m in &st.implements {
                implements.push(make_concrete_type(mapping, m)?);
            }
            struct_type(st.name.clone(), members, implements)
        }

        Type::Sum(st) => {
            let mut cases = Vec::new();
            for c in &st.cases {
                let case_typ = if let Some(st) = &c.typ {
                    Some(make_concrete_type(mapping, st)?)
                } else {
                    None
                };
                cases.push(sum_type_case(&c.name, case_typ));
            }

            let mut implements = Vec::new();
            for m in &st.implements {
                implements.push(make_concrete_type(mapping, m)?);
            }
            sum_type(&st.name, cases, implements)
        }

        Type::Pointer(inner) => ptr_type(make_concrete_type(mapping, inner)?),

        Type::Optional(inner) => optional_type(make_concrete_type(mapping, inner)?),

        Type::Result(rt) => result_type(
            make_concrete_type(mapping, &rt.ok_typ)?,
            make_concrete_type(mapping, &rt.err_typ)?,
        ),

        _ => generic.clone(),
    };

    Ok(typ)
}

pub fn make_concrete(mapping: &GenericMapping, generic: &Type, span: &Span) -> CompileResult<Type> {
    make_concrete_type(mapping, generic).map_err(|msg| type_error(span, msg))
}

fn substitute_bindings(
    ctx: &TypeCheckerContext,
    generic_args: &GenericMapping,
    lb: &[Binding],
) -> CompileResult<Vec<Binding>> {
    let mut bindings = Vec::with_capacity(lb.len());
    for b in lb {
        let binding_expr = substitute_expr(ctx, generic_args, &b.init)?;
        let typ = make_concrete(generic_args, &b.typ, &b.span)?;
        let new_binding = match &b.binding_type {
            BindingType::Name(name) => name_binding(name.clone(), binding_expr, b.mutable, typ, b.span.clone()),

            BindingType::Struct(s) => binding(
                BindingType::Struct(substitute_struct_pattern(generic_args, s)?),
                binding_expr,
                b.mutable,
                typ,
                b.span.clone(),
            ),
        };
        bindings.push(new_binding);
    }
    Ok(bindings)
}

fn substitute_struct_pattern(generic_args: &GenericMapping, p: &StructPattern) -> CompileResult<StructPattern> {
    let mut bindings = Vec::with_capacity(p.bindings.len());
    for b in &p.bindings {
        bindings.push(StructPatternBinding {
            name: b.name.clone(),
            typ: make_concrete(generic_args, &b.typ, &p.span)?,
            mode: b.mode,
        });
    }

    let sp_typ = make_concrete(generic_args, &p.typ, &p.span)?;
    Ok(struct_pattern(&p.name, bindings, sp_typ, p.span.clone()))
}

fn substitute_pattern(ctx: &TypeCheckerContext, generic_args: &GenericMapping, p: &Pattern) -> CompileResult<Pattern> {
    match p {
        Pattern::Struct(sp) => Ok(Pattern::Struct(substitute_struct_pattern(generic_args, sp)?)),

        Pattern::Name(nr) => {
            let new_nr = NameRef {
                name: nr.name.clone(),
                span: nr.span.clone(),
                typ: make_concrete(generic_args, &nr.typ, &nr.span)?,
            };
            Ok(Pattern::Name(new_nr))
        }

        Pattern::Literal(Literal::Array(al)) => substitute_array_literal(ctx, generic_args, al).map(Pattern::Literal),

        Pattern::Binding(nr) => {
            let new_nr = NameRef {
                name: nr.name.clone(),
                span: nr.span.clone(),
                typ: make_concrete(generic_args, &nr.typ, &nr.span)?,
            };
            Ok(Pattern::Binding(new_nr))
        }

        Pattern::Optional(opt) => {
            let new_opt = OptionalPattern {
                binding: opt.binding.clone(),
                span: opt.span.clone(),
                inner_type: make_concrete(generic_args, &opt.inner_type, &opt.span)?,
            };
            Ok(Pattern::Optional(new_opt))
        }

        Pattern::Ok(ok) => {
            let new_ok = OkPattern {
                inner: Box::new(substitute_pattern(ctx, generic_args, &ok.inner)?),
                span: ok.span.clone(),
                inner_type: make_concrete(generic_args, &ok.inner_type, &ok.span)?,
            };
            Ok(Pattern::Ok(new_ok))
        }

        Pattern::Error(err) => {
            let new_ok = ErrorPattern {
                inner: Box::new(substitute_pattern(ctx, generic_args, &err.inner)?),
                span: err.span.clone(),
                inner_type: make_concrete(generic_args, &err.inner_type, &err.span)?,
            };
            Ok(Pattern::Error(new_ok))
        }

        _ => Ok(p.clone()),
    }
}

fn substitute_array_literal(
    ctx: &TypeCheckerContext,
    generic_args: &GenericMapping,
    al: &ArrayLiteral,
) -> CompileResult<Literal> {
    let mut new_elements = Vec::with_capacity(al.elements.len());
    for el in &al.elements {
        new_elements.push(substitute_expr(ctx, generic_args, el)?);
    }
    Ok(array_lit(new_elements, al.span.clone()))
}

fn substitute_call(ctx: &TypeCheckerContext, generic_args: &GenericMapping, c: &Call) -> CompileResult<Call> {
    let mut new_args = Vec::with_capacity(c.args.len());
    for a in &c.args {
        new_args.push(substitute_expr(ctx, generic_args, a)?);
    }

    Ok(Call::new(c.callee.clone(), new_args, c.span.clone()))
}

fn substitute_name_ref(generic_args: &GenericMapping, nr: &NameRef) -> CompileResult<NameRef> {
    let new_nr = NameRef {
        name: nr.name.clone(),
        span: nr.span.clone(),
        typ: make_concrete(generic_args, &nr.typ, &nr.span)?,
    };
    Ok(new_nr)
}

fn substitute_member_access(
    ctx: &TypeCheckerContext,
    generic_args: &GenericMapping,
    sma: &MemberAccess,
) -> CompileResult<MemberAccess> {
    let left = substitute_expr(ctx, generic_args, &sma.left)?;
    let right = match &sma.right {
        MemberAccessType::Call(c) => {
            let new_c = substitute_call(ctx, generic_args, c)?;
            MemberAccessType::Call(Box::new(new_c))
        }
        _ => sma.right.clone(),
    };

    Ok(MemberAccess {
        left,
        right,
        span: sma.span.clone(),
        typ: sma.typ.clone(),
    })
}

fn substitute_index_op(
    ctx: &TypeCheckerContext,
    generic_args: &GenericMapping,
    iop: &IndexOperation,
) -> CompileResult<IndexOperation> {
    let target = substitute_expr(ctx, generic_args, &iop.target)?;
    let index_expr = substitute_expr(ctx, generic_args, &iop.index_expr)?;
    Ok(IndexOperation {
        target,
        index_expr,
        span: iop.span.clone(),
        typ: iop.typ.clone(),
    })
}

fn substitute_compiler_call(
    ctx: &TypeCheckerContext,
    generic_args: &GenericMapping,
    cc: &CompilerCall,
) -> CompileResult<Expression> {
    match cc {
        CompilerCall::SizeOf { typ, int_size, span } => {
            let new_t = make_concrete(generic_args, typ, span)?;
            Ok(cc_size_of(new_t, *int_size, span.clone()))
        }
        CompilerCall::Drop {
            obj,
            span,
            destructor_call,
            drop_flag,
        } => {
            let ds = if let Some(ds) = destructor_call {
                Some(substitute_call(ctx, generic_args, ds)?)
            } else {
                None
            };

            let nobj = substitute_expr(ctx, generic_args, obj)?;
            Ok(cc_drop(nobj, span.clone(), ds, drop_flag.clone()))
        }
    }
}

fn substitute_block(ctx: &TypeCheckerContext, generic_args: &GenericMapping, b: &Block) -> CompileResult<Block> {
    let mut new_expressions = Vec::with_capacity(b.expressions.len());
    for e in &b.expressions {
        new_expressions.push(substitute_expr(ctx, generic_args, e)?);
    }
    let mut new_block = Block {
        expressions: Vec::new(),
        drop_flags: b.drop_flags.clone(),
        deferred_expressions: Vec::new(),
        typ: make_concrete(generic_args, &b.typ, &b.span)?,
        span: b.span.clone(),
    };

    for e in &b.expressions {
        new_block
            .expressions
            .push(substitute_expr(ctx, generic_args, e)?);
    }

    for e in &b.deferred_expressions {
        new_block
            .deferred_expressions
            .push(substitute_expr(ctx, generic_args, e)?);
    }

    Ok(new_block)
}

fn substitute_expr(
    ctx: &TypeCheckerContext,
    generic_args: &GenericMapping,
    e: &Expression,
) -> CompileResult<Expression> {
    match e {
        Expression::UnaryOp(op) => {
            let e = substitute_expr(ctx, generic_args, &op.expression)?;
            Ok(unary_op(op.operator, e, op.span.clone()))
        }

        Expression::BinaryOp(op) => {
            let l = substitute_expr(ctx, generic_args, &op.left)?;
            let r = substitute_expr(ctx, generic_args, &op.right)?;
            Ok(bin_op(op.operator, l, r, op.span.clone()))
        }

        Expression::Literal(Literal::Array(a)) => {
            substitute_array_literal(ctx, generic_args, a).map(Expression::Literal)
        }

        Expression::Call(c) => {
            let new_c = substitute_call(ctx, generic_args, c)?;
            Ok(Expression::Call(Box::new(new_c)))
        }

        Expression::Lambda(l) => {
            let mut args = Vec::with_capacity(l.sig.args.len());
            for a in &l.sig.args {
                args.push(Argument::new(
                    a.name.clone(),
                    make_concrete(generic_args, &a.typ, &a.span)?,
                    a.mutable,
                    a.span.clone(),
                ))
            }
            let expr = substitute_expr(ctx, generic_args, &l.expr)?;
            Ok(lambda(args, expr, l.span.clone()))
        }

        Expression::Match(m) => {
            let target = substitute_expr(ctx, generic_args, &m.target)?;
            let mut cases = Vec::with_capacity(m.cases.len());
            for c in &m.cases {
                let pattern = substitute_pattern(ctx, generic_args, &c.pattern)?;
                let to_execute = substitute_expr(ctx, generic_args, &c.to_execute)?;
                cases.push(match_case(pattern, to_execute, c.span.clone()));
            }
            Ok(match_expression(target, cases, m.span.clone()))
        }

        Expression::Bindings(l) => {
            let nb = substitute_bindings(ctx, generic_args, &l.bindings)?;
            Ok(bindings(nb, l.span.clone()))
        }

        Expression::If(i) => {
            let cond = substitute_expr(ctx, generic_args, &i.condition)?;
            let then = substitute_expr(ctx, generic_args, &i.on_true)?;
            if let Some(else_block) = &i.on_false {
                Ok(if_expression(
                    cond,
                    then,
                    substitute_expr(ctx, generic_args, else_block)?,
                    i.span.clone(),
                ))
            } else {
                Ok(single_if_expression(cond, then, i.span.clone()))
            }
        }

        Expression::Block(b) => {
            let b = substitute_block(ctx, generic_args, b)?;
            Ok(Expression::Block(Box::new(b)))
        }

        Expression::Literal(lit) => Ok(Expression::Literal(lit.clone())),

        Expression::NameRef(nr) => Ok(Expression::NameRef(substitute_name_ref(generic_args, nr)?)),

        Expression::StructInitializer(si) => {
            let mut nmi = Vec::with_capacity(si.member_initializers.len());
            for mi in &si.member_initializers {
                let new_e = substitute_expr(ctx, generic_args, &mi.initializer)?;
                nmi.push(StructMemberInitializer {
                    name: mi.name.clone(),
                    initializer: new_e,
                    member_idx: mi.member_idx,
                });
            }

            Ok(Expression::StructInitializer(struct_initializer(
                si.struct_name.clone(),
                nmi,
                si.span.clone(),
            )))
        }

        Expression::MemberAccess(sma) => {
            let ma = substitute_member_access(ctx, generic_args, sma)?;
            Ok(Expression::MemberAccess(Box::new(ma)))
        }

        Expression::New(n) => {
            let inner = substitute_expr(ctx, generic_args, &n.inner)?;
            Ok(new_with_type(
                inner,
                make_concrete(generic_args, &n.typ, &n.span)?,
                n.span.clone(),
            ))
        }

        Expression::Delete(d) => match d.deref() {
            DeleteExpression::Delete { inner, span } => {
                let inner = substitute_expr(ctx, generic_args, inner)?;
                Ok(delete(inner, span.clone()))
            }
            DeleteExpression::BlockWithDestructor { block, span } => {
                let b = substitute_block(ctx, generic_args, block)?;
                Ok(Expression::Delete(Box::new(DeleteExpression::BlockWithDestructor {
                    block: b,
                    span: span.clone(),
                })))
            }
        },

        Expression::ArrayToSlice(ats) => {
            let inner = substitute_expr(ctx, generic_args, &ats.inner)?;
            Ok(array_to_slice(inner, ats.span.clone()))
        }

        Expression::AddressOf(a) => {
            let inner = substitute_expr(ctx, generic_args, &a.inner)?;
            Ok(address_of(inner, a.span.clone()))
        }

        Expression::Dereference(d) => {
            let inner = substitute_expr(ctx, generic_args, &d.inner)?;
            Ok(dereference(inner, d.span.clone()))
        }

        Expression::Assign(a) => {
            let l = match &a.left {
                AssignTarget::Var(nr) => AssignTarget::Var(substitute_name_ref(generic_args, nr)?),

                AssignTarget::MemberAccess(ma) => {
                    AssignTarget::MemberAccess(substitute_member_access(ctx, generic_args, ma)?)
                }

                AssignTarget::Dereference(d) => {
                    let inner = substitute_expr(ctx, generic_args, &d.inner)?;
                    AssignTarget::Dereference(DereferenceExpression {
                        inner,
                        typ: d.typ.clone(),
                        span: d.span.clone(),
                    })
                }

                AssignTarget::IndexOperation(iop) => {
                    let iop = substitute_index_op(ctx, generic_args, iop)?;
                    AssignTarget::IndexOperation(iop)
                }
            };
            let r = substitute_expr(ctx, generic_args, &a.right)?;
            Ok(assign(a.operator, l, r, a.span.clone()))
        }

        Expression::While(w) => {
            let c = substitute_expr(ctx, generic_args, &w.cond)?;
            let b = substitute_expr(ctx, generic_args, &w.body)?;
            Ok(while_loop(c, b, w.span.clone()))
        }

        Expression::For(f) => {
            let i = substitute_expr(ctx, generic_args, &f.iterable)?;
            let b = substitute_expr(ctx, generic_args, &f.body)?;
            Ok(for_loop(&f.loop_variable, i, b, f.span.clone()))
        }

        Expression::Nil(span) => Ok(Expression::Nil(span.clone())),

        Expression::OptionalToBool(inner) => Ok(Expression::OptionalToBool(inner.clone())),

        Expression::ToOptional(t) => {
            let inner = substitute_expr(ctx, generic_args, &t.inner)?;
            Ok(to_optional(inner, t.optional_type.clone(), t.span.clone()))
        }

        Expression::Cast(t) => {
            let inner = substitute_expr(ctx, generic_args, &t.inner)?;
            Ok(type_cast(
                inner,
                make_concrete(generic_args, &t.destination_type, &t.span)?,
                t.span.clone(),
            ))
        }

        Expression::Void => Ok(Expression::Void),

        Expression::CompilerCall(c) => substitute_compiler_call(ctx, generic_args, c),

        Expression::IndexOperation(iop) => Ok(Expression::IndexOperation(Box::new(substitute_index_op(
            ctx,
            generic_args,
            iop,
        )?))),

        Expression::Return(r) => {
            let e = substitute_expr(ctx, generic_args, &r.expression)?;
            Ok(return_expr(e, r.span.clone()))
        }

        Expression::ResultToBool(r) => {
            let e = substitute_expr(ctx, generic_args, r)?;
            Ok(Expression::ResultToBool(Box::new(e)))
        }

        Expression::ToOkResult(r) => {
            let typ = make_concrete(generic_args, &r.result_type, &r.inner.span())?;
            let e = substitute_expr(ctx, generic_args, &r.inner)?;
            Ok(to_ok_result(e, typ, r.span.clone()))
        }

        Expression::ToErrResult(r) => {
            let typ = make_concrete(generic_args, &r.result_type, &r.inner.span())?;
            let e = substitute_expr(ctx, generic_args, &r.inner)?;
            Ok(to_err_result(e, typ, r.span.clone()))
        }
        Expression::Range(r) => {
            let typ = make_concrete(generic_args, &r.typ, &r.span)?;
            let start = if let Some(start) = &r.start {
                Some(substitute_expr(ctx, generic_args, start)?)
            } else {
                None
            };

            let end = if let Some(end) = &r.end {
                Some(substitute_expr(ctx, generic_args, end)?)
            } else {
                None
            };
            Ok(range(start, end, typ, r.span.clone()))
        }
        Expression::Enclosed(e) => substitute_expr(ctx, generic_args, e),
    }
}

pub fn instantiate(
    ctx: &TypeCheckerContext,
    func: &Function,
    generic_args: &GenericMapping,
) -> CompileResult<Function> {
    let mut arg_types = Vec::with_capacity(func.sig.args.len());
    let mut args = Vec::with_capacity(func.sig.args.len());
    for arg in &func.sig.args {
        let arg_typ = make_concrete(generic_args, &arg.typ, &arg.span)?;
        args.push(Argument::new(
            arg.name.clone(),
            arg_typ.clone(),
            arg.mutable,
            arg.span.clone(),
        ));
        arg_types.push(FuncArg {
            typ: arg_typ,
            mutable: arg.mutable,
        });
    }

    let return_type = make_concrete(generic_args, &func.sig.return_type, &func.sig.span)?;
    let sig = FunctionSignature {
        name: new_func_name(&func.sig.name, generic_args),
        return_type: return_type.clone(),
        args,
        span: func.sig.span.clone(),
        typ: func_type(arg_types, return_type),
        rvo: false,
    };

    let body = substitute_expr(ctx, generic_args, &func.expression)?;
    Ok(Function::new(sig, func.public, body, func.span.clone()))
}
