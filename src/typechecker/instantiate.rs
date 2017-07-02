use std::ops::Deref;
use ast::*;
use span::Span;
use compileerror::{CompileResult, type_error};
use super::typecheckercontext::TypeCheckerContext;

fn matches_function_signature(expected: &Type, actual: &Type, concrete_type: &Type, interface: &Type, method_name: &str) -> Result<(), String>
{
    fn type_matches(expected: &Type, actual: &Type, concrete_type: &Type, interface: &Type) -> bool {
        match (expected, actual)
        {
            (&Type::Pointer(ref e), &Type::Pointer(ref a)) |
            (&Type::Optional(ref e), &Type::Optional(ref a)) => type_matches(e, a, concrete_type, interface),
            (&Type::Array(ref e), &Type::Array(ref a)) => type_matches(&e.element_type, &a.element_type, concrete_type, interface),
            (&Type::Slice(ref e), &Type::Slice(ref a)) => type_matches(&e.element_type, &a.element_type, concrete_type, interface),
            _ => *expected == *actual || (*expected == Type::SelfType && *actual == *concrete_type),
        }
    }

    match (expected, actual)
    {
        (&Type::Func(ref e), &Type::Func(ref a)) => {
            if e.args.len() != a.args.len() {
                return Err(format!("Argument count mismatch for method {}", method_name));
            }

            if !type_matches(&e.return_type, &a.return_type, concrete_type, interface) {
                return Err(format!("Return types do not match on method {}", method_name));
            }

            for (idx, (e_arg, a_arg)) in e.args.iter().zip(a.args.iter()).enumerate() {
                if !type_matches(e_arg, a_arg, concrete_type, interface) {
                    return Err(format!("The type of argument {} does not match on method {}.", idx, method_name));
                }
            }

            Ok(())
        },

        _ => Err(format!("Cannot match function signatures of {} and {} types", expected, actual))
    }
}

fn satisfies_interface(ctx: &TypeCheckerContext, concrete_type: &Type, interface: &Type) -> Result<(), String>
{
    let it = if let Type::Interface(ref it) = *interface {
        it
    } else {
        return Err(format!("{} is not an interface type", interface.name()));
    };

    let concrete_type_name = concrete_type.name();
    for func in &it.functions {
        let r = ctx.resolve(&format!("{}.{}", concrete_type_name, func.name))
            .ok_or_else(|| format!("No method {} found on type {}", func.name, concrete_type_name))?;

        matches_function_signature(&func.typ, &r.typ, concrete_type, interface, &func.name)?;
    }

    Ok(())
}

fn check_interface_constraints(ctx: &TypeCheckerContext, generic: &Type, concrete: &Type) -> Result<Type, String>
{
    match *generic
    {
        Type::Generic(ref gt) => {
            match *gt.deref()
            {
                GenericType::Any(_) => Ok(concrete.clone()),

                GenericType::Restricted(ref interfaces) => {
                    for interface in interfaces {
                        satisfies_interface(ctx, concrete, interface)
                            .map_err(|msg|
                                format!("Type {} does not implement the interface {}: {}", concrete.name(), interface.name(), msg)
                            )?;
                    }

                    Ok(concrete.clone())
                }
            }
        }

        _ => Ok(concrete.clone())
    }
}


fn make_concrete_type(ctx: &TypeCheckerContext, mapping: &GenericMapping, generic: &Type) -> Result<Type, String>
{
    if !generic.is_generic() {
        return Ok(generic.clone());
    }

    if let Some(concrete) = mapping.get(generic) {
        return check_interface_constraints(ctx, generic, concrete);
    }

    let typ = match *generic
    {
        Type::Array(ref at) => {
            array_type(make_concrete_type(ctx, mapping, &at.element_type)?, at.len)
        },

        Type::Slice(ref st) => {
            slice_type(make_concrete_type(ctx, mapping, &st.element_type)?)
        },

        Type::Func(ref ft) => {
            let mut args = Vec::new();
            for t in &ft.args {
                args.push(make_concrete_type(ctx, mapping, t)?);
            }

            func_type(args, make_concrete_type(ctx, mapping, &ft.return_type)?)
        },

        Type::Struct(ref st) => {
            let mut members = Vec::new();
            for m in &st.members {
                members.push(struct_member(&m.name, make_concrete_type(ctx, mapping, &m.typ)?));
            }

            struct_type(&st.name, members)
        },

        Type::Sum(ref st) => {
            let mut cases = Vec::new();
            for c in &st.cases {
                cases.push(sum_type_case(&c.name, make_concrete_type(ctx, mapping, &c.typ)?));
            }

            sum_type(&st.name, cases)
        },

        Type::Pointer(ref inner) => {
            ptr_type(make_concrete_type(ctx, mapping, inner)?)
        },

        Type::Optional(ref inner) => {
            optional_type(make_concrete_type(ctx, mapping, inner)?)
        },

        _ => generic.clone(),
    };

    Ok(typ)
}

pub fn make_concrete(ctx: &TypeCheckerContext, mapping: &GenericMapping, generic: &Type, span: &Span) -> CompileResult<Type>
{
    make_concrete_type(ctx, mapping, generic).map_err(|msg| type_error(span, msg))
}


fn substitute_bindings(ctx: &TypeCheckerContext, generic_args: &GenericMapping, lb: &[Binding]) -> CompileResult<Vec<Binding>>
{
    let mut bindings = Vec::with_capacity(lb.len());
    for b in lb {
        let binding_expr = substitute_expr(ctx, generic_args, &b.init)?;
        let new_binding = match b.binding_type
        {
            BindingType::Name(ref name) => {
                name_binding(name.clone(), binding_expr, b.mutable, b.span.clone())
            },

            BindingType::Struct(ref s) => {
                binding(
                    BindingType::Struct(substitute_struct_pattern(ctx, generic_args, s)?),
                    binding_expr,
                    b.mutable,
                    b.span.clone()
                )
            },
        };
        bindings.push(new_binding);
    }
    Ok(bindings)
}



fn substitute_struct_pattern(ctx: &TypeCheckerContext, generic_args: &GenericMapping, p: &StructPattern) -> CompileResult<StructPattern>
{
    let mut bindings = Vec::with_capacity(p.bindings.len());
    for b in &p.bindings {
        bindings.push(
            StructPatternBinding{
                name: b.name.clone(),
                typ: make_concrete(ctx, generic_args, &b.typ, &p.span)?,
                mode: b.mode,
            }
        );
    }

    Ok(struct_pattern(
        &p.name,
        bindings,
        make_concrete(ctx, generic_args, &p.typ, &p.span)?,
        p.span.clone()
    ))
}

fn substitute_pattern(ctx: &TypeCheckerContext, generic_args: &GenericMapping, p: &Pattern) -> CompileResult<Pattern>
{
    match *p
    {
        Pattern::Struct(ref sp) => {
            Ok(Pattern::Struct(substitute_struct_pattern(ctx, generic_args, sp)?))
        },

        Pattern::Name(ref nr) => {
            let new_nr = NameRef{
                name: nr.name.clone(),
                span: nr.span.clone(),
                typ: make_concrete(ctx, generic_args, &nr.typ, &nr.span)?,
            };
            Ok(Pattern::Name(new_nr))
        },

        Pattern::Literal(Literal::Array(ref al)) => {
            substitute_array_literal(ctx, generic_args, al).map(Pattern::Literal)
        },

        _ => Ok(p.clone()),
    }
}

fn substitute_array_literal(ctx: &TypeCheckerContext, generic_args: &GenericMapping, al: &ArrayLiteral) -> CompileResult<Literal>
{
    let mut new_elements = Vec::with_capacity(al.elements.len());
    for el in &al.elements {
        new_elements.push(substitute_expr(ctx, generic_args, el)?);
    }
    Ok(array_lit(new_elements, al.span.clone()))
}

fn substitute_call(ctx: &TypeCheckerContext, generic_args: &GenericMapping, c: &Call) -> CompileResult<Call>
{
    let mut new_args = Vec::with_capacity(c.args.len());
    for a in &c.args {
        new_args.push(substitute_expr(ctx, generic_args, a)?);
    }

    Ok(Call::new(c.callee.clone(), new_args, c.span.clone()))
}

fn substitute_name_ref(ctx: &TypeCheckerContext, generic_args: &GenericMapping, nr: &NameRef) -> CompileResult<NameRef>
{
    let new_nr = NameRef{
        name: nr.name.clone(),
        span: nr.span.clone(),
        typ: make_concrete(ctx, generic_args, &nr.typ, &nr.span)?,
    };
    Ok(new_nr)
}

fn substitute_member_access(ctx: &TypeCheckerContext, generic_args: &GenericMapping, sma: &MemberAccess) -> CompileResult<MemberAccess>
{
    let left = substitute_expr(ctx, generic_args, &sma.left)?;
    let right = match sma.right
    {
        MemberAccessType::Call(ref c) => {
            let new_c = substitute_call(ctx, generic_args, c)?;
            MemberAccessType::Call(Box::new(new_c))
        },
        _ => sma.right.clone(),
    };

    Ok(MemberAccess{
        left,
        right,
        span: sma.span.clone(),
        typ: sma.typ.clone(),
    })
}

fn substitute_expr(ctx: &TypeCheckerContext, generic_args: &GenericMapping, e: &Expression) -> CompileResult<Expression>
{
    match *e
    {
        Expression::UnaryOp(ref op) => {
            let e = substitute_expr(ctx, generic_args, &op.expression)?;
            Ok(unary_op(op.operator, e, op.span.clone()))
        },

        Expression::BinaryOp(ref op) => {
            let l = substitute_expr(ctx, generic_args, &op.left)?;
            let r = substitute_expr(ctx, generic_args, &op.right)?;
            Ok(bin_op(op.operator, l, r, op.span.clone()))
        },

        Expression::Literal(Literal::Array(ref a)) => {
            substitute_array_literal(ctx, generic_args, a).map(Expression::Literal)
        },

        Expression::Call(ref c) => {
            let new_c = substitute_call(ctx, generic_args, c)?;
            Ok(Expression::Call(Box::new(new_c)))
        },

        Expression::Lambda(ref l) => {
            let mut args = Vec::with_capacity(l.sig.args.len());
            for a in &l.sig.args {
                args.push(
                    Argument::new(
                        a.name.clone(),
                        make_concrete(ctx, generic_args, &a.typ, &a.span)?,
                        a.mutable,
                        a.span.clone()
                    )
                )
            }
            let expr = substitute_expr(ctx, generic_args, &l.expr)?;
            Ok(lambda(args, expr, l.span.clone()))
        },

        Expression::Match(ref m) => {
            let target = substitute_expr(ctx, generic_args, &m.target)?;
            let mut cases = Vec::with_capacity(m.cases.len());
            for c in &m.cases
                {
                    let pattern = substitute_pattern(ctx, generic_args, &c.pattern)?;
                    let to_execute = substitute_expr(ctx, generic_args, &c.to_execute)?;
                    cases.push(match_case(pattern, to_execute, c.span.clone()));
                }
            Ok(match_expression(target, cases, m.span.clone()))
        },

        Expression::Bindings(ref l) => {
            let nb = substitute_bindings(ctx, generic_args, &l.bindings)?;
            Ok(bindings(nb, l.span.clone()))
        },

        Expression::If(ref i) => {
            Ok(if_expression(
                substitute_expr(ctx, generic_args, &i.condition)?,
                substitute_expr(ctx, generic_args, &i.on_true)?,
                substitute_expr(ctx, generic_args, &i.on_true)?,
                i.span.clone(),
            ))
        },

        Expression::Block(ref b) => {
            let mut new_expressions = Vec::with_capacity(b.expressions.len());
            for e in &b.expressions {
                new_expressions.push(substitute_expr(ctx, generic_args, e)?);
            }
            Ok(block(new_expressions, b.span.clone()))
        },

        Expression::Literal(ref lit) => Ok(Expression::Literal(lit.clone())),

        Expression::NameRef(ref nr) => {
            Ok(Expression::NameRef(substitute_name_ref(ctx, generic_args, nr)?))
        },

        Expression::StructInitializer(ref si) => {
            let mut nmi = Vec::with_capacity(si.member_initializers.len());
            for e in &si.member_initializers {
                let new_e = substitute_expr(ctx, generic_args, e)?;
                nmi.push(new_e);
            }

            Ok(Expression::StructInitializer(struct_initializer(&si.struct_name, nmi, si.span.clone())))
        },

        Expression::MemberAccess(ref sma) => {
            let ma = substitute_member_access(ctx, generic_args, sma)?;
            Ok(Expression::MemberAccess(Box::new(ma)))
        },

        Expression::New(ref n) => {
            let inner = substitute_expr(ctx, generic_args, &n.inner)?;
            Ok(new_with_type(inner, make_concrete(ctx, generic_args, &n.typ, &n.span)?, n.span.clone()))
        },

        Expression::Delete(ref n) => {
            let inner = substitute_expr(ctx, generic_args, &n.inner)?;
            Ok(delete(inner, n.span.clone()))
        },

        Expression::ArrayToSlice(ref ats) => {
            let inner = substitute_expr(ctx, generic_args, &ats.inner)?;
            Ok(array_to_slice(inner, ats.span.clone()))
        },

        Expression::AddressOf(ref a) => {
            let inner = substitute_expr(ctx, generic_args, &a.inner)?;
            Ok(address_of(inner, a.span.clone()))
        },

        Expression::Dereference(ref d) => {
            let inner = substitute_expr(ctx, generic_args, &d.inner)?;
            Ok(dereference(inner, d.span.clone()))
        }

        Expression::Assign(ref a) => {
            let l = match a.left {
                AssignTarget::Var(ref nr) =>
                    AssignTarget::Var(substitute_name_ref(ctx, generic_args, nr)?),

                AssignTarget::MemberAccess(ref ma) =>
                    AssignTarget::MemberAccess(substitute_member_access(ctx, generic_args, ma)?),

                AssignTarget::Dereference(ref d) => {
                    let inner = substitute_expr(ctx, generic_args, &d.inner)?;
                    AssignTarget::Dereference(DereferenceExpression{
                        inner,
                        typ: d.typ.clone(),
                        span: d.span.clone()
                    })
                },

                AssignTarget::IndexOperation(ref iop) => {
                    let target = substitute_expr(ctx, generic_args, &iop.target)?;
                    let index_expr = substitute_expr(ctx, generic_args, &iop.index_expr)?;
                    AssignTarget::IndexOperation(IndexOperation{
                        target,
                        index_expr,
                        span: iop.span.clone(),
                        typ: iop.typ.clone(),
                    })
                }
            };
            let r = substitute_expr(ctx, generic_args, &a.right)?;
            Ok(assign(a.operator, l, r, a.span.clone()))
        },

        Expression::While(ref w) => {
            let c = substitute_expr(ctx, generic_args, &w.cond)?;
            let b = substitute_expr(ctx, generic_args, &w.body)?;
            Ok(while_loop(c, b, w.span.clone()))
        },

        Expression::For(ref f) => {
            let i = substitute_expr(ctx, generic_args, &f.iterable)?;
            let b = substitute_expr(ctx, generic_args, &f.body)?;
            Ok(for_loop(&f.loop_variable, i, b, f.span.clone()))
        },

        Expression::Nil(ref span) => {
            Ok(Expression::Nil(span.clone()))
        },

        Expression::OptionalToBool(ref inner) => {
            Ok(Expression::OptionalToBool(inner.clone()))
        },

        Expression::ToOptional(ref t) => {
            let inner = substitute_expr(ctx, generic_args, &t.inner)?;
            Ok(to_optional(inner, t.optional_type.clone()))
        },

        Expression::Cast(ref t) => {
            let inner = substitute_expr(ctx, generic_args, &t.inner)?;
            Ok(type_cast(inner, make_concrete(ctx, generic_args, &t.destination_type, &t.span)?, t.span.clone()))
        },

        Expression::Void => Ok(Expression::Void),

        Expression::CompilerCall(CompilerCall::SizeOf(ref t, ref span)) => {
            let new_t = make_concrete(ctx, generic_args, t, span)?;
            Ok(Expression::CompilerCall(CompilerCall::SizeOf(new_t, span.clone())))
        }

        Expression::CompilerCall(CompilerCall::Slice{ref data, ref len, ref typ, ref span}) => {
            let new_data = substitute_expr(ctx, generic_args, data)?;
            let new_len = substitute_expr(ctx, generic_args, len)?;
            let new_type = make_concrete(ctx, generic_args, typ, span)?;
            Ok(Expression::CompilerCall(CompilerCall::Slice{
                data: Box::new(new_data),
                len: Box::new(new_len),
                typ: new_type,
                span: span.clone(),
            }))
        },

        Expression::IndexOperation(ref iop) => {
            let target = substitute_expr(ctx, generic_args, &iop.target)?;
            let index_expr = substitute_expr(ctx, generic_args, &iop.index_expr)?;
            Ok(index_op(target, index_expr, iop.span.clone()))
        }

        Expression::Return(ref r) => {
            let e = substitute_expr(ctx, generic_args, &r.expression)?;
            Ok(return_expr(e, r.span.clone()))
        }
    }
}

pub fn instantiate(ctx: &TypeCheckerContext, func: &Function, generic_args: &GenericMapping) -> CompileResult<Function>
{
    let mut arg_types = Vec::with_capacity(func.sig.args.len());
    let mut args = Vec::with_capacity(func.sig.args.len());
    for arg in &func.sig.args {
        let arg_typ = make_concrete(ctx, generic_args, &arg.typ, &arg.span)?;
        args.push(Argument::new(arg.name.clone(), arg_typ.clone(), arg.mutable, arg.span.clone()));
        arg_types.push(arg_typ);
    }

    let return_type = make_concrete(ctx, generic_args, &func.sig.return_type, &func.sig.span)?;
    let sig = FunctionSignature{
        name: new_func_name(&func.sig.name, generic_args),
        return_type: return_type.clone(),
        args: args,
        span: func.sig.span.clone(),
        typ: func_type(arg_types, return_type),
    };

    let body = substitute_expr(ctx, generic_args, &func.expression)?;
    Ok(Function::new(sig, func.public, body, func.span.clone()))
}