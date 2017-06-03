use std::ops::Deref;
use std::collections::HashMap;
use itertools::free::join;
use ast::*;
use span::Span;
use typechecker::typecheckercontext::TypeCheckerContext;
use compileerror::{CompileResult, unknown_name_result, type_error};


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


fn subsitute_bindings(ctx: &TypeCheckerContext, generic_args: &GenericMapping, lb: &[Binding]) -> CompileResult<Vec<Binding>>
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

        Expression::Binding(ref l) => {
            let bindings = subsitute_bindings(ctx, generic_args, &l.bindings)?;
            let expr = substitute_expr(ctx, generic_args, &l.expression)?;
            Ok(binding_expression(bindings, expr, l.span.clone()))
        },

        Expression::Bindings(ref l) => {
            let nb = subsitute_bindings(ctx, generic_args, &l.bindings)?;
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
                }
            };
            let r = substitute_expr(ctx, generic_args, &a.right)?;
            Ok(assign(l, r, a.span.clone()))
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
    }
}

fn new_func_name(func_name: &str, generic_args: &GenericMapping) -> String
{
    format!("{}<{}>", func_name, join(generic_args.values(), ","))
}

fn instantiate(ctx: &TypeCheckerContext, func: &Function, generic_args: &GenericMapping) -> CompileResult<Function>
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

type FunctionMap = HashMap<String, Function>;

fn resolve_generic_call(ctx: &TypeCheckerContext, new_functions: &mut FunctionMap, module: &Module, call: &Call) -> CompileResult<()>
{
    match module.functions.get(&call.callee.name)
    {
        None => {
            unknown_name_result(&call.span, format!("Unknown function {}", call.callee.name))
        },
        Some(func) => {
            let name = new_func_name(&func.sig.name, &call.generic_args);
            if !new_functions.contains_key(&name) && !module.functions.contains_key(&name) {
                let new_func = instantiate(ctx, func, &call.generic_args)?;
                new_functions.insert(name, new_func);
            }

            Ok(())
        }
    }
}

fn resolve_generics_in_pattern(ctx: &TypeCheckerContext, new_functions: &mut FunctionMap, module: &Module, p: &Pattern) -> CompileResult<()>
{
    match *p
    {
        Pattern::Literal(Literal::Array(ref al)) => {
            for el in &al.elements {
                resolve_generics(ctx, new_functions, module, el)?;
            }
            Ok(())
        },
        _ => Ok(())
    }
}

fn resolve_generics(ctx: &TypeCheckerContext, new_functions: &mut FunctionMap, module: &Module, e: &Expression) -> CompileResult<()>
{
    match *e
    {
        Expression::UnaryOp(ref op) => resolve_generics(ctx, new_functions, module, &op.expression),
        Expression::BinaryOp(ref op) => {
            resolve_generics(ctx, new_functions, module, &op.left)?;
            resolve_generics(ctx, new_functions, module, &op.right)
        },

        Expression::Literal(Literal::Array(ref a)) => {
            for el in &a.elements {
                resolve_generics(ctx, new_functions, module, el)?;
            }
            Ok(())
        },

        Expression::Call(ref c) => {
            for a in &c.args {
                resolve_generics(ctx, new_functions, module, a)?;
            }
            if !c.generic_args.is_empty() {
                resolve_generic_call(ctx, new_functions, module, c)
            } else {
                Ok(())
            }
        },

        Expression::Match(ref m) => {
            resolve_generics(ctx, new_functions, module, &m.target)?;
            for c in &m.cases
            {
                resolve_generics_in_pattern(ctx, new_functions, module, &c.pattern)?;
                resolve_generics(ctx, new_functions, module, &c.to_execute)?;
            }
            Ok(())
        },

        Expression::Lambda(ref l) => {
            resolve_generics(ctx, new_functions, module, &l.expr)
        },

        Expression::Binding(ref l) => {
            for b in &l.bindings {
                resolve_generics(ctx, new_functions, module, &b.init)?
            }

            resolve_generics(ctx, new_functions, module, &l.expression)
        },

        Expression::Bindings(ref l) => {
            for b in &l.bindings {
                resolve_generics(ctx, new_functions, module, &b.init)?
            }

            Ok(())
        },

        Expression::StructInitializer(ref si) => {
            for mi in &si.member_initializers {
                resolve_generics(ctx, new_functions, module, mi)?;
            }
            Ok(())
        }

        Expression::Block(ref b) => {
            for e in &b.expressions {
                resolve_generics(ctx, new_functions, module, e)?;
            }
            Ok(())
        },

        Expression::New(ref n) => {
            resolve_generics(ctx, new_functions, module, &n.inner)
        },

        Expression::Delete(ref n) => {
            resolve_generics(ctx, new_functions, module, &n.inner)
        },

        Expression::ArrayToSlice(ref ats) => {
            resolve_generics(ctx, new_functions, module, &ats.inner)
        },

        Expression::AddressOf(ref a) => {
            resolve_generics(ctx, new_functions, module, &a.inner)
        },

        Expression::Dereference(ref d) => {
            resolve_generics(ctx, new_functions, module, &d.inner)
        },

        Expression::Assign(ref a) => {
            resolve_generics(ctx, new_functions, module, &a.right)
        },

        Expression::While(ref w) => {
            resolve_generics(ctx, new_functions, module, &w.cond)?;
            resolve_generics(ctx, new_functions, module, &w.body)
        },

        Expression::For(ref f) => {
            resolve_generics(ctx, new_functions, module, &f.iterable)?;
            resolve_generics(ctx, new_functions, module, &f.body)
        },

        Expression::ToOptional(ref t) => {
            resolve_generics(ctx, new_functions, module, &t.inner)
        },

        Expression::Cast(ref t) => {
            resolve_generics(ctx, new_functions, module, &t.inner)
        },

        Expression::OptionalToBool(ref inner) => {
            resolve_generics(ctx, new_functions, module, inner)
        },

        Expression::CompilerCall(CompilerCall::SizeOf(_, _)) |
        Expression::Nil(_) |
        Expression::NameRef(_) |
        Expression::If(_) |
        Expression::MemberAccess(_) |
        Expression::Literal(_) |
        Expression::Void => Ok(()),
    }
}

fn replace_generic_calls_in_pattern(new_functions: &FunctionMap, p: &mut Pattern) -> CompileResult<()>
{
    match *p
    {
        Pattern::Literal(Literal::Array(ref mut al)) => {
            for el in &mut al.elements {
                replace_generic_calls(new_functions, el)?;
            }
            Ok(())
        },
        _ => Ok(())
    }
}

fn replace_generic_calls(new_functions: &FunctionMap, e: &mut Expression) -> CompileResult<()>
{
    match *e
    {
        Expression::UnaryOp(ref mut op) => replace_generic_calls(new_functions, &mut op.expression),
        Expression::BinaryOp(ref mut op) => {
            replace_generic_calls(new_functions, &mut op.left)?;
            replace_generic_calls(new_functions, &mut op.right)
        },

        Expression::Literal(Literal::Array(ref mut a)) => {
            for el in &mut a.elements {
                replace_generic_calls(new_functions, el)?;
            }
            Ok(())
        },

        Expression::Call(ref mut call) => {
            for a in &mut call.args {
                replace_generic_calls(new_functions, a)?;
            }

            if !call.generic_args.is_empty() {
                call.callee.name = new_func_name(&call.callee.name, &call.generic_args);
            }

            Ok(())
        },

        Expression::Lambda(ref mut l) => {
            replace_generic_calls(new_functions, &mut l.expr)
        },

        Expression::Match(ref mut m) => {
            replace_generic_calls(new_functions, &mut m.target)?;
            for c in &mut m.cases
            {
                replace_generic_calls_in_pattern(new_functions, &mut c.pattern)?;
                replace_generic_calls(new_functions, &mut c.to_execute)?;
            }
            Ok(())
        },

        Expression::Binding(ref mut l) => {
            for b in &mut l.bindings {
                replace_generic_calls(new_functions, &mut b.init)?
            }

            replace_generic_calls(new_functions, &mut l.expression)
        },

        Expression::Bindings(ref mut l) => {
            for b in &mut l.bindings {
                replace_generic_calls(new_functions, &mut b.init)?
            }
            Ok(())
        },

        Expression::Block(ref mut b) => {
            for e in &mut b.expressions {
                replace_generic_calls(new_functions, e)?;
            }
            Ok(())
        },

        Expression::New(ref mut n) => {
            replace_generic_calls(new_functions, &mut n.inner)?;
            Ok(())
        },

        Expression::Delete(ref mut d) => {
            replace_generic_calls(new_functions, &mut d.inner)?;
            Ok(())
        },

        Expression::ArrayToSlice(ref mut ats) => {
            replace_generic_calls(new_functions, &mut ats.inner)?;
            Ok(())
        },
        _ => Ok(()),
    }
}

/*
    Instantiate all generics
*/
pub fn instantiate_generics(module: &mut Module, ctx: &TypeCheckerContext) -> CompileResult<()>
{
    let mut new_functions = FunctionMap::new();
    for f in module.functions.values()
    {
        if !f.generics_resolved && !f.is_generic() {
            resolve_generics(ctx, &mut new_functions, module, &f.expression)?;
        }
    }

    for f in module.functions.values_mut()
    {
        if !f.generics_resolved && !f.is_generic() {
            replace_generic_calls(&new_functions, &mut f.expression)?;
            f.generics_resolved = true;
        }
    }

    module.functions.extend(new_functions.into_iter());
    Ok(())
}
