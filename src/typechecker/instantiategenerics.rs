use std::collections::HashMap;
use ast::*;
use compileerror::{CompileResult, ErrorCode, err};
use typechecker::GenericMapper;

fn subsitute_let_bindings(generic_args: &GenericMapper, lb: &Vec<LetBinding>) -> CompileResult<Vec<LetBinding>>
{
    let mut bindings = Vec::with_capacity(lb.len());
    for b in lb.iter() {
        let binding_expr = substitute_expr(generic_args, &b.init)?;
        let new_binding = match b.binding_type
        {
            LetBindingType::Name(ref name) => {
                let_name_binding(name.clone(), binding_expr, b.span.clone())
            },

            LetBindingType::Struct(ref s) => {
                let_binding(
                    LetBindingType::Struct(substitute_struct_pattern(generic_args, s)),
                    binding_expr,
                    b.span.clone()
                )
            },
        };
        bindings.push(new_binding);
    }
    Ok(bindings)
}

fn substitute_struct_pattern(generic_args: &GenericMapper, p: &StructPattern) -> StructPattern
{
    struct_pattern(
        &p.name,
        p.bindings.clone(),
        p.types.iter().map(|t| generic_args.substitute(t)).collect(),
        generic_args.substitute(&p.typ),
        p.span.clone()
    )
}

fn substitute_pattern(generic_args: &GenericMapper, p: &Pattern) -> CompileResult<Pattern>
{
    match *p
    {
        Pattern::Struct(ref sp) => {
            Ok(Pattern::Struct(substitute_struct_pattern(generic_args, sp)))
        },

        Pattern::Name(ref nr) => {
            let new_nr = NameRef{
                name: nr.name.clone(),
                span: nr.span.clone(),
                typ: generic_args.substitute(&nr.typ),
            };
            Ok(Pattern::Name(new_nr))
        },

        Pattern::Literal(Literal::Array(ref al)) => {
            substitute_array_literal(generic_args, al).map(|al| Pattern::Literal(al))
        },

        _ => Ok(p.clone()),
    }
}

fn substitute_array_literal(generic_args: &GenericMapper, al: &ArrayLiteral) -> CompileResult<Literal>
{
    let mut new_elements = Vec::with_capacity(al.elements.len());
    for el in al.elements.iter() {
        new_elements.push(substitute_expr(generic_args, el)?);
    }
    Ok(array_lit(new_elements, al.span.clone()))
}

fn substitute_call(generic_args: &GenericMapper, c: &Call) -> CompileResult<Call>
{
    let mut new_args = Vec::with_capacity(c.args.len());
    for a in c.args.iter() {
        new_args.push(substitute_expr(generic_args, a)?);
    }

    Ok(Call::new(c.callee.clone(), new_args, c.span.clone()))
}

fn substitute_expr(generic_args: &GenericMapper, e: &Expression) -> CompileResult<Expression>
{
    match *e
    {
        Expression::UnaryOp(ref op) => {
            let e = substitute_expr(generic_args, &op.expression)?;
            Ok(unary_op(op.operator, e, op.span.clone()))
        },

        Expression::BinaryOp(ref op) => {
            let l = substitute_expr(generic_args, &op.left)?;
            let r = substitute_expr(generic_args, &op.right)?;
            Ok(bin_op(op.operator, l, r, op.span.clone()))
        },

        Expression::Literal(Literal::Array(ref a)) => {
            substitute_array_literal(generic_args, a).map(|al| Expression::Literal(al))
        },

        Expression::Call(ref c) => {
            let new_c = substitute_call(generic_args, c)?;
            Ok(Expression::Call(new_c))
        },

        Expression::Lambda(ref l) => {
            let args: Vec<Argument> = l.sig.args.iter().map(|a| Argument::new(a.name.clone(), generic_args.substitute(&a.typ), a.span.clone())).collect();
            let expr = substitute_expr(generic_args, &l.expr)?;
            Ok(lambda(args, expr, l.span.clone()))
        },

        Expression::Match(ref m) => {
            let target = substitute_expr(generic_args, &m.target)?;
            let mut cases = Vec::with_capacity(m.cases.len());
            for c in m.cases.iter()
            {
                let pattern = substitute_pattern(generic_args, &c.pattern)?;
                let to_execute = substitute_expr(generic_args, &c.to_execute)?;
                cases.push(match_case(pattern, to_execute, c.span.clone()));
            }
            Ok(match_expression(target, cases, m.span.clone()))
        },

        Expression::Let(ref l) => {
            let bindings = subsitute_let_bindings(generic_args, &l.bindings)?;
            let expr = substitute_expr(generic_args, &l.expression)?;
            Ok(let_expression(bindings, expr, l.span.clone()))
        },

        Expression::LetBindings(ref l) => {
            let bindings = subsitute_let_bindings(generic_args, &l.bindings)?;
            Ok(let_bindings(bindings, l.span.clone()))
        },

        Expression::If(ref i) => {
            Ok(if_expression(
                substitute_expr(generic_args, &i.condition)?,
                substitute_expr(generic_args, &i.on_true)?,
                substitute_expr(generic_args, &i.on_true)?,
                i.span.clone(),
            ))
        },

        Expression::Block(ref b) => {
            let mut new_expressions = Vec::with_capacity(b.expressions.len());
            for e in &b.expressions {
                new_expressions.push(substitute_expr(generic_args, e)?);
            }
            Ok(block(new_expressions, b.span.clone()))
        },

        Expression::Literal(ref lit) => Ok(Expression::Literal(lit.clone())),

        Expression::NameRef(ref nr) => {
            let new_nr = NameRef{
                name: nr.name.clone(),
                span: nr.span.clone(),
                typ: generic_args.substitute(&nr.typ),
            };
            Ok(Expression::NameRef(new_nr))
        },

        Expression::StructInitializer(ref si) => {
            let mut nmi = Vec::with_capacity(si.member_initializers.len());
            for e in si.member_initializers.iter() {
                let new_e = substitute_expr(generic_args, e)?;
                nmi.push(new_e);
            }

            Ok(Expression::StructInitializer(struct_initializer(&si.struct_name, nmi, si.span.clone())))
        },

        Expression::MemberAccess(ref sma) => {
            let left = substitute_expr(generic_args, &sma.left)?;
            let right = match sma.right
            {
                MemberAccessType::Call(ref c) => {
                    let new_c = substitute_call(generic_args, c)?;
                    MemberAccessType::Call(new_c)
                },
                _ => sma.right.clone(),
            };

            Ok(member_access(left, right, sma.span.clone()))
        },

        Expression::New(ref n) => {
            let inner = substitute_expr(generic_args, &n.inner)?;
            Ok(new_with_type(inner, generic_args.substitute(&n.typ), n.span.clone()))
        },

        Expression::Delete(ref n) => {
            let inner = substitute_expr(generic_args, &n.inner)?;
            Ok(delete(inner, n.span.clone()))
        },

        Expression::ArrayToSlice(ref ats) => {
            let inner = substitute_expr(generic_args, &ats.inner)?;
            Ok(array_to_slice(inner, ats.span.clone()))
        },

        Expression::AddressOf(ref a) => {
            let inner = substitute_expr(generic_args, &a.inner)?;
            Ok(address_of(inner, a.span.clone()))
        },

        Expression::Assign(ref a) => {
            let l = substitute_expr(generic_args, &a.left)?;
            let r = substitute_expr(generic_args, &a.right)?;
            Ok(assign(l, r, a.span.clone()))
        },

        Expression::While(ref w) => {
            let c = substitute_expr(generic_args, &w.cond)?;
            let b = substitute_expr(generic_args, &w.body)?;
            Ok(while_loop(c, b, w.span.clone()))
        },

        Expression::Nil(ref span) => {
            Ok(Expression::Nil(span.clone()))
        },

        Expression::ToOptional(ref t) => {
            let inner = substitute_expr(generic_args, &t.inner)?;
            Ok(to_optional(inner, t.optional_type.clone()))
        },

        Expression::Void => Ok(Expression::Void),
    }
}

fn new_func_name(func_name: &str, generic_args: &GenericMapper) -> String
{
    format!("{}{}", func_name, generic_args.to_string())
}

fn instantiate(func: &Function, generic_args: &GenericMapper) -> CompileResult<Function>
{
    let arg_types = func.sig.args.iter()
        .map(|arg| generic_args.substitute(&arg.typ))
        .collect();
    let args = func.sig.args.iter()
        .map(|arg| Argument::new(arg.name.clone(), generic_args.substitute(&arg.typ), arg.span.clone()))
        .collect();
    let return_type = generic_args.substitute(&func.sig.return_type);
    let sig = FunctionSignature{
        name: new_func_name(&func.sig.name, generic_args),
        return_type: return_type.clone(),
        args: args,
        span: func.sig.span.clone(),
        typ: func_type(arg_types, return_type),
    };

    let body = substitute_expr(generic_args, &func.expression)?;
    Ok(Function::new(sig, func.public, body, func.span.clone()))
}

type FunctionMap = HashMap<String, Function>;

fn resolve_generic_call(new_functions: &mut FunctionMap, module: &Module, call: &Call) -> CompileResult<()>
{
    match module.functions.get(&call.callee.name)
    {
        None => {
            err(&call.span, ErrorCode::UnknownName, format!("Unknown function {}", call.callee.name))
        },
        Some(ref func) => {
            let name = new_func_name(&func.sig.name, &call.generic_args);
            if !new_functions.contains_key(&name) && !module.functions.contains_key(&name) {
                let new_func = instantiate(func, &call.generic_args)?;
                new_functions.insert(name, new_func);
            }

            Ok(())
        }
    }
}

fn resolve_generics_in_pattern(new_functions: &mut FunctionMap, module: &Module, p: &Pattern) -> CompileResult<()>
{
    match *p
    {
        Pattern::Literal(Literal::Array(ref al)) => {
            for el in al.elements.iter() {
                resolve_generics(new_functions, module, el)?;
            }
            Ok(())
        },
        _ => Ok(())
    }
}

fn resolve_generics(new_functions: &mut FunctionMap, module: &Module, e: &Expression) -> CompileResult<()>
{
    match *e
    {
        Expression::UnaryOp(ref op) => resolve_generics(new_functions, module, &op.expression),
        Expression::BinaryOp(ref op) => {
            resolve_generics(new_functions, module, &op.left)?;
            resolve_generics(new_functions, module, &op.right)
        },

        Expression::Literal(Literal::Array(ref a)) => {
            for el in a.elements.iter() {
                resolve_generics(new_functions, module, el)?;
            }
            Ok(())
        },

        Expression::Call(ref c) => {
            for a in c.args.iter() {
                resolve_generics(new_functions, module, a)?;
            }
            if !c.generic_args.is_empty() {
                resolve_generic_call(new_functions, module, c)
            } else {
                Ok(())
            }
        },

        Expression::Match(ref m) => {
            resolve_generics(new_functions, module, &m.target)?;
            for c in m.cases.iter()
            {
                resolve_generics_in_pattern(new_functions, module, &c.pattern)?;
                resolve_generics(new_functions, module, &c.to_execute)?;
            }
            Ok(())
        },

        Expression::Lambda(ref l) => {
            resolve_generics(new_functions, module, &l.expr)
        },

        Expression::Let(ref l) => {
            for b in l.bindings.iter() {
                resolve_generics(new_functions, module, &b.init)?
            }

            resolve_generics(new_functions, module, &l.expression)
        },

        Expression::LetBindings(ref l) => {
            for b in l.bindings.iter() {
                resolve_generics(new_functions, module, &b.init)?
            }

            Ok(())
        },

        Expression::StructInitializer(ref si) => {
            for mi in si.member_initializers.iter() {
                resolve_generics(new_functions, module, mi)?;
            }
            Ok(())
        }

        Expression::Block(ref b) => {
            for e in &b.expressions {
                resolve_generics(new_functions, module, e)?;
            }
            Ok(())
        },

        Expression::New(ref n) => {
            resolve_generics(new_functions, module, &n.inner)
        },

        Expression::Delete(ref n) => {
            resolve_generics(new_functions, module, &n.inner)
        },

        Expression::ArrayToSlice(ref ats) => {
            resolve_generics(new_functions, module, &ats.inner)
        },

        Expression::AddressOf(ref a) => {
            resolve_generics(new_functions, module, &a.inner)
        },

        Expression::Assign(ref a) => {
            resolve_generics(new_functions, module, &a.left)?;
            resolve_generics(new_functions, module, &a.right)
        },

        Expression::While(ref w) => {
            resolve_generics(new_functions, module, &w.cond)?;
            resolve_generics(new_functions, module, &w.body)
        },

        Expression::ToOptional(ref t) => {
            resolve_generics(new_functions, module, &t.inner)
        },

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
            for el in al.elements.iter_mut() {
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
            for el in a.elements.iter_mut() {
                replace_generic_calls(new_functions, el)?;
            }
            Ok(())
        },

        Expression::Call(ref mut call) => {
            for a in call.args.iter_mut() {
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
            for c in m.cases.iter_mut()
            {
                replace_generic_calls_in_pattern(new_functions, &mut c.pattern)?;
                replace_generic_calls(new_functions, &mut c.to_execute)?;
            }
            Ok(())
        },

        Expression::Let(ref mut l) => {
            for b in l.bindings.iter_mut() {
                replace_generic_calls(new_functions, &mut b.init)?
            }

            replace_generic_calls(new_functions, &mut l.expression)
        },

        Expression::LetBindings(ref mut l) => {
            for b in l.bindings.iter_mut() {
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
pub fn instantiate_generics(module: &mut Module) -> CompileResult<()>
{
    let mut new_functions = FunctionMap::new();
    for (_, ref f) in module.functions.iter()
    {
        if !f.generics_resolved && !f.is_generic() {
            resolve_generics(&mut new_functions, module, &f.expression)?;
        }
    }

    for (_, ref mut f) in module.functions.iter_mut()
    {
        if !f.generics_resolved && !f.is_generic() {
            replace_generic_calls(&new_functions, &mut f.expression)?;
            f.generics_resolved = true;
        }
    }

    module.functions.extend(new_functions.into_iter());
    Ok(())
}
