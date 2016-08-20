use std::collections::HashMap;
use itertools::free::join;
use ast::{Module, Expression, Function, FunctionSignature, Argument, Type, Call, unary_op, bin_op, array_lit,
    array_generator, match_case, match_expression, let_expression, let_binding, sig, lambda};
use compileerror::{CompileResult, ErrorCode, err};


fn substitute(generic_args: &HashMap<Type, Type>, t: &Type) -> Type
{
    if let Some(new_t) = generic_args.get(&t) {
        new_t.clone()
    } else {
        t.clone()
    }
}

fn substitute_types(generic_args: &HashMap<Type, Type>, e: &Expression) -> CompileResult<Expression>
{
    match *e
    {
        Expression::UnaryOp(ref op) => {
            let e = try!(substitute_types(generic_args, &op.expression));
            Ok(unary_op(op.operator, e, op.span))
        },

        Expression::BinaryOp(ref op) => {
            let l = try!(substitute_types(generic_args, &op.left));
            let r = try!(substitute_types(generic_args, &op.right));
            Ok(bin_op(op.operator, l, r, op.span))
        },

        Expression::ArrayLiteral(ref a) => {
            let mut new_elements = Vec::with_capacity(a.elements.len());
            for el in a.elements.iter() {
                new_elements.push(try!(substitute_types(generic_args, el)));
            }
            Ok(array_lit(new_elements, a.span))
        },

        Expression::ArrayGenerator(ref a) => {
            let iterable = try!(substitute_types(generic_args, &a.iterable));
            let left = try!(substitute_types(generic_args, &a.left));
            Ok(array_generator(left, &a.var, iterable, a.span))
        },

        Expression::Call(ref c) => {
            let mut new_args = Vec::with_capacity(c.args.len());
            for a in c.args.iter() {
                new_args.push(try!(substitute_types(generic_args, a)));
            }

            Ok(Expression::Call(Call::new(c.callee.clone(), new_args, c.span)))
        },

        Expression::Function(ref f) => {
            let args: Vec<Argument> = f.sig.args.iter().map(|a| Argument::new(a.name.clone(), substitute(generic_args, &a.typ), a.span)).collect();
            let ret_type = substitute(generic_args, &f.sig.return_type);
            let body = try!(substitute_types(generic_args, &f.expression));

            let new_f = Function::new(sig(&f.sig.name, ret_type, args, f.sig.span), f.public, body, f.span);
            Ok(Expression::Function(new_f))
        },

        Expression::Lambda(ref l) => {
            let args: Vec<Argument> = l.sig.args.iter().map(|a| Argument::new(a.name.clone(), substitute(generic_args, &a.typ), a.span)).collect();
            let expr = try!(substitute_types(generic_args, &l.expr));
            Ok(Expression::Lambda(lambda(args, expr, l.span)))
        },

        Expression::Match(ref m) => {
            let target = try!(substitute_types(generic_args, &m.target));
            let mut cases = Vec::with_capacity(m.cases.len());
            for c in m.cases.iter()
            {
                let match_expr = try!(substitute_types(generic_args, &c.match_expr));
                let to_execute = try!(substitute_types(generic_args, &c.to_execute));
                cases.push(match_case(match_expr, to_execute, c.span));
            }
            Ok(Expression::Match(match_expression(target, cases, m.span)))
        },

        Expression::Let(ref l) => {
            let mut bindings = Vec::with_capacity(l.bindings.len());
            for b in l.bindings.iter() {
                let binding_expr = try!(substitute_types(generic_args, &b.init));
                bindings.push(let_binding(b.name.clone(), binding_expr, b.span));
            }

            let expr = try!(substitute_types(generic_args, &l.expression));
            Ok(let_expression(bindings, expr, l.span))
        },

        Expression::Enclosed(span, ref inner) => {
            let new_inner = try!(substitute_types(generic_args, inner));
            Ok(Expression::Enclosed(span, Box::new(new_inner)))
        },
        Expression::ArrayToSliceConversion(ref inner) => {
            let new_inner = try!(substitute_types(generic_args, inner));
            Ok(Expression::ArrayToSliceConversion(Box::new(new_inner)))
        },
        Expression::IntLiteral(span, v) => Ok(Expression::IntLiteral(span, v)),
        Expression::BoolLiteral(span, v) => Ok(Expression::BoolLiteral(span, v)),
        Expression::FloatLiteral(span, ref v) => Ok(Expression::FloatLiteral(span, v.clone())),
        Expression::StringLiteral(span, ref v) => Ok(Expression::StringLiteral(span, v.clone())),
        Expression::ArrayPattern(ref ap) => Ok(Expression::ArrayPattern(ap.clone())),
        Expression::NameRef(ref nr) => Ok(Expression::NameRef(nr.clone())),
    }
}

fn new_func_name(func_name: &str, generic_args: &HashMap<Type, Type>) -> String
{
    format!("{}<{}>", func_name, join(generic_args.values(), ","))
}

fn instantiate(func: &Function, generic_args: &HashMap<Type, Type>) -> CompileResult<Function>
{
    let sig = FunctionSignature{
        name: new_func_name(&func.sig.name, generic_args),
        return_type: substitute(generic_args, &func.sig.return_type),
        args: func.sig.args.iter().map(|arg| Argument::new(arg.name.clone(), substitute(generic_args, &arg.typ), arg.span)).collect(),
        span: func.sig.span,
    };

    let body = try!(substitute_types(generic_args, &func.expression));

    Ok(Function::new(sig, func.public, body, func.span))
}

type FunctionMap = HashMap<String, Function>;

fn resolve_generic_call(new_functions: &mut FunctionMap, module: &Module, call: &Call) -> CompileResult<()>
{
    match module.functions.get(&call.callee.name)
    {
        None => {
            err(call.span.start, ErrorCode::RedefinitionOfFunction, format!("Unknown function {}", call.callee.name))
        },
        Some(ref func) => {
            let name = new_func_name(&func.sig.name, &call.generic_args);
            if !new_functions.contains_key(&name) {
                let new_func = try!(instantiate(func, &call.generic_args));
                new_functions.insert(name, new_func);
            }

            Ok(())
        }
    }
}

fn resolve_generics(new_functions: &mut FunctionMap, module: &Module, e: &Expression) -> CompileResult<()>
{
    match *e
    {
        Expression::UnaryOp(ref op) => resolve_generics(new_functions, module, &op.expression),
        Expression::BinaryOp(ref op) => {
            try!(resolve_generics(new_functions, module, &op.left));
            resolve_generics(new_functions, module, &op.right)
        },

        Expression::ArrayLiteral(ref a) => {
            for el in a.elements.iter() {
                try!(resolve_generics(new_functions, module, el));
            }
            Ok(())
        },

        Expression::ArrayGenerator(ref a) => {
            try!(resolve_generics(new_functions, module, &a.iterable));
            resolve_generics(new_functions, module, &a.left)
        },

        Expression::Call(ref c) => {
            for a in c.args.iter() {
                try!(resolve_generics(new_functions, module, a));
            }
            if !c.generic_args.is_empty() {
                resolve_generic_call(new_functions, module, c)
            } else {
                Ok(())
            }
        },

        Expression::Function(ref f) => {
            resolve_generics(new_functions, module, &f.expression)
        },

        Expression::Match(ref m) => {
            try!(resolve_generics(new_functions, module, &m.target));
            for c in m.cases.iter()
            {
                try!(resolve_generics(new_functions, module, &c.match_expr));
                try!(resolve_generics(new_functions, module, &c.to_execute));
            }
            Ok(())
        },

        Expression::Lambda(ref l) => {
            resolve_generics(new_functions, module, &l.expr)
        },

        Expression::Let(ref l) => {
            for b in l.bindings.iter() {
                try!(resolve_generics(new_functions, module, &b.init))
            }

            resolve_generics(new_functions, module, &l.expression)
        },

        Expression::Enclosed(_, ref inner) => resolve_generics(new_functions, module, inner),
        Expression::ArrayToSliceConversion(ref inner) => resolve_generics(new_functions, module, inner),
        _ => Ok(()),
    }
}

fn replace_generic_calls(new_functions: &FunctionMap, e: &mut Expression) -> CompileResult<()>
{
    match *e
    {
        Expression::UnaryOp(ref mut op) => replace_generic_calls(new_functions, &mut op.expression),
        Expression::BinaryOp(ref mut op) => {
            try!(replace_generic_calls(new_functions, &mut op.left));
            replace_generic_calls(new_functions, &mut op.right)
        },

        Expression::ArrayLiteral(ref mut a) => {
            for el in a.elements.iter_mut() {
                try!(replace_generic_calls(new_functions, el));
            }
            Ok(())
        },

        Expression::ArrayGenerator(ref mut a) => {
            try!(replace_generic_calls(new_functions, &mut a.iterable));
            replace_generic_calls(new_functions, &mut a.left)
        },

        Expression::Call(ref mut call) => {
            for a in call.args.iter_mut() {
                try!(replace_generic_calls(new_functions, a));
            }

            if !call.generic_args.is_empty() {
                let name = new_func_name(&call.callee.name, &call.generic_args);
                if !new_functions.contains_key(&name) {
                    return err(call.span.start, ErrorCode::RedefinitionOfFunction, format!("Unknown function {}", call.callee.name));
                }

                call.callee.name = name;
            }

            Ok(())
        },

        Expression::Function(ref mut f) => {
            replace_generic_calls(new_functions, &mut f.expression)
        },

        Expression::Lambda(ref mut l) => {
            replace_generic_calls(new_functions, &mut l.expr)
        },

        Expression::Match(ref mut m) => {
            try!(replace_generic_calls(new_functions, &mut m.target));
            for c in m.cases.iter_mut()
            {
                try!(replace_generic_calls(new_functions, &mut c.match_expr));
                try!(replace_generic_calls(new_functions, &mut c.to_execute));
            }
            Ok(())
        },

        Expression::Let(ref mut l) => {
            for b in l.bindings.iter_mut() {
                try!(replace_generic_calls(new_functions, &mut b.init))
            }

            replace_generic_calls(new_functions, &mut l.expression)
        },

        Expression::Enclosed(_, ref mut inner) => replace_generic_calls(new_functions, inner),
        Expression::ArrayToSliceConversion(ref mut inner) => replace_generic_calls(new_functions, inner),
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
        if !f.generics_resolved {
            try!(resolve_generics(&mut new_functions, module, &f.expression));
        }
    }


    if new_functions.is_empty() {return Ok(());}

    for (_, ref mut f) in module.functions.iter_mut()
    {
        if !f.generics_resolved {
            try!(replace_generic_calls(&new_functions, &mut f.expression));
            f.generics_resolved = true;
        }
    }

    module.functions.extend(new_functions.into_iter());
    Ok(())
}
