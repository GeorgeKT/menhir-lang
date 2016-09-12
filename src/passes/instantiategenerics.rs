use std::collections::HashMap;
use ast::*;
use compileerror::{CompileResult, ErrorCode, err};
use passes::GenericMapper;

fn subsitute_let_bindings(generic_args: &GenericMapper, lb: &Vec<LetBinding>) -> CompileResult<Vec<LetBinding>>
{
    let mut bindings = Vec::with_capacity(lb.len());
    for b in lb.iter() {
        let binding_expr = try!(substitute_expr(generic_args, &b.init));
        bindings.push(let_binding(b.name.clone(), binding_expr, b.span.clone()));
    }
    Ok(bindings)
}

fn substitute_expr(generic_args: &GenericMapper, e: &Expression) -> CompileResult<Expression>
{
    match *e
    {
        Expression::UnaryOp(ref op) => {
            let e = try!(substitute_expr(generic_args, &op.expression));
            Ok(unary_op(op.operator, e, op.span.clone()))
        },

        Expression::BinaryOp(ref op) => {
            let l = try!(substitute_expr(generic_args, &op.left));
            let r = try!(substitute_expr(generic_args, &op.right));
            Ok(bin_op(op.operator, l, r, op.span.clone()))
        },

        Expression::ArrayLiteral(ref a) => {
            let mut new_elements = Vec::with_capacity(a.elements.len());
            for el in a.elements.iter() {
                new_elements.push(try!(substitute_expr(generic_args, el)));
            }
            Ok(array_lit(new_elements, a.span.clone()))
        },

        Expression::ArrayGenerator(ref a) => {
            let iterable = try!(substitute_expr(generic_args, &a.iterable));
            let left = try!(substitute_expr(generic_args, &a.left));
            Ok(array_generator(left, &a.var, iterable, a.span.clone()))
        },

        Expression::Call(ref c) => {
            let mut new_args = Vec::with_capacity(c.args.len());
            for a in c.args.iter() {
                new_args.push(try!(substitute_expr(generic_args, a)));
            }

            Ok(Expression::Call(Call::new(c.callee.clone(), new_args, c.span.clone())))
        },

        Expression::Lambda(ref l) => {
            let args: Vec<Argument> = l.sig.args.iter().map(|a| Argument::new(a.name.clone(), generic_args.substitute(&a.typ), a.span.clone())).collect();
            let expr = try!(substitute_expr(generic_args, &l.expr));
            Ok(lambda(args, expr, l.span.clone()))
        },

        Expression::Match(ref m) => {
            let target = try!(substitute_expr(generic_args, &m.target));
            let mut cases = Vec::with_capacity(m.cases.len());
            for c in m.cases.iter()
            {
                let match_expr = try!(substitute_expr(generic_args, &c.match_expr));
                let to_execute = try!(substitute_expr(generic_args, &c.to_execute));
                cases.push(match_case(match_expr, to_execute, c.span.clone()));
            }
            Ok(match_expression(target, cases, m.span.clone()))
        },

        Expression::Let(ref l) => {
            let bindings = try!(subsitute_let_bindings(generic_args, &l.bindings));
            let expr = try!(substitute_expr(generic_args, &l.expression));
            Ok(let_expression(bindings, expr, l.span.clone()))
        },

        Expression::LetBindings(ref l) => {
            let bindings = try!(subsitute_let_bindings(generic_args, &l.bindings));
            Ok(let_bindings(bindings, l.span.clone()))
        },

        Expression::If(ref i) => {
            Ok(if_expression(
                 try!(substitute_expr(generic_args, &i.condition)),
                 try!(substitute_expr(generic_args, &i.on_true)),
                 try!(substitute_expr(generic_args, &i.on_true)),
                 i.span.clone(),
            ))
        },

        Expression::Block(ref b) => {
            let mut new_expressions = Vec::with_capacity(b.expressions.len());
            for e in &b.expressions {
                new_expressions.push(try!(substitute_expr(generic_args, e)));
            }
            Ok(block(new_expressions, b.span.clone()))
        },
        Expression::IntLiteral(ref span, ref v) => Ok(Expression::IntLiteral(span.clone(), v.clone())),
        Expression::BoolLiteral(ref span, ref v) => Ok(Expression::BoolLiteral(span.clone(), v.clone())),
        Expression::FloatLiteral(ref span, ref v) => Ok(Expression::FloatLiteral(span.clone(), v.clone())),
        Expression::StringLiteral(ref span, ref v) => Ok(Expression::StringLiteral(span.clone(), v.clone())),
        Expression::ArrayPattern(ref ap) => Ok(Expression::ArrayPattern(ap.clone())),
        Expression::EmptyArrayPattern(ref ap) => Ok(Expression::EmptyArrayPattern(ap.clone())),
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
                let new_e = try!(substitute_expr(generic_args, e));
                nmi.push(new_e);
            }

            Ok(Expression::StructInitializer(struct_initializer(&si.struct_name, nmi, si.span.clone())))
        },
        Expression::StructMemberAccess(ref sma) => {
            Ok(Expression::StructMemberAccess(sma.clone()))
        },
        Expression::StructPattern(ref p) => {
            Ok(Expression::StructPattern(struct_pattern(
                &p.name,
                p.bindings.clone(),
                p.types.iter().map(|t| generic_args.substitute(t)).collect(),
                generic_args.substitute(&p.typ),
                p.span.clone()))
            )
        },
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

    let body = try!(substitute_expr(generic_args, &func.expression));
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

        Expression::StructInitializer(ref si) => {
            for mi in si.member_initializers.iter() {
                try!(resolve_generics(new_functions, module, mi));
            }
            Ok(())
        }

        Expression::Block(ref b) => {
            for e in &b.expressions {
                try!(resolve_generics(new_functions, module, e));
            }
            Ok(())
        },
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
                call.callee.name = new_func_name(&call.callee.name, &call.generic_args);
            }

            Ok(())
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

        Expression::Block(ref mut b) => {
            for e in &mut b.expressions {
                try!(replace_generic_calls(new_functions, e));
            }
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
            try!(resolve_generics(&mut new_functions, module, &f.expression));
        }
    }

    for (_, ref mut f) in module.functions.iter_mut()
    {
        if !f.generics_resolved && !f.is_generic() {
            try!(replace_generic_calls(&new_functions, &mut f.expression));
            f.generics_resolved = true;
        }
    }

    module.functions.extend(new_functions.into_iter());
    Ok(())
}
