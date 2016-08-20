use std::collections::HashMap;
use std::ops::{DerefMut, Deref};
use ast::{Module, Expression, NameRef, UnaryOp, BinaryOp, ArrayLiteral,
    ArrayGenerator, MatchExpression, Function, Lambda, Call, Type, LetExpression, ArgumentPassingMode,
    func_type, array_type, slice_type};
use compileerror::{CompileResult, Pos, CompileError, ErrorCode, err};
use parser::{Operator};
use passes::instantiate_generics;


pub struct StackFrame
{
    variables: HashMap<String, Type>,
    functions: HashMap<String, Type>,
}

impl StackFrame
{
    pub fn new() -> StackFrame
    {
        StackFrame{
            variables: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    pub fn resolve_type(&self, name: &str) -> Option<Type>
    {
        let v = self.variables.get(name).map(|t| t.clone());
        if v.is_some() {
            return v;
        }

        self.functions.get(name).map(|t| t.clone())
    }

    pub fn add_function(&mut self, name: &str, sig: Type)
    {
        self.functions.insert(name.into(), sig);
    }

    pub fn add_variable(&mut self, name: &str, t: Type, pos: Pos) -> CompileResult<()>
    {
        if self.variables.insert(name.into(), t).is_some() {
            err(pos, ErrorCode::RedefinitionOfVariable, format!("Variable {} has already been defined", name))
        } else {
            Ok(())
        }
    }
}

pub struct TypeCheckerContext
{
    stack: Vec<StackFrame>,
}

impl TypeCheckerContext
{
    pub fn new() -> TypeCheckerContext
    {
        TypeCheckerContext{
            stack: vec![StackFrame::new()],
        }
    }

    pub fn resolve_type(&self, name: &str) -> Option<Type>
    {
        for sf in self.stack.iter().rev() {
            let t = sf.resolve_type(name);
            if t.is_some() {
                return t;
            }
        }

        None
    }

    pub fn add_function(&mut self, name: &str, sig: Type)
    {
        self.stack.last_mut().expect("Empty stack").add_function(name, sig)
    }

    pub fn add_variable(&mut self, name: &str, t: Type, pos: Pos) -> CompileResult<()>
    {
        self.stack.last_mut().expect("Empty stack").add_variable(name, t, pos)
    }

    pub fn push_stack(&mut self)
    {
        self.stack.push(StackFrame::new());
    }

    pub fn pop_stack(&mut self)
    {
        self.stack.pop();
    }
}

fn invalid_unary_operator<T>(pos: Pos, op: Operator) -> CompileResult<T>
{
    err(pos, ErrorCode::InvalidUnaryOperator, format!("{} is not a valid unary operator", op))
}

fn expected_numeric_operands<T>(pos: Pos, op: Operator) -> CompileResult<T>
{
    err(pos, ErrorCode::TypeError, format!("Operator {} expects two numeric expression as operands", op))
}

fn unknown_name(pos: Pos, name: &str) -> CompileError
{
    CompileError::new(pos, ErrorCode::UnknownName, format!("Unable to resolve name {}", name))
}

fn is_numeric(t: &Type) -> bool
{
    *t == Type::Int || *t == Type::Float
}

fn is_integer(t: &Type) -> bool
{
    *t == Type::Int
}

fn is_bool(t: &Type) -> bool
{
    *t == Type::Bool
}

fn infer_and_check_unary_op(ctx: &mut TypeCheckerContext, u: &mut UnaryOp) -> CompileResult<Type>
{
    let e_type = try!(infer_and_check_expression(ctx, &mut u.expression, None));
    match u.operator
    {
        Operator::Sub => {
            if !is_numeric(&e_type) {
                err(u.span.start, ErrorCode::TypeError, format!("Unary operator {} expects a numeric expression", u.operator))
            } else {
                Ok(e_type)
            }
        },

        Operator::Not => {
            if !is_bool(&e_type) {
                err(u.span.start, ErrorCode::TypeError, format!("Unary operator {} expects a boolean expression", u.operator))
            } else {
                Ok(Type::Bool)
            }
        }
        _ => invalid_unary_operator(u.span.start, u.operator),
    }
}

fn infer_and_check_binary_op(ctx: &mut TypeCheckerContext, b: &mut BinaryOp) -> CompileResult<Type>
{
    let left_type = try!(infer_and_check_expression(ctx, &mut b.left, None));
    let right_type = try!(infer_and_check_expression(ctx, &mut b.right, None));
    match b.operator
    {
        Operator::Add |
        Operator::Sub |
        Operator::Mul |
        Operator::Div =>
            if !is_numeric(&left_type) || !is_numeric(&right_type) {
                expected_numeric_operands(b.span.start, b.operator)
            } else if left_type != right_type {
                err(b.span.start, ErrorCode::TypeError, format!("Operator {} expects operands of the same type", b.operator))
            } else {
                Ok(left_type)
            },

        Operator::LessThan |
        Operator::GreaterThan |
        Operator::LessThanEquals |
        Operator::GreaterThanEquals =>
            if !is_numeric(&left_type) || !is_numeric(&right_type) {
                expected_numeric_operands(b.span.start, b.operator)
            } else if left_type != right_type {
                err(b.span.start, ErrorCode::TypeError, format!("Operator {} expects operands of the same type", b.operator))
            } else {
                Ok(Type::Bool)
            },

        Operator::Mod =>
            if !is_integer(&left_type) || !is_integer(&right_type) {
                err(b.span.start, ErrorCode::TypeError, format!("Operator {} expects two integer expressions as operands", b.operator))
            } else {
                Ok(Type::Int)
            },
        Operator::Equals | Operator::NotEquals =>
            if left_type != right_type {
                err(b.span.start, ErrorCode::TypeError, format!("Operator {} expects two expressions of the same type as operands", b.operator))
            } else {
                Ok(Type::Bool)
            },

        Operator::And | Operator::Or =>
            if !is_bool(&left_type) || !is_bool(&right_type) {
                err(b.span.start, ErrorCode::TypeError, format!("Operator {} expects two boolean expressions as operands", b.operator))
            } else {
                Ok(Type::Bool)
            },
        Operator::Concat => {
            if left_type.concat_allowed(&right_type) {
                Ok(right_type)
            } else {
                err(b.span.start, ErrorCode::TypeError,
                    format!("Operator {} expects two arrays with the same element type, or an expression and an array, where the expression has the same element type as the array", b.operator))
            }
        },
        _ => err(b.span.start, ErrorCode::InvalidBinaryOperator, format!("Operator {} is not a binary operator", b.operator))
    }
}

fn infer_and_check_array_literal(ctx: &mut TypeCheckerContext, a: &mut ArrayLiteral) -> CompileResult<Type>
{
    if a.elements.is_empty() {
        a.array_type = array_type(Type::Unknown, 0);
        return Ok(a.array_type.clone());
    }

    let mut array_element_type = Type::Unknown;
    for e in a.elements.iter_mut() {
        let t = try!(infer_and_check_expression(ctx, e, None));
        if array_element_type == Type::Unknown {
            array_element_type = t;
        } else if array_element_type != t {
            return err(e.span().start, ErrorCode::TypeError, format!("Array elements must have the same type"));
        }
    }

    let array_type = array_type(array_element_type, a.elements.len());
    if a.array_type == Type::Unknown {
        a.array_type = array_type;
    } else if a.array_type != array_type {
        return err(a.span.start, ErrorCode::TypeError, format!("Array has type {}, but elements have type {}", a.array_type, array_type))
    }

    Ok(a.array_type.clone())
}

fn infer_and_check_array_generator(ctx: &mut TypeCheckerContext, a: &mut ArrayGenerator) -> CompileResult<Type>
{
    ctx.push_stack();

    // At the moment assume iterable is an array, in the future expand to all iterators
    let it_type = try!(infer_and_check_expression(ctx, &mut a.iterable, None));
    let it_element_type = match it_type.get_element_type()
    {
        Some(Type::Unknown) => return err(a.span.start, ErrorCode::TypeError, format!("Extract expression with empty array is pointless")),
        Some(et) => et,
        None => return err(a.span.start, ErrorCode::TypeError, format!("Iterable expression in array generator is not an array")),
    };

    try!(ctx.add_variable(&a.var, it_element_type, a.span.start));

    let element_type = try!(infer_and_check_expression(ctx, &mut a.left, None));
    a.array_type = slice_type(element_type);
    ctx.pop_stack();
    Ok(a.array_type.clone())
}

fn infer_and_check_call(ctx: &mut TypeCheckerContext, c: &mut Call) -> CompileResult<Type>
{
    let func_type = try!(ctx.resolve_type(&c.callee.name).ok_or(unknown_name(c.span.start, &c.callee.name)));

    if let Type::Func(arg_types, ret) = func_type
    {
        for (idx, (arg, expected_arg_type)) in c.args.iter_mut().zip(arg_types).enumerate()
        {
            let arg_type = try!(infer_and_check_expression(ctx, arg, Some(expected_arg_type.clone())));
            if expected_arg_type.is_generic()
            {
                if let Some(prev_arg_type) = c.generic_args.insert(expected_arg_type.clone(), arg_type.clone()) {
                    if prev_arg_type != arg_type {
                        return err(c.span.start, ErrorCode::GenericTypeSubstitutionError,
                            format!("Generic argument {} mismatch, expecting type {}, not {}", expected_arg_type, prev_arg_type, arg_type));
                    }
                }
            }
            else if arg_type == expected_arg_type
            {
                continue
            }
            else
            {
                if let Some(conversion_expr) = expected_arg_type.convert(&arg_type, &arg)
                {
                    *arg = conversion_expr;
                }
                else
                {
                    return err(arg.span().start, ErrorCode::TypeError,
                        format!("Argument {} has the wrong type, function {} expects the type {}, argument provided has type {}",
                            idx, c.callee.name, expected_arg_type, arg_type))
                }
            }
        }

        if ret.is_generic() {
            if let Some(resolved_ret) = c.generic_args.get(ret.deref()) {
                Ok(resolved_ret.clone())
            } else {
                err(c.span.start, ErrorCode::GenericTypeSubstitutionError,
                    format!("Cannot determine the type of the generic return type {}", ret))
            }
        } else {
            Ok(ret.deref().clone())
        }
    }
    else
    {
        err(c.span.start, ErrorCode::CallingNonCallable, format!("{} is not callable", c.callee.name))
    }
}


fn infer_and_check_function(ctx: &mut TypeCheckerContext, fun: &mut Function) -> CompileResult<Type>
{
    if fun.is_generic() { // Generics have to be instantiated at call time
        return Ok(Type::Unknown);
    }

    ctx.push_stack();
    for arg in fun.sig.args.iter_mut()
    {
        if arg.typ.is_sequence() || arg.typ.is_function() {
            arg.passing_mode = ArgumentPassingMode::ByPtr;
        }
        try!(ctx.add_variable(&arg.name, arg.typ.clone(), arg.span.start));
    }

    let et = try!(infer_and_check_expression(ctx, &mut fun.expression, None));
    ctx.pop_stack();
    if et != fun.sig.return_type {
        return err(fun.span.start, ErrorCode::TypeError, format!("Function {} has return type {}, but it is returning an expression of type {}",
            fun.sig.name, fun.sig.return_type, et));
    }

    fun.type_checked = true;
    Ok(fun.get_type())
}

fn infer_and_check_match(ctx: &mut TypeCheckerContext, m: &mut MatchExpression) -> CompileResult<Type>
{
    let target_type = try!(infer_and_check_expression(ctx, &mut m.target, None));
    let mut return_type = Type::Unknown;
    for c in &mut m.cases
    {
        let infer_case_type = |ctx: &mut TypeCheckerContext, e: &mut Expression, return_type: &Type| {
            let tt = try!(infer_and_check_expression(ctx, e, None));
            if *return_type != Type::Unknown && *return_type != tt {
                return err(e.span().start, ErrorCode::TypeError, format!("Expressions in match statements must return the same type"));
            } else {
                Ok(tt)
            }
        };

        match c.match_expr
        {
            Expression::ArrayPattern(ref ap) => {
                if !target_type.is_sequence() {
                    return err(ap.span.start, ErrorCode::TypeError, format!("Attempting to pattern match an expression of type {}, with an array", target_type));
                }

                let element_type = target_type.get_element_type().expect("target_type is not an array type");

                ctx.push_stack();
                try!(ctx.add_variable(&ap.head, element_type.clone(), ap.span.start));
                try!(ctx.add_variable(&ap.tail, slice_type(element_type.clone()), ap.span.start));
                return_type = try!(infer_case_type(ctx, &mut c.to_execute, &return_type));
                ctx.pop_stack();
            },

            Expression::NameRef(ref nr) => {
                if nr.name != "_" {
                    return err(c.match_expr.span().start, ErrorCode::TypeError, format!("Invalid pattern match"));
                }

                return_type = try!(infer_case_type(ctx, &mut c.to_execute, &return_type));
            },

            Expression::ArrayLiteral(_) |
            Expression::IntLiteral(_, _) |
            Expression::BoolLiteral(_, _) |
            Expression::FloatLiteral(_, _) |
            Expression::StringLiteral(_, _) => {
                let m_type = try!(infer_and_check_expression(ctx, &mut c.match_expr, None));
                if !target_type.is_matchable(&m_type) {
                    return err(c.match_expr.span().start, ErrorCode::TypeError, format!("Pattern match of type {}, cannot match with an expression of type {}",
                        m_type, target_type));
                }

                return_type = try!(infer_case_type(ctx, &mut c.to_execute, &return_type));
            },

            _ => {
                return err(c.match_expr.span().start, ErrorCode::TypeError, format!("Invalid pattern match"));
            }
        }
    }

    if return_type == Type::Unknown {
        return err(m.span.start, ErrorCode::TypeError, format!("Cannot infer type of match expression"));
    }

    m.typ = return_type.clone();
    Ok(return_type)
}

fn infer_and_check_lambda_body(ctx: &mut TypeCheckerContext, m: &mut Lambda) -> CompileResult<Type>
{
    ctx.push_stack();
    for arg in &mut m.sig.args {
        if arg.typ.is_sequence() || arg.typ.is_function() {
            arg.passing_mode = ArgumentPassingMode::ByPtr;
        }
        try!(ctx.add_variable(&arg.name, arg.typ.clone(), arg.span.start));
    }

    let return_type = try!(infer_and_check_expression(ctx, m.expr.deref_mut(), None));
    ctx.pop_stack();
    m.set_return_type(return_type);
    Ok(m.sig.get_type())
}

fn infer_and_check_lambda(ctx: &mut TypeCheckerContext, m: &mut Lambda, type_hint: Option<Type>) -> CompileResult<Type>
{
    match type_hint
    {
        Some(typ) => {
            use uuid::{Uuid};
            m.sig.name = format!("lambda-{}", Uuid::new_v4()); // Add a uuid, so we don't get name clashes
            try!(m.apply_type(&typ));
            let infered_type = try!(infer_and_check_lambda_body(ctx, m));
            if infered_type != typ {
                return err(m.span.start, ErrorCode::TypeError, format!("Lambda body has the wrong type, expecting {}, got {}", typ, infered_type));
            }

            Ok(infered_type)
        },
        None => {
            if m.is_generic() {
                return Ok(Type::Unknown);
            }

            infer_and_check_lambda_body(ctx, m)
        },
    }
}

fn infer_and_check_name(ctx: &mut TypeCheckerContext, nr: &mut NameRef) -> CompileResult<Type>
{
    nr.typ = try!(ctx.resolve_type(&nr.name).ok_or(unknown_name(nr.span.start, &nr.name)));
    Ok(nr.typ.clone())
}

fn infer_and_check_let(ctx: &mut TypeCheckerContext, l: &mut LetExpression) -> CompileResult<Type>
{
    ctx.push_stack();
    for b in &mut l.bindings
    {
        b.typ = try!(infer_and_check_expression(ctx, &mut b.init, None));
        try!(ctx.add_variable(&b.name, b.typ.clone(), b.span.start));
    }

    l.typ = try!(infer_and_check_expression(ctx, &mut l.expression, None));
    ctx.pop_stack();
    Ok(l.typ.clone())
}

pub fn infer_and_check_expression(ctx: &mut TypeCheckerContext, e: &mut Expression, type_hint: Option<Type>) -> CompileResult<Type>
{
    match *e
    {
        Expression::UnaryOp(ref mut op) => infer_and_check_unary_op(ctx, op),
        Expression::BinaryOp(ref mut op) => infer_and_check_binary_op(ctx, op),
        Expression::ArrayLiteral(ref mut a) => infer_and_check_array_literal(ctx, a),
        Expression::ArrayPattern(_) => Ok(Type::Unknown), // Doesn't really have a type
        Expression::ArrayGenerator(ref mut a) => infer_and_check_array_generator(ctx, a),
        Expression::Call(ref mut c) => infer_and_check_call(ctx, c),
        Expression::NameRef(ref mut nr) => infer_and_check_name(ctx, nr),
        Expression::Function(ref mut f) => {
            ctx.add_function(&f.sig.name, f.get_type());
            infer_and_check_function(ctx, f)
        },
        Expression::Match(ref mut m) => infer_and_check_match(ctx, m),
        Expression::Lambda(ref mut l) => infer_and_check_lambda(ctx, l, type_hint),
        Expression::Let(ref mut l) => infer_and_check_let(ctx, l),
        Expression::Enclosed(_, ref mut inner) => infer_and_check_expression(ctx, inner, type_hint),
        Expression::IntLiteral(_, _) => Ok(Type::Int),
        Expression::FloatLiteral(_, _) => Ok(Type::Float),
        Expression::StringLiteral(_, _)  => Ok(Type::String),
        Expression::BoolLiteral(_, _) => Ok(Type::Bool),
        Expression::ArrayToSliceConversion(ref mut inner) => infer_and_check_expression(ctx, inner, type_hint),
    }
}


/*
    Type check and infer all the unkown types
*/
pub fn infer_and_check_types(module: &mut Module) -> CompileResult<()>
{
    loop {
        let mut ctx = TypeCheckerContext::new();
        for (_, ref f) in module.functions.iter() {
            ctx.add_function(&f.sig.name, f.get_type());
        }

        for (_, ref mut f) in module.functions.iter_mut() {
            if !f.type_checked {
                try!(infer_and_check_function(&mut ctx, f));
            }
        }

        let count = module.functions.len();
        try!(instantiate_generics(module));
        // As long as we are adding new generic functions, we need to type check the module again
        if count == module.functions.len() {
            break;
        }
    }

/*
    use ast::TreePrinter;
    module.print(0);
*/
    Ok(())
}
