use std::collections::{HashMap};
use std::ops::{DerefMut};
use ast::{Module, Expression, NameRef, UnaryOp, BinaryOp, ArrayLiteral, ArrayGenerator,
    MatchExpression, Function, Lambda, Call, Type, LetExpression, ArgumentPassingMode,
    StructInitializer, StructMemberAccess, StructMember, StructPattern, func_type, array_type, slice_type};
use compileerror::{CompileResult, CompileError, Pos, ErrorCode, err, unknown_name};
use parser::{Operator};
use passes::{instantiate_generics, fill_in_generics, substitute_types, resolve_types};


pub struct StackFrame
{
    symbols: HashMap<String, Type>,
}

impl StackFrame
{
    pub fn new() -> StackFrame
    {
        StackFrame{
            symbols: HashMap::new(),
        }
    }

    pub fn resolve_type(&self, name: &str) -> Option<Type>
    {
        self.symbols.get(name).map(|t| t.clone())
    }

    pub fn add(&mut self, name: &str, t: Type, pos: Pos) -> CompileResult<()>
    {
        if self.symbols.insert(name.into(), t).is_some() {
            err(pos, ErrorCode::RedefinitionOfVariable, format!("Symbol {} has already been defined", name))
        } else {
            Ok(())
        }
    }

    pub fn update(&mut self, name: &str, t: Type)
    {
        self.symbols.insert(name.into(), t);
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

    pub fn add(&mut self, name: &str, t: Type, pos: Pos) -> CompileResult<()>
    {
        self.stack.last_mut().expect("Empty stack").add(name, t, pos)
    }

    pub fn update(&mut self, name: &str, t: Type)
    {
        self.stack.last_mut().expect("Empty stack").update(name, t)
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

fn type_check_unary_op(ctx: &mut TypeCheckerContext, u: &mut UnaryOp) -> CompileResult<Type>
{
    let e_type = try!(type_check_expression(ctx, &mut u.expression, None));
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

fn type_check_binary_op(ctx: &mut TypeCheckerContext, b: &mut BinaryOp) -> CompileResult<Type>
{
    let left_type = try!(type_check_expression(ctx, &mut b.left, None));
    let right_type = try!(type_check_expression(ctx, &mut b.right, None));
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

fn type_check_array_literal(ctx: &mut TypeCheckerContext, a: &mut ArrayLiteral) -> CompileResult<Type>
{
    if a.elements.is_empty() {
        a.array_type = array_type(Type::Unknown, 0);
        return Ok(a.array_type.clone());
    }

    let mut array_element_type = Type::Unknown;
    for e in a.elements.iter_mut() {
        let t = try!(type_check_expression(ctx, e, None));
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

fn type_check_array_generator(ctx: &mut TypeCheckerContext, a: &mut ArrayGenerator) -> CompileResult<Type>
{
    ctx.push_stack();

    // At the moment assume iterable is an array, in the future expand to all iterators
    let it_type = try!(type_check_expression(ctx, &mut a.iterable, None));
    let it_element_type = match it_type.get_element_type()
    {
        Some(Type::Unknown) => return err(a.span.start, ErrorCode::TypeError, format!("Extract expression with empty array is pointless")),
        Some(et) => et,
        None => return err(a.span.start, ErrorCode::TypeError, format!("Iterable expression in array generator is not an array")),
    };

    try!(ctx.add(&a.var, it_element_type, a.span.start));

    let element_type = try!(type_check_expression(ctx, &mut a.left, None));
    a.array_type = slice_type(element_type);
    ctx.pop_stack();
    Ok(a.array_type.clone())
}

fn type_check_call(ctx: &mut TypeCheckerContext, c: &mut Call) -> CompileResult<Type>
{
    let func_type = try!(ctx.resolve_type(&c.callee.name).ok_or(unknown_name(c.span.start, &c.callee.name)));

    if let Type::Func(ref ft) = func_type
    {
        for (idx, (arg, expected_arg_type)) in c.args.iter_mut().zip(ft.args.iter()).enumerate()
        {
            let expected_arg_type = substitute_types(&expected_arg_type, &c.generic_args);
            let arg_type = try!(type_check_expression(ctx, arg, Some(expected_arg_type.clone())));
            let real_expected_arg_type = if expected_arg_type.is_generic() {
                try!(fill_in_generics(&arg_type, &expected_arg_type, &mut c.generic_args, arg.span().start))
            } else {
                expected_arg_type
            };

            if arg_type == real_expected_arg_type
            {
                continue
            }
            else
            {
                if let Some(conversion_expr) = real_expected_arg_type.convert(&arg_type, &arg)
                {
                    *arg = conversion_expr;
                }
                else
                {
                    return err(arg.span().start, ErrorCode::TypeError,
                        format!("Argument {} has the wrong type, function {} expects the type {}, argument provided has type {}",
                            idx, c.callee.name, real_expected_arg_type, arg_type))
                }
            }
        }

        if ft.return_type.is_generic() {
            if let Some(resolved_ret) = c.generic_args.get(&ft.return_type) {
                Ok(resolved_ret.clone())
            } else {
                err(c.span.start, ErrorCode::GenericTypeSubstitutionError,
                    format!("Cannot determine the type of the generic return type {}", ft.return_type))
            }
        } else {
            Ok(ft.return_type.clone())
        }
    }
    else
    {
        err(c.span.start, ErrorCode::CallingNonCallable, format!("{} is not callable", c.callee.name))
    }
}


fn type_check_function(ctx: &mut TypeCheckerContext, fun: &mut Function) -> CompileResult<Type>
{
    if fun.is_generic() { // Generics have to be instantiated at call time
        return Ok(Type::Unknown);
    }

    ctx.push_stack();
    for arg in fun.sig.args.iter_mut()
    {
        if arg.typ.pass_by_ptr() {
            arg.passing_mode = ArgumentPassingMode::ByPtr;
        }
        try!(ctx.add(&arg.name, arg.typ.clone(), arg.span.start));
    }

    let et = try!(type_check_expression(ctx, &mut fun.expression, None));
    ctx.pop_stack();
    if et != fun.sig.return_type {
        return err(fun.span.start, ErrorCode::TypeError, format!("Function {} has return type {}, but it is returning an expression of type {}",
            fun.sig.name, fun.sig.return_type, et));
    }

    fun.type_checked = true;
    Ok(fun.sig.typ.clone())
}

fn type_check_match(ctx: &mut TypeCheckerContext, m: &mut MatchExpression) -> CompileResult<Type>
{
    let target_type = try!(type_check_expression(ctx, &mut m.target, None));
    let mut return_type = Type::Unknown;

    for c in &mut m.cases
    {
        let infer_case_type = |ctx: &mut TypeCheckerContext, e: &mut Expression, return_type: &Type| {
            let tt = try!(type_check_expression(ctx, e, None));
            if *return_type != Type::Unknown && *return_type != tt {
                return err(e.span().start, ErrorCode::TypeError, format!("Expressions in match statements must return the same type"));
            } else {
                Ok(tt)
            }
        };

        let case_type = match c.match_expr
        {
            Expression::ArrayPattern(ref ap) => {
                if !target_type.is_sequence() {
                    return err(ap.span.start, ErrorCode::TypeError, format!("Attempting to pattern match an expression of type {}, with an array", target_type));
                }

                let element_type = target_type.get_element_type().expect("target_type is not an array type");

                ctx.push_stack();
                try!(ctx.add(&ap.head, element_type.clone(), ap.span.start));
                try!(ctx.add(&ap.tail, slice_type(element_type.clone()), ap.span.start));
                let ct = try!(infer_case_type(ctx, &mut c.to_execute, &return_type));
                ctx.pop_stack();
                ct
            },

            Expression::NameRef(ref nr) => {
                if let Some(Type::Sum(ref st)) = ctx.resolve_type(&nr.name)
                {
                    let ref case = st.cases[st.index.expect("Sum type index must be known here")];
                    if case.typ == Type::Int {
                        try!(infer_case_type(ctx, &mut c.to_execute, &return_type))
                    } else {
                        return err(c.match_expr.span().start, ErrorCode::TypeError, format!("Invalid pattern match, match should be with an empty sum case"));
                    }
                }
                else if nr.name != "_"
                {
                    return err(c.match_expr.span().start, ErrorCode::TypeError, format!("Invalid pattern match"));
                }
                else
                {
                    try!(infer_case_type(ctx, &mut c.to_execute, &return_type))
                }
            },

            Expression::ArrayLiteral(_) |
            Expression::IntLiteral(_, _) |
            Expression::BoolLiteral(_, _) |
            Expression::FloatLiteral(_, _) |
            Expression::StringLiteral(_, _) => {
                let m_type = try!(type_check_expression(ctx, &mut c.match_expr, None));
                if !target_type.is_matchable(&m_type) {
                    return err(c.match_expr.span().start, ErrorCode::TypeError, format!("Pattern match of type {}, cannot match with an expression of type {}",
                        m_type, target_type));
                }

                try!(infer_case_type(ctx, &mut c.to_execute, &return_type))
            },

            Expression::StructPattern(ref mut p) => {
                try!(type_check_struct_pattern(ctx, p));
                ctx.push_stack();

                for (binding, typ) in p.bindings.iter().zip(p.types.iter()) {
                    if binding != "_" {
                        println!("Add binding {} of {}", binding, typ);
                        try!(ctx.add(binding, typ.clone(), p.span.start));
                    }
                }

                let ct = try!(infer_case_type(ctx, &mut c.to_execute, &return_type));
                ctx.pop_stack();
                ct
            },

            _ => {
                return err(c.match_expr.span().start, ErrorCode::TypeError, format!("Invalid pattern match"));
            }
        };

        if return_type == Type::Unknown {
            return_type = case_type;
        } else if return_type != case_type {
            return err(m.span.start, ErrorCode::TypeError, format!("Cases of match statements must return the same type"));
        }
    }

    m.typ = return_type.clone();
    Ok(return_type)
}

fn type_check_lambda_body(ctx: &mut TypeCheckerContext, m: &mut Lambda) -> CompileResult<Type>
{
    ctx.push_stack();
    for arg in &mut m.sig.args {
        if arg.typ.pass_by_ptr() {
            arg.passing_mode = ArgumentPassingMode::ByPtr;
        }
        try!(ctx.add(&arg.name, arg.typ.clone(), arg.span.start));
    }

    let return_type = try!(type_check_expression(ctx, m.expr.deref_mut(), None));
    ctx.pop_stack();
    m.set_return_type(return_type);
    Ok(m.sig.typ.clone())
}

fn type_check_lambda(ctx: &mut TypeCheckerContext, m: &mut Lambda, type_hint: Option<Type>) -> CompileResult<Type>
{
    match type_hint
    {
        Some(typ) => {
            use uuid::{Uuid};
            m.sig.name = format!("lambda-{}", Uuid::new_v4()); // Add a uuid, so we don't get name clashes
            try!(m.apply_type(&typ));
            let infered_type = try!(type_check_lambda_body(ctx, m));
            if infered_type != typ {
                return err(m.span.start, ErrorCode::TypeError, format!("Lambda body has the wrong type, expecting {}, got {}", typ, infered_type));
            }

            Ok(infered_type)
        },
        None => {
            if m.is_generic() {
                return Ok(Type::Unknown);
            }

            type_check_lambda_body(ctx, m)
        },
    }
}

fn type_check_name(ctx: &mut TypeCheckerContext, nr: &mut NameRef, type_hint: Option<Type>) -> CompileResult<Type>
{
    nr.typ = try!(ctx.resolve_type(&nr.name).ok_or(unknown_name(nr.span.start, &nr.name)));
    if nr.typ == Type::Unknown && type_hint.is_some() {
        err(nr.span.start, ErrorCode::UnknownType(nr.name.clone(), type_hint.unwrap()), format!("{} has unknown type", nr.name))
    } else {
        Ok(nr.typ.clone())
    }
}

fn type_check_let(ctx: &mut TypeCheckerContext, l: &mut LetExpression) -> CompileResult<Type>
{
    ctx.push_stack();
    for b in &mut l.bindings
    {
        b.typ = try!(type_check_expression(ctx, &mut b.init, None));
        try!(ctx.add(&b.name, b.typ.clone(), b.span.start));
    }

    match type_check_expression(ctx, &mut l.expression, None)
    {
        Err(ref cr) => {
            if let ErrorCode::UnknownType(ref name, ref expected_type) = cr.error {
                let mut handled = false;
                for b in &mut l.bindings
                {
                    if b.name == *name
                    {
                        // It's one we know, so lets try again with a proper type hint
                        b.typ = try!(type_check_expression(ctx, &mut b.init, Some(expected_type.clone())));
                        ctx.update(&b.name, b.typ.clone());
                        l.typ = try!(type_check_expression(ctx, &mut l.expression, None));
                        handled = true;
                        break;
                    }
                }

                if !handled {
                    return Err(cr.clone());
                }
            } else {
                return Err(cr.clone());
            }
        },
        Ok(typ) => {
            l.typ = typ;
        }
    }

    ctx.pop_stack();
    Ok(l.typ.clone())
}

fn type_check_struct_members_in_initializer(ctx: &mut TypeCheckerContext, members: &Vec<StructMember>, si: &mut StructInitializer) -> CompileResult<()>
{
    if members.len() != si.member_initializers.len() {
        return err(si.span.start, ErrorCode::WrongArgumentCount,
            format!("Type {} has {} members, but attempting to initialize {} members", si.struct_name, members.len(), si.member_initializers.len()));
    }

    for (idx, (member, mi)) in members.iter().zip(si.member_initializers.iter_mut()).enumerate()
    {
        let t = try!(type_check_expression(ctx, mi, Some(member.typ.clone())));
        if t != member.typ
        {
            return err(mi.span().start, ErrorCode::TypeError,
                format!("Attempting to initialize member {} with type '{}', expecting an expression of type '{}'",
                    idx, t, member.typ));
        }
    }

    Ok(())
}

fn type_check_struct_initializer(ctx: &mut TypeCheckerContext, si: &mut StructInitializer) -> CompileResult<Type>
{
    let st = try!(ctx.resolve_type(&si.struct_name).ok_or(unknown_name(si.span.start, &si.struct_name)));
    match st
    {
        Type::Struct(st) => {
            try!(type_check_struct_members_in_initializer(ctx, &st.members, si));
            si.typ = Type::Struct(st);
            Ok(si.typ.clone())
        },
        Type::Sum(st) => {
            let idx = if let Some(idx) = st.index {
                idx
            } else {
                return err(si.span.start, ErrorCode::TypeError, format!("Cannot determine Sum type case"))
            };

            match st.cases[idx].typ
            {
                Type::Struct(ref s) => try!(type_check_struct_members_in_initializer(ctx, &s.members, si)),
                Type::Int => {},
                _ => return err(si.span.start, ErrorCode::TypeError, format!("Invalid sum type case")),
            }

            si.typ = Type::Sum(st);
            Ok(si.typ.clone())
        },
        _ => err(si.span.start, ErrorCode::TypeError, format!("{} is not a struct", si.struct_name)),
    }
}



fn find_member_type(members: &Vec<StructMember>, member_name: &str, pos: Pos) -> CompileResult<(usize, Type)>
{
    members.iter()
        .enumerate()
        .find(|&(_, m)| m.name == member_name)
        .map(|(idx, m)| (idx, m.typ.clone()))
        .ok_or(CompileError::new(pos, ErrorCode::UnknownStructMember, format!("Unknown struct member {}", member_name)))
}

fn type_check_struct_member_access(ctx: &mut TypeCheckerContext, sma: &mut StructMemberAccess) -> CompileResult<Type>
{
    let mut st = try!(ctx.resolve_type(&sma.name).ok_or(unknown_name(sma.span.start, &sma.name)));
    for ref member_name in &sma.members
    {
        st = match st
        {
            Type::Struct(ref st) => {
                let (member_idx, member_type) = try!(find_member_type(&st.members, &member_name, sma.span.start));
                sma.indices.push(member_idx);
                member_type
            },
            _ => {
                return err(sma.span.start, ErrorCode::TypeError, format!("Type '{}' is not a struct, so you cannot access it's members", st));
            },
        };
    }

    Ok(st)
}

fn type_check_struct_pattern(ctx: &mut TypeCheckerContext, p: &mut StructPattern) -> CompileResult<Type>
{
    let typ = try!(ctx.resolve_type(&p.name).ok_or(unknown_name(p.span.start, &p.name)));
    match typ
    {
        Type::Sum(ref st) => {
            if let Some(idx) = st.index
            {
                let ref case = st.cases[idx];
                match case.typ
                {
                    Type::Struct(ref s) => {
                        if s.members.len() != p.bindings.len() {
                            err(p.span.start, ErrorCode::TypeError, format!("Not enough bindings in pattern match"))
                        } else {
                            p.types = s.members.iter().map(|sm| sm.typ.clone()).collect();
                            Ok(Type::Unknown)
                        }
                    },
                    _ => err(p.span.start, ErrorCode::TypeError, format!("Attempting to pattern match a normal sum type case with a struct")),
                }
            }
            else
            {
                err(p.span.start, ErrorCode::TypeError, format!("Cannot determine the case of a sum type"))
            }
        },

        Type::Struct(ref st) => {
            p.types = st.members.iter().map(|sm| sm.typ.clone()).collect();
            Ok(Type::Unknown)
        },
        _ => err(p.span.start, ErrorCode::TypeError, format!("Struct pattern is only allowed for structs and sum types containing structs"))
    }
}

pub fn type_check_expression(ctx: &mut TypeCheckerContext, e: &mut Expression, type_hint: Option<Type>) -> CompileResult<Type>
{
    match *e
    {
        Expression::UnaryOp(ref mut op) => type_check_unary_op(ctx, op),
        Expression::BinaryOp(ref mut op) => type_check_binary_op(ctx, op),
        Expression::ArrayLiteral(ref mut a) => type_check_array_literal(ctx, a),
        Expression::ArrayPattern(_) => Ok(Type::Unknown), // Doesn't really have a type
        Expression::StructPattern(ref mut p) => type_check_struct_pattern(ctx, p),
        Expression::ArrayGenerator(ref mut a) => type_check_array_generator(ctx, a),
        Expression::Call(ref mut c) => type_check_call(ctx, c),
        Expression::NameRef(ref mut nr) => type_check_name(ctx, nr, type_hint),
        Expression::Match(ref mut m) => type_check_match(ctx, m),
        Expression::Lambda(ref mut l) => type_check_lambda(ctx, l, type_hint),
        Expression::Let(ref mut l) => type_check_let(ctx, l),
        Expression::Enclosed(_, ref mut inner) => type_check_expression(ctx, inner, type_hint),
        Expression::IntLiteral(_, _) => Ok(Type::Int),
        Expression::FloatLiteral(_, _) => Ok(Type::Float),
        Expression::StringLiteral(_, _)  => Ok(Type::String),
        Expression::BoolLiteral(_, _) => Ok(Type::Bool),
        Expression::ArrayToSliceConversion(ref mut inner) => type_check_expression(ctx, inner, type_hint),
        Expression::StructInitializer(ref mut si) => type_check_struct_initializer(ctx, si),
        Expression::StructMemberAccess(ref mut sma) => type_check_struct_member_access(ctx, sma),
    }
}


/*
    Type check and infer all the unkown types
*/
pub fn type_check_module(module: &mut Module) -> CompileResult<()>
{
    loop {
        let mut ctx = TypeCheckerContext::new();
        try!(resolve_types(&mut ctx, module));

        // Register all the generic functions
        for f in module.functions.values() {
            if f.is_generic() {
                try!(ctx.add(&f.sig.name, f.sig.get_type(), f.span.start));
            }
        }

        for ref mut f in module.functions.values_mut() {
            if !f.type_checked {
                try!(type_check_function(&mut ctx, f));
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
