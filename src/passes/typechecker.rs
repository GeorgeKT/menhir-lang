use std::collections::HashMap;
use std::ops::Deref;
use ast::{Module, Expression, NameRef, UnaryOp, BinaryOp, ArrayLiteral, ArrayInitializer, ArrayPattern,
    MatchExpression, Function, Lambda, Call, Type, func_type};
use compileerror::{CompileResult, Pos, CompileError, ErrorCode, err};
use parser::{Operator};


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

    pub fn add_variable(&mut self, name: &str, t: Type)
    {
        self.variables.insert(name.into(), t);
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

    pub fn add_variable(&mut self, name: &str, t: Type)
    {
        self.stack.last_mut().expect("Empty stack").add_variable(name, t)
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

/*
fn infer(ctx: &TypeCheckerContext, e: &Expression) -> CompileResult<Type>
{
    match *e
    {
        Expression::IntLiteral(_, _) => Ok(Type::Int),
        Expression::FloatLiteral(_, _) => Ok(Type::Float),
        Expression::StringLiteral(_, _) => Ok(Type::String),
        Expression::Enclosed(_, ref e) => infer(ctx, &e),

        Expression::ArrayLiteral(ref a) => {
            if a.elements.is_empty() {
                Ok(Type::Array(Box::new(Type::Unknown)))
            } else {
                let et = try!(infer(ctx, &a.elements[0]));
                Ok(Type::Array(Box::new(et)))
            }
        },

        Expression::ArrayInitializer(ref a) => {
            let et = try!(infer(ctx, &a.init));
            Ok(Type::Array(Box::new(et)))
        },

        Expression::ArrayPattern(ref a) => {
            err(a.span.start, ErrorCode::TypeError, "Cannot infer type of a pattern".into())
        },

        Expression::UnaryOp(ref u) => {
            match u.operator
            {
                Operator::Not => Ok(Type::Bool),
                Operator::Sub => infer(ctx, &u.expression),
                _ => invalid_unary_operator(u.span.start, u.operator),
            }
        },

        Expression::BinaryOp(ref b) => {
            match b.operator 
            {
                Operator::LessThan | Operator::GreaterThan | Operator::GreaterThanEquals | Operator::LessThanEquals |
                Operator::Equals | Operator::NotEquals | Operator::Not | Operator::And | Operator::Or => Ok(Type::Bool),
                Operator::Add | Operator::Sub | Operator::Mul | Operator::Div => {
                    let left = try!(infer(ctx, &b.left));
                    let right = try!(infer(ctx, &b.right));
                    match (left, right)
                    {
                        (Type::Int, Type::Int) => Ok(Type::Int),
                        (Type::Float, Type::Float) | 
                        (Type::Int, Type::Float) |
                        (Type::Float, Type::Int) => Ok(Type::Float), 
                        _ => expected_numeric_operands(b.span.start, b.operator),
                    }
                },
                Operator::Mod => Ok(Type::Int),
                Operator::Concat => infer(ctx, &b.left),
            }
        },

        Expression::Call(ref c) => {
           
        },

        Expression::NameRef(ref nr) => {
            ctx.resolve_type(&nr.name)
                .ok_or(unknown_name(nr.span.start, &nr.name))
        },

        Expression::Function(ref fun) => {
            Ok(func_type(fun.sig.args.iter().map(|a| a.typ.clone()).collect(), fun.sig.return_type.clone()))
        },

        Expression::Match(ref m) => {
            m.cases
                .first()
                .ok_or(CompileError::new(m.span.start, ErrorCode::EmptyMatch, "empty match".into())) 
                .and_then(|c| infer(ctx, &c.to_execute))
        },

        Expression::Lambda(ref l) => {
            infer(ctx, &l.expr)
        },
    }
}
*/

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
    let e_type = try!(infer_and_check_expression(ctx, &mut u.expression));
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
    let left_type = try!(infer_and_check_expression(ctx, &mut b.right));
    let right_type = try!(infer_and_check_expression(ctx, &mut b.left));
    match b.operator
    {
        Operator::Add |
        Operator::Sub |
        Operator::Mul |
        Operator::Div |
        Operator::LessThan |
        Operator::GreaterThan |
        Operator::LessThanEquals |
        Operator::GreaterThanEquals =>
            if !is_numeric(&left_type) || !is_numeric(&right_type) {
                expected_numeric_operands(b.span.start, b.operator)
            } else if left_type != right_type {
                err(b.span.start, ErrorCode::TypeError, format!("Operator {} expects operands of the same type", b.operator))
            } else {
                Ok(left_type)
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
            if left_type.concattable(&right_type) {
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
    let mut array_element_type = Type::Unknown;
    for e in a.elements.iter_mut() {
        let t = try!(infer_and_check_expression(ctx, e));
        if array_element_type == Type::Unknown {
            array_element_type = t;
        } else if array_element_type != t {
            return err(e.span().start, ErrorCode::TypeError, format!("Array elements must have the same type"));
        }
    }

    let array_type = Type::Array(Box::new(array_element_type));
    if a.array_type == Type::Unknown {
        a.array_type = array_type;
    } else if a.array_type != array_type {
        return err(a.span.start, ErrorCode::TypeError, format!("Array has type {}, but elements have type {}", a.array_type, array_type))
    }

    Ok(a.array_type.clone())
}

fn infer_and_check_array_initializer(ctx: &mut TypeCheckerContext, a: &mut ArrayInitializer) -> CompileResult<Type>
{
    let array_element_type = try!(infer_and_check_expression(ctx, &mut a.init));
    let array_type = Type::Array(Box::new(array_element_type));
    if a.array_type == Type::Unknown {
        a.array_type = array_type;
    } else if a.array_type != array_type {
        return err(a.span.start, ErrorCode::TypeError, format!("Array has type {}, but elements have type {}", a.array_type, array_type))
    }

    Ok(a.array_type.clone())
}

fn infer_and_check_call(ctx: &mut TypeCheckerContext, c: &mut Call) -> CompileResult<Type>
{
    let func_type = try!(ctx.resolve_type(&c.callee.name).ok_or(unknown_name(c.span.start, &c.callee.name)));
    
    if let Type::Func(arg_types, ret) = func_type 
    {
        for (idx, (arg, expected_arg_type)) in c.args.iter_mut().zip(arg_types).enumerate()
        {
            let arg_type = try!(infer_and_check_expression(ctx, arg));
            if arg_type != expected_arg_type {
                return err(arg.span().start, ErrorCode::TypeError, 
                    format!("Argument {} has the wrong type, function {} expects the type {}, argument provided has type {}", 
                        idx, c.callee.name, expected_arg_type, arg_type))
            }
        }
        Ok(ret.deref().clone())
    } 
    else 
    {
        err(c.span.start, ErrorCode::CallingNonCallable, format!("{} is not callable", c.callee.name))
    }
}

fn infer_and_check_function(ctx: &mut TypeCheckerContext, f: &Function) -> CompileResult<Type>
{
err(f.span.start, ErrorCode::TypeError, format!("NYI"))
}

fn infer_and_check_match(ctx: &mut TypeCheckerContext, m: &MatchExpression) -> CompileResult<Type>
{
err(m.span.start, ErrorCode::TypeError, format!("NYI"))
}

fn infer_and_check_lambda(ctx: &mut TypeCheckerContext, m: &Lambda) -> CompileResult<Type>
{
err(m.span.start, ErrorCode::TypeError, format!("NYI"))
}

fn infer_and_check_name(ctx: &mut TypeCheckerContext, nr: &NameRef) -> CompileResult<Type>
{
err(nr.span.start, ErrorCode::TypeError, format!("NYI"))
}

pub fn infer_and_check_expression(ctx: &mut TypeCheckerContext, e: &mut Expression) -> CompileResult<Type>
{
    match *e
    {
        Expression::UnaryOp(ref mut op) => infer_and_check_unary_op(ctx, op),
        Expression::BinaryOp(ref mut op) => infer_and_check_binary_op(ctx, op),
        Expression::ArrayLiteral(ref mut a) => infer_and_check_array_literal(ctx, a),
        Expression::ArrayInitializer(ref mut a) => infer_and_check_array_initializer(ctx, a),
        Expression::ArrayPattern(ref a) => Ok(Type::Unknown), // Doesn't really have a type
        Expression::Call(ref mut c) => infer_and_check_call(ctx, c),
        Expression::NameRef(ref nr) => infer_and_check_name(ctx, nr),
        Expression::Function(ref f) => infer_and_check_function(ctx, f),
        Expression::Match(ref m) => infer_and_check_match(ctx, m),
        Expression::Lambda(ref l) => infer_and_check_lambda(ctx, l),
        Expression::Enclosed(_, ref mut inner) => infer_and_check_expression(ctx, inner),
        Expression::IntLiteral(_, _) => Ok(Type::Int),
        Expression::FloatLiteral(_, _) => Ok(Type::Float),
        Expression::StringLiteral(_, _)  => Ok(Type::String),
        Expression::BoolLiteral(_, _) => Ok(Type::Bool),
    }
}

/*
    Type check and infer all the unkown types
*/
pub fn infer_and_check_types(module: &mut Module) -> CompileResult<()>
{
    let mut ctx = TypeCheckerContext::new();
    for ref mut e in module.expressions.iter_mut()
    {
        try!(infer_and_check_expression(&mut ctx, e));
    }

    Ok(())
}
