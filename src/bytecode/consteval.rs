use ast::{Expression, Literal, UnaryOperator, UnaryOp, BinaryOperator, BinaryOp, Block};
use bytecode::Constant;

macro_rules! try_opt {
    ($e:expr) =>(
        match $e {
            Some(v) => v,
            None => return None,
        }
    )
}


fn lit_to_const(lit: &Literal) -> Option<Constant>
{
    match lit {
        &Literal::Int(_, v) => Some(Constant::Int(v)),
        &Literal::UInt(_, v) => Some(Constant::UInt(v)),
        &Literal::Bool(_, v) => Some(Constant::Bool(v)),
        &Literal::Char(_, v) => Some(Constant::Char(v)),
        &Literal::String(_, ref v) => Some(Constant::String(v.clone())),

        &Literal::Float(_, ref v) => {
            match v.parse::<f64>() {
                Ok(f) => Some(Constant::Float(f)),
                Err(_) => panic!("Internal Compiler Error: {} is not a valid floating point number", v)
            }
        }

        &Literal::Array(ref array_lit) => {
            let mut elements = Vec::with_capacity(array_lit.elements.len());
            for e in &array_lit.elements {
                if let Some(c) = expr_to_const(e) {
                    elements.push(c);
                } else {
                    return None
                }
            }

            Some(Constant::Array(elements))
        }

    }
}

fn unary_op_to_const(uop: &UnaryOp) -> Option<Constant>
{
    let cst = try_opt!(expr_to_const(&uop.expression));

    match (uop.operator, cst)  {
        (UnaryOperator::Not, Constant::Bool(v)) =>
            Some(Constant::Bool(!v)),

        (UnaryOperator::Sub, Constant::Int(v)) =>
            Some(Constant::Int(-v)),

        (UnaryOperator::Sub, Constant::UInt(v)) =>
            Some(Constant::Int(-(v as isize))),

        (UnaryOperator::Sub, Constant::Float(v)) =>
            Some(Constant::Float(-v)),

        _ => None,
    }
}

fn binary_op_to_const(bop: &BinaryOp) -> Option<Constant>
{
    let left = try_opt!(expr_to_const(&bop.left));
    let right = try_opt!(expr_to_const(&bop.right));
    
    match (bop.operator, left, right) {
        (BinaryOperator::Add, Constant::Int(l), Constant::Int(r)) => Some(Constant::Int(l + r)),
        (BinaryOperator::Add, Constant::UInt(l), Constant::UInt(r)) => Some(Constant::UInt(l + r)),
        (BinaryOperator::Add, Constant::Float(l), Constant::Float(r)) => Some(Constant::Float(l + r)),

        (BinaryOperator::Sub, Constant::Int(l), Constant::Int(r)) => Some(Constant::Int(l - r)),
        (BinaryOperator::Sub, Constant::UInt(l), Constant::UInt(r)) => Some(Constant::UInt(l - r)),
        (BinaryOperator::Sub, Constant::Float(l), Constant::Float(r)) => Some(Constant::Float(l - r)),

        (BinaryOperator::Mul, Constant::Int(l), Constant::Int(r)) => Some(Constant::Int(l * r)),
        (BinaryOperator::Mul, Constant::UInt(l), Constant::UInt(r)) => Some(Constant::UInt(l * r)),
        (BinaryOperator::Mul, Constant::Float(l), Constant::Float(r)) => Some(Constant::Float(l * r)),

        (BinaryOperator::Div, Constant::Int(l), Constant::Int(r)) => Some(Constant::Int(l / r)),
        (BinaryOperator::Div, Constant::UInt(l), Constant::UInt(r)) => Some(Constant::UInt(l / r)),
        (BinaryOperator::Div, Constant::Float(l), Constant::Float(r)) => Some(Constant::Float(l / r)),

        (BinaryOperator::Mod, Constant::Int(l), Constant::Int(r)) => Some(Constant::Int(l % r)),
        (BinaryOperator::Mod, Constant::UInt(l), Constant::UInt(r)) => Some(Constant::UInt(l % r)),

        (BinaryOperator::LessThan, Constant::Int(l), Constant::Int(r)) => Some(Constant::Bool(l < r)),
        (BinaryOperator::LessThan, Constant::UInt(l), Constant::UInt(r)) => Some(Constant::Bool(l < r)),
        (BinaryOperator::LessThan, Constant::Float(l), Constant::Float(r)) => Some(Constant::Bool(l < r)),
        (BinaryOperator::LessThan, Constant::Char(l), Constant::Char(r)) => Some(Constant::Bool(l < r)),

        (BinaryOperator::GreaterThan, Constant::Int(l), Constant::Int(r)) => Some(Constant::Bool(l > r)),
        (BinaryOperator::GreaterThan, Constant::UInt(l), Constant::UInt(r)) => Some(Constant::Bool(l > r)),
        (BinaryOperator::GreaterThan, Constant::Float(l), Constant::Float(r)) => Some(Constant::Bool(l > r)),
        (BinaryOperator::GreaterThan, Constant::Char(l), Constant::Char(r)) => Some(Constant::Bool(l > r)),

        (BinaryOperator::LessThanEquals, Constant::Int(l), Constant::Int(r)) => Some(Constant::Bool(l <= r)),
        (BinaryOperator::LessThanEquals, Constant::UInt(l), Constant::UInt(r)) => Some(Constant::Bool(l <= r)),
        (BinaryOperator::LessThanEquals, Constant::Float(l), Constant::Float(r)) => Some(Constant::Bool(l <= r)),
        (BinaryOperator::LessThanEquals, Constant::Char(l), Constant::Char(r)) => Some(Constant::Bool(l <= r)),

        (BinaryOperator::GreaterThanEquals, Constant::Int(l), Constant::Int(r)) => Some(Constant::Bool(l >= r)),
        (BinaryOperator::GreaterThanEquals, Constant::UInt(l), Constant::UInt(r)) => Some(Constant::Bool(l >= r)),
        (BinaryOperator::GreaterThanEquals, Constant::Float(l), Constant::Float(r)) => Some(Constant::Bool(l >= r)),
        (BinaryOperator::GreaterThanEquals, Constant::Char(l), Constant::Char(r)) => Some(Constant::Bool(l >= r)),

        (BinaryOperator::Equals, Constant::Int(l), Constant::Int(r)) => Some(Constant::Bool(l == r)),
        (BinaryOperator::Equals, Constant::UInt(l), Constant::UInt(r)) => Some(Constant::Bool(l == r)),
        (BinaryOperator::Equals, Constant::Float(l), Constant::Float(r)) => Some(Constant::Bool(l == r)),
        (BinaryOperator::Equals, Constant::Char(l), Constant::Char(r)) => Some(Constant::Bool(l == r)),
        (BinaryOperator::Equals, Constant::Bool(l), Constant::Bool(r)) => Some(Constant::Bool(l == r)),
        (BinaryOperator::Equals, Constant::String(ref l), Constant::String(ref r)) => Some(Constant::Bool(*l == *r)),

        (BinaryOperator::NotEquals, Constant::Int(l), Constant::Int(r)) => Some(Constant::Bool(l != r)),
        (BinaryOperator::NotEquals, Constant::UInt(l), Constant::UInt(r)) => Some(Constant::Bool(l != r)),
        (BinaryOperator::NotEquals, Constant::Float(l), Constant::Float(r)) => Some(Constant::Bool(l != r)),
        (BinaryOperator::NotEquals, Constant::Char(l), Constant::Char(r)) => Some(Constant::Bool(l != r)),
        (BinaryOperator::NotEquals, Constant::Bool(l), Constant::Bool(r)) => Some(Constant::Bool(l != r)),
        (BinaryOperator::NotEquals, Constant::String(ref l), Constant::String(ref r)) => Some(Constant::Bool(*l != *r)),

        (BinaryOperator::And, Constant::Bool(l), Constant::Bool(r)) => Some(Constant::Bool(l && r)),
        (BinaryOperator::Or, Constant::Bool(l), Constant::Bool(r)) => Some(Constant::Bool(l || r)),

        _ => None,
    }
}

fn block_to_const(block: &Block) -> Option<Constant>
{
    let mut ret = Constant::Int(0);
    for e in &block.expressions {
        ret = try_opt!(expr_to_const(e));
    }

    Some(ret)
}


pub fn expr_to_const(expr: &Expression) -> Option<Constant>
{
    match *expr
    {
        Expression::Literal(ref lit) => {
            lit_to_const(lit)
        },

        Expression::UnaryOp(ref uop) => {
            unary_op_to_const(uop)
        },

        Expression::BinaryOp(ref bop) => {
            binary_op_to_const(bop)
        },

        Expression::Block(ref block) => {
            block_to_const(block)
        }

        _ => None,
    }
}