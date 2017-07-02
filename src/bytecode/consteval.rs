use ast::{Expression, Literal, UnaryOperator, UnaryOp, BinaryOperator, BinaryOp, Block, IntSize};
use bytecode::Constant;

fn lit_to_const(lit: &Literal) -> Option<Constant>
{
    match *lit {
        Literal::Int(_, v, int_size) => Some(Constant::Int(v, int_size)),
        Literal::UInt(_, v, int_size) => Some(Constant::UInt(v, int_size)),
        Literal::Bool(_, v) => Some(Constant::Bool(v)),
        Literal::Char(_, v) => Some(Constant::Char(v)),
        Literal::String(_, ref v) => Some(Constant::String(v.clone())),
        Literal::NullPtr(_, ref inner_type) => Some(Constant::NullPtr(inner_type.clone())),

        Literal::Float(_, ref v, float_size) => {
            match v.parse::<f64>() {
                Ok(f) => Some(Constant::Float(f, float_size)),
                _ => panic!("Internal Compiler Error: {} is not a valid floating point number", v)
            }
        }

        Literal::Array(ref array_lit) => {
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

        (UnaryOperator::Sub, Constant::Int(v, int_size)) =>
            Some(Constant::Int(-v, int_size)),

        (UnaryOperator::Sub, Constant::UInt(v, int_size)) =>
            Some(Constant::Int(-(v as i64), int_size)),

        (UnaryOperator::Sub, Constant::Float(v, float_size)) =>
            Some(Constant::Float(-v, float_size)),

        _ => None,
    }
}

#[cfg_attr(feature = "cargo-clippy", allow(float_cmp))]
fn binary_op_to_const(bop: &BinaryOp) -> Option<Constant>
{
    let left = try_opt!(expr_to_const(&bop.left));
    let right = try_opt!(expr_to_const(&bop.right));
    
    match (bop.operator, left, right) {
        (BinaryOperator::Add, Constant::Int(l, ls), Constant::Int(r, _)) => Some(Constant::Int(l + r, ls)),
        (BinaryOperator::Add, Constant::UInt(l, ls), Constant::UInt(r, _)) => Some(Constant::UInt(l + r, ls)),
        (BinaryOperator::Add, Constant::Float(l, ls), Constant::Float(r, _)) => Some(Constant::Float(l + r, ls)),

        (BinaryOperator::Sub, Constant::Int(l, ls), Constant::Int(r, _)) => Some(Constant::Int(l - r, ls)),
        (BinaryOperator::Sub, Constant::UInt(l, ls), Constant::UInt(r, _)) => Some(Constant::UInt(l - r, ls)),
        (BinaryOperator::Sub, Constant::Float(l, ls), Constant::Float(r, _)) => Some(Constant::Float(l - r, ls)),

        (BinaryOperator::Mul, Constant::Int(l, ls), Constant::Int(r, _)) => Some(Constant::Int(l * r, ls)),
        (BinaryOperator::Mul, Constant::UInt(l, ls), Constant::UInt(r, _)) => Some(Constant::UInt(l * r, ls)),
        (BinaryOperator::Mul, Constant::Float(l, ls), Constant::Float(r, _)) => Some(Constant::Float(l * r, ls)),

        (BinaryOperator::Div, Constant::Int(l, ls), Constant::Int(r, _)) => Some(Constant::Int(l / r, ls)),
        (BinaryOperator::Div, Constant::UInt(l, ls), Constant::UInt(r, _)) => Some(Constant::UInt(l / r, ls)),
        (BinaryOperator::Div, Constant::Float(l, ls), Constant::Float(r, _)) => Some(Constant::Float(l / r, ls)),

        (BinaryOperator::Mod, Constant::Int(l, ls), Constant::Int(r, _)) => Some(Constant::Int(l % r, ls)),
        (BinaryOperator::Mod, Constant::UInt(l, ls), Constant::UInt(r, _)) => Some(Constant::UInt(l % r, ls)),

        (BinaryOperator::LessThan, Constant::Int(l, _), Constant::Int(r, _)) => Some(Constant::Bool(l < r)),
        (BinaryOperator::LessThan, Constant::UInt(l, _), Constant::UInt(r, _)) => Some(Constant::Bool(l < r)),
        (BinaryOperator::LessThan, Constant::Float(l, _), Constant::Float(r, _)) => Some(Constant::Bool(l < r)),
        (BinaryOperator::LessThan, Constant::Char(l), Constant::Char(r)) => Some(Constant::Bool(l < r)),

        (BinaryOperator::GreaterThan, Constant::Int(l, _), Constant::Int(r, _)) => Some(Constant::Bool(l > r)),
        (BinaryOperator::GreaterThan, Constant::UInt(l, _), Constant::UInt(r, _)) => Some(Constant::Bool(l > r)),
        (BinaryOperator::GreaterThan, Constant::Float(l, _), Constant::Float(r, _)) => Some(Constant::Bool(l > r)),
        (BinaryOperator::GreaterThan, Constant::Char(l), Constant::Char(r)) => Some(Constant::Bool(l > r)),

        (BinaryOperator::LessThanEquals, Constant::Int(l, _), Constant::Int(r, _)) => Some(Constant::Bool(l <= r)),
        (BinaryOperator::LessThanEquals, Constant::UInt(l, _), Constant::UInt(r, _)) => Some(Constant::Bool(l <= r)),
        (BinaryOperator::LessThanEquals, Constant::Float(l, _), Constant::Float(r, _)) => Some(Constant::Bool(l <= r)),
        (BinaryOperator::LessThanEquals, Constant::Char(l), Constant::Char(r)) => Some(Constant::Bool(l <= r)),

        (BinaryOperator::GreaterThanEquals, Constant::Int(l, _), Constant::Int(r, _)) => Some(Constant::Bool(l >= r)),
        (BinaryOperator::GreaterThanEquals, Constant::UInt(l, _), Constant::UInt(r, _)) => Some(Constant::Bool(l >= r)),
        (BinaryOperator::GreaterThanEquals, Constant::Float(l, _), Constant::Float(r, _)) => Some(Constant::Bool(l >= r)),
        (BinaryOperator::GreaterThanEquals, Constant::Char(l), Constant::Char(r)) => Some(Constant::Bool(l >= r)),

        (BinaryOperator::Equals, Constant::Int(l, _), Constant::Int(r, _)) => Some(Constant::Bool(l == r)),
        (BinaryOperator::Equals, Constant::UInt(l, _), Constant::UInt(r, _)) => Some(Constant::Bool(l == r)),
        (BinaryOperator::Equals, Constant::Float(l, _), Constant::Float(r, _)) => Some(Constant::Bool(l == r)),
        (BinaryOperator::Equals, Constant::Char(l), Constant::Char(r)) => Some(Constant::Bool(l == r)),
        (BinaryOperator::Equals, Constant::Bool(l), Constant::Bool(r)) => Some(Constant::Bool(l == r)),
        (BinaryOperator::Equals, Constant::String(ref l), Constant::String(ref r)) => Some(Constant::Bool(*l == *r)),

        (BinaryOperator::NotEquals, Constant::Int(l, _), Constant::Int(r, _)) => Some(Constant::Bool(l != r)),
        (BinaryOperator::NotEquals, Constant::UInt(l, _), Constant::UInt(r, _)) => Some(Constant::Bool(l != r)),
        (BinaryOperator::NotEquals, Constant::Float(l, _), Constant::Float(r, _)) => Some(Constant::Bool(l != r)),
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
    let mut ret = Constant::Int(0, IntSize::I64);
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