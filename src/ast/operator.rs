use std::fmt;

pub const TOP_PRECEDENCE: usize = 2000;

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum BinaryOperator
{
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    LessThan,
    GreaterThan,
    LessThanEquals,
    GreaterThanEquals,
    Equals,
    NotEquals,
    And,
    Or,
    Dot,
    As,
}


impl fmt::Display for BinaryOperator
{
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error>
    {
        match *self
        {
            BinaryOperator::Add => write!(fmt, "+"),
            BinaryOperator::Sub => write!(fmt, "-"),
            BinaryOperator::Mul => write!(fmt, "*"),
            BinaryOperator::Div => write!(fmt, "/"),
            BinaryOperator::Mod => write!(fmt, "%"),
            BinaryOperator::LessThan => write!(fmt, "<"),
            BinaryOperator::GreaterThan => write!(fmt, ">"),
            BinaryOperator::LessThanEquals => write!(fmt, "<="),
            BinaryOperator::GreaterThanEquals => write!(fmt, ">="),
            BinaryOperator::Equals => write!(fmt, "=="),
            BinaryOperator::NotEquals => write!(fmt, "!="),
            BinaryOperator::And => write!(fmt, "&&"),
            BinaryOperator::Or => write!(fmt, "||"),
            BinaryOperator::Dot => write!(fmt, "."),
            BinaryOperator::As => write!(fmt, "as"),
        }
    }
}

impl BinaryOperator
{
    pub fn precedence(&self) -> usize
    {
        match *self
        {
            BinaryOperator::Dot | BinaryOperator::As => TOP_PRECEDENCE,
            BinaryOperator::Mul | BinaryOperator::Div | BinaryOperator::Mod => TOP_PRECEDENCE - 100,
            BinaryOperator::Add | BinaryOperator::Sub => TOP_PRECEDENCE - 200,
            BinaryOperator::LessThan | BinaryOperator::GreaterThan | BinaryOperator::LessThanEquals |
            BinaryOperator::GreaterThanEquals | BinaryOperator::Equals | BinaryOperator::NotEquals => TOP_PRECEDENCE - 300,
            BinaryOperator::And => TOP_PRECEDENCE - 400,
            BinaryOperator::Or => TOP_PRECEDENCE - 500,
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum UnaryOperator
{
    Not,
    Sub,
}

impl fmt::Display for UnaryOperator
{
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error>
    {
        match *self
        {
            UnaryOperator::Not => write!(fmt, "!"),
            UnaryOperator::Sub => write!(fmt, "-"),
        }
    }
}
