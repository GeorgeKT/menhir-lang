use std::fmt;

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Operator
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
    Not,
    And,
    Or,
    Dot,
    As,
}

pub const TOP_PRECEDENCE: usize = 2000;

impl Operator
{
    pub fn precedence(&self) -> usize
    {
        match *self
        {
            Operator::Not | Operator::Dot | Operator::As => TOP_PRECEDENCE,
            Operator::Mul | Operator::Div | Operator::Mod => TOP_PRECEDENCE - 100,
            Operator::Add | Operator::Sub => TOP_PRECEDENCE - 200,
            Operator::LessThan | Operator::GreaterThan | Operator::LessThanEquals |
            Operator::GreaterThanEquals | Operator::Equals | Operator::NotEquals => TOP_PRECEDENCE - 300,
            Operator::And => TOP_PRECEDENCE - 400,
            Operator::Or => TOP_PRECEDENCE - 500,
        }
    }

    pub fn is_binary_operator(&self) -> bool
    {
        match *self
        {
            Operator::Not => false,
            _ => true,
        }
    }
}

impl fmt::Display for Operator
{
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error>
    {
        match *self
        {
            Operator::Add => write!(fmt, "+"),
            Operator::Sub => write!(fmt, "-"),
            Operator::Mul => write!(fmt, "*"),
            Operator::Div => write!(fmt, "/"),
            Operator::Mod => write!(fmt, "%"),
            Operator::LessThan => write!(fmt, "<"),
            Operator::GreaterThan => write!(fmt, ">"),
            Operator::LessThanEquals => write!(fmt, "<="),
            Operator::GreaterThanEquals => write!(fmt, ">="),
            Operator::Equals => write!(fmt, "=="),
            Operator::NotEquals => write!(fmt, "!="),
            Operator::Not => write!(fmt, "!"),
            Operator::And => write!(fmt, "&&"),
            Operator::Or => write!(fmt, "||"),
            Operator::Dot => write!(fmt, "."),
            Operator::As => write!(fmt, "as"),
        }
    }
}
