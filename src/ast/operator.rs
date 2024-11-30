use serde_derive::{Deserialize, Serialize};
use std::fmt;

pub const TOP_PRECEDENCE: usize = 2000;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Serialize, Deserialize)]
pub enum BinaryOperator {
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
    LeftShift,
    RightShift,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
}

impl fmt::Display for BinaryOperator {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match *self {
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
            BinaryOperator::LeftShift => write!(fmt, "<<"),
            BinaryOperator::RightShift => write!(fmt, ">>"),
            BinaryOperator::BitwiseAnd => write!(fmt, "&"),
            BinaryOperator::BitwiseOr => write!(fmt, "|"),
            BinaryOperator::BitwiseXor => write!(fmt, "^"),
        }
    }
}

impl BinaryOperator {
    pub fn precedence(&self) -> usize {
        match *self {
            BinaryOperator::Dot | BinaryOperator::As => TOP_PRECEDENCE,
            BinaryOperator::Mul | BinaryOperator::Div | BinaryOperator::Mod => TOP_PRECEDENCE - 100,
            BinaryOperator::Add | BinaryOperator::Sub => TOP_PRECEDENCE - 200,
            BinaryOperator::LeftShift | BinaryOperator::RightShift => TOP_PRECEDENCE - 210,
            BinaryOperator::BitwiseAnd => TOP_PRECEDENCE - 220,
            BinaryOperator::BitwiseXor => TOP_PRECEDENCE - 230,
            BinaryOperator::BitwiseOr => TOP_PRECEDENCE - 240,
            BinaryOperator::LessThan
            | BinaryOperator::GreaterThan
            | BinaryOperator::LessThanEquals
            | BinaryOperator::GreaterThanEquals
            | BinaryOperator::Equals
            | BinaryOperator::NotEquals => TOP_PRECEDENCE - 300,
            BinaryOperator::And => TOP_PRECEDENCE - 400,
            BinaryOperator::Or => TOP_PRECEDENCE - 500,
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Serialize, Deserialize)]
pub enum UnaryOperator {
    Not,
    BitwiseNot,
    Sub,
    TryResult,
    TryOptional,
}

impl fmt::Display for UnaryOperator {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            UnaryOperator::Not => write!(fmt, "!"),
            UnaryOperator::BitwiseNot => write!(fmt, "~"),
            UnaryOperator::Sub => write!(fmt, "-"),
            UnaryOperator::TryResult => write!(fmt, "!"),
            UnaryOperator::TryOptional => write!(fmt, "?"),
        }
    }
}
