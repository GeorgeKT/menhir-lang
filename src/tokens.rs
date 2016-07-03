use std::fmt::{Formatter, Display, Error};
use compileerror::Pos;

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
    Assign,
    Arrow,
    Range,
    Increment,
    Decrement,
}

impl Display for Operator
{
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error>
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
            Operator::Assign => write!(fmt, "="),
            Operator::Arrow => write!(fmt, "->"),
            Operator::Range => write!(fmt, ".."),
            Operator::Increment => write!(fmt, "++"),
            Operator::Decrement => write!(fmt, "--"),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TokenKind
{
    Import,
    Var,
    Const,
    Func,
    If,
    Else,
    While,
    For,
    Return,
    Struct,
    Pub,
    In,
    Match,
    Union, 
    Identifier(String),
    Colon,
    Comma,
    OpenParen,
    CloseParen,
    Number(String),
    StringLiteral(String),
    Indent(usize),
    Operator(Operator),
    EOF,
}

impl Display for TokenKind
{
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error>
    {
        match *self
        {
            TokenKind::Identifier(ref s) => write!(fmt, "identifier '{}'", s),
            TokenKind::Number(ref n) => write!(fmt, "number '{}'", n),
            TokenKind::StringLiteral(ref s) => write!(fmt, "string litteral '{}'", s),
            TokenKind::Indent(i) => write!(fmt, "indentation (level {})", i),
            TokenKind::Operator(ref op) => write!(fmt, "operator {:?}", op),
            _ => write!(fmt, "{:?}", self),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Token
{
    pub kind: TokenKind,
    pub pos: Pos,
}

impl Token
{
    pub fn new(kind: TokenKind, pos: Pos) -> Token
    {
        Token{
            kind: kind,
            pos: pos,
        }
    }

    pub fn is_indent(&self) -> bool
    {
        match self.kind
        {
            TokenKind::Indent(_) => true,
            _ => false,
        }
    }
}
