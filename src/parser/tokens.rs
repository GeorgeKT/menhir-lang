use std::fmt::{Formatter, Display, Error};
use compileerror::{Span};

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
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    Arrow,
    Range,
    Increment,
    Decrement,
    Dot,
}

pub const TOP_PRECEDENCE: usize = 2000;

impl Operator
{
    pub fn precedence(&self) -> usize
    {
        match *self
        {
            Operator::Not | Operator::Arrow | Operator::Dot |
            Operator::Increment | Operator::Decrement | Operator::Range => TOP_PRECEDENCE,
            Operator::Mul | Operator::Div | Operator::Mod => TOP_PRECEDENCE - 100,
            Operator::Add | Operator::Sub => TOP_PRECEDENCE - 200,
            Operator::LessThan | Operator::GreaterThan | Operator::LessThanEquals |
            Operator::GreaterThanEquals | Operator::Equals | Operator::NotEquals => TOP_PRECEDENCE - 300,
            Operator::And => TOP_PRECEDENCE - 400,
            Operator::Or => TOP_PRECEDENCE - 500,
            Operator::Assign | Operator::AddAssign | Operator::SubAssign |
            Operator::MulAssign | Operator::DivAssign => TOP_PRECEDENCE - 600,
        }
    }

    pub fn is_binary_operator(&self) -> bool
    {
        match *self
        {
            Operator::Increment | Operator::Decrement | Operator::Not | Operator::Arrow => false,
            _ => true,
        }
    }

    pub fn is_assignment(&self) -> bool
    {
        match *self
        {
            Operator::Assign | Operator::AddAssign | Operator::SubAssign |
            Operator::MulAssign | Operator::DivAssign => true,
            _ => false,
        }
    }
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
            Operator::AddAssign => write!(fmt, "+="),
            Operator::SubAssign => write!(fmt, "-="),
            Operator::DivAssign => write!(fmt, "*="),
            Operator::MulAssign => write!(fmt, "/="),
            Operator::Dot => write!(fmt, "."),
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
    Extern,
    Identifier(String),
    Colon,
    DoubleColon,
    Comma,
    OpenParen,
    CloseParen,
    OpenCurly,
    CloseCurly,
    OpenBracket,
    CloseBracket,
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
            TokenKind::Operator(ref op) => write!(fmt, "operator {}", op),
            _ => write!(fmt, "{:?}", self),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Token
{
    pub kind: TokenKind,
    pub span: Span,
}

impl Token
{
    pub fn new(kind: TokenKind, span: Span) -> Token
    {
        Token{
            kind: kind,
            span: span,
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
