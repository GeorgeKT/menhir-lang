use std::fmt::{Formatter, Display, Error};
use span::{Span};

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
    Extract, // <-
    Dot,
}

pub const TOP_PRECEDENCE: usize = 2000;

impl Operator
{
    pub fn precedence(&self) -> usize
    {
        match *self
        {
            Operator::Not | Operator::Dot => TOP_PRECEDENCE,
            Operator::Mul | Operator::Div | Operator::Mod => TOP_PRECEDENCE - 100,
            Operator::Add | Operator::Sub => TOP_PRECEDENCE - 200,
            Operator::LessThan | Operator::GreaterThan | Operator::LessThanEquals |
            Operator::GreaterThanEquals | Operator::Equals | Operator::NotEquals => TOP_PRECEDENCE - 300,
            Operator::And => TOP_PRECEDENCE - 400,
            Operator::Or => TOP_PRECEDENCE - 500,
            Operator::Extract => TOP_PRECEDENCE - 600,
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
            Operator::Extract => write!(fmt, "<-"),
            Operator::Dot => write!(fmt, "."),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TokenKind
{
    Identifier(String),
    Number(String),
    StringLiteral(String),
    CharLiteral(char),
    Operator(Operator),
    Colon,
    DoubleColon,
    SemiColon,
    Comma,
    OpenParen,
    CloseParen,
    OpenCurly,
    CloseCurly,
    OpenBracket,
    CloseBracket,
    Arrow,
    FatArrow,
    Assign,
    Match,
    Let,
    In,
    Import,
    If,
    Else,
    Extern,
    Lambda,
    Dollar,
    Pipe,
    True,
    False,
    Type,
    Tilde,
    New,
    Delete,
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
            TokenKind::CharLiteral(c) => write!(fmt, "char literal '{}'", c),
            TokenKind::Operator(ref op) => write!(fmt, "operator {}", op),
            TokenKind::Colon => write!(fmt, ":"),
            TokenKind::DoubleColon => write!(fmt, "::"),
            TokenKind::SemiColon => write!(fmt, ";"),
            TokenKind::Comma => write!(fmt, ","),
            TokenKind::OpenParen => write!(fmt, "("),
            TokenKind::CloseParen => write!(fmt, ")"),
            TokenKind::OpenCurly => write!(fmt, "{}", '{'),
            TokenKind::CloseCurly => write!(fmt, "{}", '}'),
            TokenKind::OpenBracket => write!(fmt, "["),
            TokenKind::CloseBracket => write!(fmt, "]"),
            TokenKind::Arrow => write!(fmt, "->"),
            TokenKind::FatArrow => write!(fmt, "=>"),
            TokenKind::Match => write!(fmt, "match"),
            TokenKind::Let => write!(fmt, "let"),
            TokenKind::In => write!(fmt, "in"),
            TokenKind::Import => write!(fmt, "import"),
            TokenKind::If => write!(fmt, "if"),
            TokenKind::Else => write!(fmt, "else"),
            TokenKind::Extern => write!(fmt, "extern"),
            TokenKind::Lambda => write!(fmt, "@"),
            TokenKind::Assign => write!(fmt, "="),
            TokenKind::Dollar => write!(fmt, "$"),
            TokenKind::Pipe => write!(fmt, "|"),
            TokenKind::True => write!(fmt, "true"),
            TokenKind::False => write!(fmt, "false"),
            TokenKind::Type => write!(fmt, "type"),
            TokenKind::Tilde => write!(fmt, "~"),
            TokenKind::New => write!(fmt, "new"),
            TokenKind::Delete => write!(fmt, "delete"),
            TokenKind::EOF => write!(fmt, "EOF"),
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
}


impl Display for Token
{
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error>
    {
        write!(fmt, "{}", self.kind)
    }
}
