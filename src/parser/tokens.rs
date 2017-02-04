use std::fmt::{Formatter, Display, Error};
use span::{Span};
use ast::{Operator};

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
    While,
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
    QuestionMark,
    Nil,
    Var,
    For,
    Interface,
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
            TokenKind::While => write!(fmt, "while"),
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
            TokenKind::QuestionMark => write!(fmt, "?"),
            TokenKind::Nil => write!(fmt, "nil"),
            TokenKind::Var => write!(fmt, "var"),
            TokenKind::For => write!(fmt, "for"),
            TokenKind::Interface => write!(fmt, "interface"),
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
