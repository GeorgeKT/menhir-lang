use crate::ast::{AssignOperator, BinaryOperator, UnaryOperator};
use crate::span::Span;
use std::fmt::{Display, Error, Formatter};

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TokenKind {
    Identifier(String),
    Number(String),
    StringLiteral(String),
    CharLiteral(char),
    BinaryOperator(BinaryOperator),
    UnaryOperator(UnaryOperator),
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
    Assign(AssignOperator),
    Match,
    Let,
    In,
    Import,
    If,
    Else,
    While,
    Extern,
    Dollar,
    Pipe,
    True,
    False,
    Type,
    Struct,
    Enum,
    Tilde,
    New,
    Delete,
    QuestionMark,
    Nil,
    Null,
    Var,
    For,
    Interface,
    Func,
    Indent(usize),
    Ampersand,
    At,
    Return,
    EOF,
}

impl Display for TokenKind {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        match *self {
            TokenKind::Identifier(ref s) => write!(fmt, "identifier '{}'", s),
            TokenKind::Number(ref n) => write!(fmt, "number '{}'", n),
            TokenKind::StringLiteral(ref s) => write!(fmt, "string litteral '{}'", s),
            TokenKind::CharLiteral(c) => write!(fmt, "char literal '{}'", c),
            TokenKind::BinaryOperator(ref op) => write!(fmt, "operator {}", op),
            TokenKind::UnaryOperator(ref op) => write!(fmt, "operator {}", op),
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
            TokenKind::Assign(op) => write!(fmt, "{}", op),
            TokenKind::Dollar => write!(fmt, "$"),
            TokenKind::Pipe => write!(fmt, "|"),
            TokenKind::True => write!(fmt, "true"),
            TokenKind::False => write!(fmt, "false"),
            TokenKind::Type => write!(fmt, "type"),
            TokenKind::Struct => write!(fmt, "struct"),
            TokenKind::Enum => write!(fmt, "enum"),
            TokenKind::Tilde => write!(fmt, "~"),
            TokenKind::New => write!(fmt, "new"),
            TokenKind::Delete => write!(fmt, "delete"),
            TokenKind::QuestionMark => write!(fmt, "?"),
            TokenKind::Nil => write!(fmt, "nil"),
            TokenKind::Null => write!(fmt, "null"),
            TokenKind::Var => write!(fmt, "var"),
            TokenKind::For => write!(fmt, "for"),
            TokenKind::Interface => write!(fmt, "interface"),
            TokenKind::Func => write!(fmt, "fn"),
            TokenKind::Indent(lvl) => write!(fmt, "indent {}", lvl),
            TokenKind::Ampersand => write!(fmt, "&"),
            TokenKind::At => write!(fmt, "@"),
            TokenKind::Return => write!(fmt, "return"),
            TokenKind::EOF => write!(fmt, "EOF"),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Token {
        Token { kind: kind, span: span }
    }
}

impl Display for Token {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        write!(fmt, "{}", self.kind)
    }
}
