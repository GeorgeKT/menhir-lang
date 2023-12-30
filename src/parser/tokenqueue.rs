use super::tokens::{Token, TokenKind};
use crate::ast::{AssignOperator, BinaryOperator};
use crate::compileerror::{parse_error, parse_error_result, CompileError, CompileResult, ErrorData};
use crate::span::{Pos, Span};
use std::collections::VecDeque;

#[derive(Debug)]
pub struct TokenQueue {
    tokens: VecDeque<Token>,
    last_pos: Pos,
}

impl TokenQueue {
    pub fn new() -> TokenQueue {
        TokenQueue {
            tokens: VecDeque::new(),
            last_pos: Pos::new(1, 1),
        }
    }

    pub fn add(&mut self, tok: Token) {
        self.tokens.push_back(tok);
    }

    pub fn pos(&self) -> Pos {
        self.last_pos
    }

    #[allow(dead_code)]
    pub fn dump(&self) {
        for tok in &self.tokens {
            println!("{:?}", tok);
        }
    }

    pub fn push_front(&mut self, tok: Token) {
        self.tokens.push_front(tok)
    }

    pub fn pop(&mut self) -> CompileResult<Token> {
        if let Some(tok) = self.tokens.pop_front() {
            self.last_pos = tok.span.end();
            Ok(tok)
        } else {
            parse_error_result(&Span::default(), "Unexpected end of file")
        }
    }

    pub fn pop_if(&mut self, pred: impl Fn(&Token) -> bool) -> CompileResult<Option<Token>> {
        let matches = self
            .peek()
            .map(|t| pred(t))
            .ok_or_else(|| parse_error(&Span::default(), "Unexpected end of file"))?;

        if matches {
            self.pop().map(|t| Some(t))
        } else {
            Ok(None)
        }
    }

    pub fn peek(&self) -> Option<&Token> {
        self.tokens.front()
    }

    pub fn peek_at(&self, index: usize) -> Option<&Token> {
        self.tokens.get(index)
    }

    pub fn expect(&mut self, kind: &TokenKind) -> CompileResult<Token> {
        self.pop().and_then(|tok| {
            if tok.kind == *kind {
                Ok(tok)
            } else {
                parse_error_result(
                    &tok.span,
                    format!("Unexpected token '{}', expecting '{}'", tok.kind, kind),
                )
            }
        })
    }

    pub fn expect_int(&mut self) -> CompileResult<(u64, Span)> {
        let tok = self.pop()?;
        if let TokenKind::Number(ref v) = tok.kind {
            let val = v
                .parse::<u64>()
                .map_err(|_| CompileError::Parse(ErrorData::new(&tok.span, format!("{} is not a valid integer", v))))?;
            Ok((val, tok.span))
        } else {
            parse_error_result(&tok.span, format!("Expected integer literal, found {}", tok))
        }
    }

    pub fn expect_identifier(&mut self) -> CompileResult<(String, Span)> {
        let tok = self.pop()?;
        if let TokenKind::Identifier(s) = tok.kind {
            Ok((s, tok.span))
        } else {
            parse_error_result(&tok.span, format!("Expected identifier, found {}", tok))
        }
    }

    pub fn expect_binary_operator(&mut self) -> CompileResult<BinaryOperator> {
        let tok = self.pop()?;
        if let TokenKind::BinaryOperator(op) = tok.kind {
            Ok(op)
        } else {
            parse_error_result(&tok.span, format!("Expected operator, found {}", tok))
        }
    }

    pub fn is_next(&self, kind: &TokenKind) -> bool {
        match self.tokens.front() {
            Some(tok) => tok.kind == *kind,
            None => false,
        }
    }

    pub fn is_next_at(&self, index: usize, kind: &TokenKind) -> bool {
        match self.peek_at(index) {
            Some(tok) => tok.kind == *kind,
            None => false,
        }
    }

    pub fn is_next_binary_operator(&self) -> bool {
        match self.tokens.front() {
            Some(tok) => {
                matches!(tok.kind, TokenKind::BinaryOperator(_))
            }
            None => false,
        }
    }

    pub fn is_next_assign_operator(&self) -> Option<AssignOperator> {
        match self.tokens.front() {
            Some(tok) => {
                if let TokenKind::Assign(op) = tok.kind {
                    Some(op)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    pub fn is_next_identifier(&self, value: &str) -> bool {
        match self.tokens.front() {
            Some(tok) => {
                if let TokenKind::Identifier(v) = &tok.kind {
                    v == value
                } else {
                    false
                }
            }
            None => false,
        }
    }

    pub fn is_in_same_block(&self, indent_level: usize) -> bool {
        match self.tokens.front() {
            Some(tok) => {
                if let TokenKind::Indent(level) = tok.kind {
                    level >= indent_level
                } else {
                    tok.kind != TokenKind::EOF
                }
            }
            None => false,
        }
    }

    pub fn pop_indent(&mut self) -> CompileResult<Option<(usize, Span)>> {
        let level = if let Some(tok) = self.tokens.front() {
            if let TokenKind::Indent(level) = tok.kind {
                level
            } else {
                return Ok(None);
            }
        } else {
            return Ok(None);
        };

        let tok = self.pop()?;
        Ok(Some((level, tok.span)))
    }
}

impl Iterator for TokenQueue {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.tokens.pop_front()
    }
}
