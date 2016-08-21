use std::collections::VecDeque;
use compileerror::{Pos, Span, CompileError, CompileResult, ErrorCode, err};
use parser::{Token, TokenKind, Operator};



pub struct TokenQueue
{
    tokens: VecDeque<Token>,
    last_pos: Pos,
}

impl TokenQueue
{
    pub fn new() -> TokenQueue
    {
        TokenQueue{
            tokens: VecDeque::new(),
            last_pos: Pos::new(1, 1),
        }
    }

    pub fn add(&mut self, tok: Token)
    {
        self.tokens.push_back(tok);
    }

    pub fn pos(&self) -> Pos
    {
        self.last_pos
    }

    #[allow(dead_code)]
    pub fn dump(&self)
    {
        for tok in &self.tokens
        {
            println!("{:?}", tok);
        }
    }

    pub fn push_front(&mut self, tok: Token)
    {
        self.tokens.push_front(tok)
    }

    pub fn pop(&mut self) -> CompileResult<Token>
    {
        if let Some(tok) = self.tokens.pop_front() {
            self.last_pos = tok.span.end;
            Ok(tok)
        } else {
            err(self.last_pos, ErrorCode::UnexpectedEOF, format!("Unexpected end of file"))
        }
    }

    pub fn peek(&self) -> Option<&Token>
    {
        self.tokens.front()
    }

    pub fn peek_at(&self, index: usize) -> Option<&Token>
    {
        self.tokens.iter().nth(index)
    }

    pub fn expect(&mut self, kind: TokenKind) -> CompileResult<Token>
    {
        self.pop().and_then(
            |tok| if tok.kind == kind
            {
                Ok(tok)
            }
            else
            {
                Err(CompileError::new(tok.span.start, ErrorCode::UnexpectedToken, format!("Unexpected token '{}'", tok)))
            })
    }

    pub fn expect_int(&mut self) -> CompileResult<(u64, Pos)>
    {
        let tok = try!(self.pop());
        if let TokenKind::Number(v) = tok.kind
        {
            let pos = tok.span.start;
            let val = try!(v.parse::<u64>().map_err(|_| CompileError::new(pos, ErrorCode::InvalidInteger, format!("{} is not a valid integer", v))));
            Ok((val, tok.span.start))
        }
        else
        {
            err(tok.span.start, ErrorCode::ExpectedIntLiteral, format!("Expected integer literal, found {}", tok))
        }
    }

    pub fn expect_identifier(&mut self) -> CompileResult<(String, Span)>
    {
        let tok = try!(self.pop());
        if let TokenKind::Identifier(s) = tok.kind
        {
            Ok((s, tok.span))
        }
        else
        {
            err(tok.span.start, ErrorCode::ExpectedIdentifier, format!("Expected identifier, found {}", tok))
        }
    }

    pub fn expect_operator(&mut self) -> CompileResult<Operator>
    {
        let tok = try!(self.pop());
        if let TokenKind::Operator(op) = tok.kind
        {
            Ok(op)
        }
        else
        {
            err(tok.span.start, ErrorCode::ExpectedOperator, format!("Expected operator, found {}", tok))
        }
    }

    pub fn is_next(&self, kind: TokenKind) -> bool
    {
        match self.tokens.front()
        {
            Some(tok) => tok.kind == kind,
            None => false,
        }
    }

    pub fn is_next_at(&self, index: usize, kind: TokenKind) -> bool
    {
        match self.peek_at(index)
        {
            Some(tok) => tok.kind == kind,
            None => false,
        }
    }


    pub fn is_next_operator(&self) -> bool
    {
        match self.tokens.front()
        {
            Some(tok) => if let TokenKind::Operator(_) = tok.kind {true} else {false},
            None => false,
        }
    }
}

impl Iterator for TokenQueue
{
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item>
    {
        self.tokens.pop_front()
    }
}
