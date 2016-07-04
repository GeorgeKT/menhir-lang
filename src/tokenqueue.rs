use std::collections::VecDeque;
use compileerror::{Pos, CompileError, ErrorType};
use tokens::{Token, TokenKind, Operator};



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
        self.last_pos = tok.pos;

        // Remove consecutive indents
        if let Some(last) = self.tokens.back_mut()
        {
            if tok.is_indent() && last.is_indent() {
                *last = tok;
                return;
            }
        }

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

    pub fn pop(&mut self) -> Result<Token, CompileError>
    {
        self.tokens.pop_front()
            .ok_or(CompileError::new(self.last_pos, ErrorType::UnexpectedEOF))
    }

    pub fn pop_if<P>(&mut self, predicate: P) ->  Result<Option<Token>, CompileError>
        where P: Fn(&Token) -> bool
    {
        let pop = self.tokens.front().map(|tok| predicate(tok)).unwrap_or(false);
        if pop {
            self.pop().map(|tok| Some(tok))
        } else {
            Ok(None)
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

    pub fn expect(&mut self, kind: TokenKind) -> Result<Token, CompileError>
    {
        self.pop()
            .and_then(|tok| if tok.kind == kind
                {
                    Ok(tok)
                }
                else
                {
                    Err(CompileError::new(tok.pos, ErrorType::UnexpectedToken(tok)))
                })
    }

    pub fn expect_indent(&mut self) -> Result<usize, CompileError>
    {
        let tok = try!(self.pop());
        if let TokenKind::Indent(i) = tok.kind
        {
            Ok(i)
        }
        else
        {
            Err(CompileError::new(tok.pos, ErrorType::ExpectedIndent))
        }
    }

    pub fn expect_string(&mut self) -> Result<(String, Pos), CompileError>
    {
        let tok = try!(self.pop());
        if let TokenKind::StringLiteral(s) = tok.kind
        {
            Ok((s, tok.pos))
        }
        else
        {
            Err(CompileError::new(tok.pos, ErrorType::ExpectedStringLiteral))
        }
    }

    pub fn expect_identifier(&mut self) -> Result<(String, Pos), CompileError>
    {
        let tok = try!(self.pop());
        if let TokenKind::Identifier(s) = tok.kind
        {
            Ok((s, tok.pos))
        }
        else
        {
            Err(CompileError::new(tok.pos, ErrorType::ExpectedIdentifier))
        }
    }

    pub fn expect_operator(&mut self) -> Result<Operator, CompileError>
    {
        let tok = try!(self.pop());
        if let TokenKind::Operator(op) = tok.kind
        {
            Ok(op)
        }
        else
        {
            Err(CompileError::new(tok.pos, ErrorType::ExpectedOperator))
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

    pub fn is_next_identifier(&self) -> bool
    {
        match self.tokens.front()
        {
            Some(tok) => if let TokenKind::Identifier(_) = tok.kind {true} else {false},
            None => false,
        }
    }

    pub fn next_indent(&self) -> Option<usize>
    {
        match self.tokens.front()
        {
            Some(tok) => if let TokenKind::Indent(lvl) = tok.kind {Some(lvl)} else {None},
            _ => None,
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
