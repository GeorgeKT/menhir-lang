use std::collections::VecDeque;
use compileerror::{Pos, CompileError};
use tokens::{Token, TokenKind};



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
            .ok_or(CompileError::new(self.last_pos, format!("Unexpected end of file")))
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

    pub fn expect(&mut self, kind: TokenKind) -> Result<Token, CompileError>
    {
        self.pop()
            .and_then(|tok| if tok.kind == kind
                {
                    Ok(tok)
                }
                else
                {
                    Err(CompileError::new(tok.pos, format!("Invalid token {}, expecting {}", tok.kind, kind)))
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
            Err(CompileError::new(tok.pos, format!("Invalid token {}, expecting indentation", tok.kind)))
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
            Err(CompileError::new(tok.pos, format!("Invalid token {}, expecting string litteral", tok.kind)))
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
            Err(CompileError::new(tok.pos, format!("Invalid token {}, expecting identifier", tok.kind)))
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

    pub fn is_next_indent(&self) -> bool
    {
        match self.tokens.front()
        {
            Some(tok) => if let TokenKind::Indent(_) = tok.kind {true} else {false},
            _ => false,
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
