use std::io::{Read, BufReader, BufRead};
use std::mem;
use std::collections::VecDeque;
use compileerror::CompileError;

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
    Identifier(String),
    Colon,
    Comma,
    OpenParen,
    CloseParen,
    Number(String),
    StringLiteral(String),
    Indent(usize),
    Operator(Operator),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Token
{
    pub kind: TokenKind,
    pub line: usize,
    pub offset: usize,
}

impl Token
{
    fn new(kind: TokenKind, line: usize, offset: usize) -> Token
    {
        Token{
            kind: kind,
            line: line,
            offset: offset,
        }
    }
}


#[derive(Debug, Clone, Eq, PartialEq)]
enum LexState
{
    StartOfLine,
    Idle,
    Comment,
    Identifier,
    Number,
    Operator,
    InString,
}

pub struct Lexer
{
    state: LexState,
    tokens: VecDeque<Token>,
    line: usize,
    offset: usize,
    data: String,
    escape_code: bool,
}

fn is_operator_start(c: char) -> bool
{
    for &op in ['+', '-', '*', '/', '%', '>', '<', '=', '!', '.', '|', '&'].iter()
    {
        if op == c {return true;}
    }

    false
}

fn is_identifier_start(c: char) -> bool
{
     c.is_alphanumeric() || c == '_'
}

impl Lexer
{
    pub fn new() -> Lexer
    {
        Lexer {
            state: LexState::StartOfLine,
            tokens: VecDeque::new(),
            line: 1,
            offset: 1,
            data: String::new(),
            escape_code: false,
        }
    }

    fn add(&mut self, tok: TokenKind)
    {
        self.tokens.push_back(Token::new(tok, self.line, self.offset));
    }


    fn start(&mut self, c: char, ns: LexState)
    {
        self.state = ns;
        self.data.clear();
        if self.state != LexState::InString {self.data.push(c);}
    }

    fn start_of_line(&mut self, c: char) -> Result<(), CompileError>
    {
        match c
        {
            ' ' | '\t' | '\n' => Ok(()),
            _ => {
                let offset = self.offset;
                self.add(TokenKind::Indent(offset));
                self.state = LexState::Idle;
                self.idle(c)
            },
        }
    }


    fn idle(&mut self, c: char) -> Result<(), CompileError>
    {
        match c
        {
            '\n' => {self.state = LexState::StartOfLine; Ok(())},
            ' ' | '\t' => Ok(()),
            '#' => {self.state = LexState::Comment; Ok(())},
            ':' => {self.add(TokenKind::Colon); Ok(())},
            ',' => {self.add(TokenKind::Comma); Ok(())},
            '(' => {self.add(TokenKind::OpenParen); Ok(())},
            ')' => {self.add(TokenKind::CloseParen); Ok(())},
            '0'...'9' => {self.start(c, LexState::Number); Ok(())},
            '\"' => {self.start(c, LexState::InString); Ok(())},
            ch if is_identifier_start(ch) => {self.start(c, LexState::Identifier); Ok(())},
            ch if is_operator_start(ch) => {self.start(c, LexState::Operator); Ok(())}
            _ => {
                Err(CompileError::new(self.line, self.offset + 1, &format!("Unexpected token {}", c)))
            }
        }
    }

    fn comment(&mut self, c: char) -> Result<(), CompileError>
    {
        if c == '\n' {self.state = LexState::StartOfLine;}
        Ok(())
    }

    fn add_identifier(&mut self) -> TokenKind
    {
        let tok = match &self.data[..]
        {
            "import" => TokenKind::Import,
            "var" => TokenKind::Var,
            "const" => TokenKind::Const,
            "for" => TokenKind::For,
            "while" => TokenKind::While,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "func" => TokenKind::Func,
            "return" => TokenKind::Return,
            _ => TokenKind::Identifier(mem::replace(&mut self.data, String::new())),
        };

        self.data.clear();
        tok
    }

    fn identifier(&mut self, c: char) -> Result<(), CompileError>
    {
        if is_identifier_start(c)
        {
            self.data.push(c);
            Ok(())
        }
        else
        {
            self.state = LexState::Idle;
            let tok = self.add_identifier();
            self.add(tok);
            self.idle(c)
        }
    }

    fn number(&mut self, c: char) -> Result<(), CompileError>
    {
        if c.is_numeric()
        {
            self.data.push(c);
            Ok(())
        }
        else
        {
            self.state = LexState::Idle;
            let num = mem::replace(&mut self.data, String::new());
            self.add(TokenKind::Number(num));
            self.idle(c)
        }
    }

    fn data_to_operator(&self) -> Result<Operator, CompileError>
    {
        match &self.data[..]
        {
            "+" => Ok(Operator::Add),
            "-" => Ok(Operator::Sub),
            "*" => Ok(Operator::Mul),
            "/" => Ok(Operator::Div),
            "%" => Ok(Operator::Mod),
            ">" => Ok(Operator::GreaterThan),
            ">=" => Ok(Operator::GreaterThanEquals),
            "<" => Ok(Operator::LessThan),
            "<=" => Ok(Operator::LessThanEquals),
            "=" => Ok(Operator::Assign),
            "==" => Ok(Operator::Equals),
            "!" => Ok(Operator::Not),
            "!=" => Ok(Operator::NotEquals),
            "&&" => Ok(Operator::And),
            "||" => Ok(Operator::Or),
            "->" => Ok(Operator::Arrow),
            ".." => Ok(Operator::Range),
            "--" => Ok(Operator::Decrement),
            "++" => Ok(Operator::Increment),
            _ => Err(CompileError::new(self.line, self.offset, &format!("Invalid operator {}", self.data))),
        }
    }

    fn operator(&mut self, c: char) -> Result<(), CompileError>
    {
        if c.is_whitespace() || c.is_alphanumeric()
        {
            let op = try!(self.data_to_operator());
            self.state = LexState::Idle;
            self.add(TokenKind::Operator(op));
            self.idle(c)
        }
        else
        {
            self.data.push(c);
            Ok(())
        }
    }

    fn in_string(&mut self, c: char) -> Result<(), CompileError>
    {
        if self.escape_code
        {
            self.escape_code = false;
            match c
            {
                'r' => self.data.push('\r'),
                'n' => self.data.push('\n'),
                't' => self.data.push('\n'),
                _   => self.data.push(c),
            }

            Ok(())
        }
        else if c == '\\'
        {
            self.escape_code = true;
            Ok(())
        }
        else if c == '"'
        {
            let s = mem::replace(&mut self.data, String::new());
            self.add(TokenKind::StringLiteral(s));
            self.escape_code = false;
            self.state = LexState::Idle;
            Ok(())
        }
        else
        {
            self.data.push(c);
            Ok(())
        }
    }

    fn feed(&mut self, c: char) -> Result<(), CompileError>
    {
        match self.state
        {
            LexState::StartOfLine => self.start_of_line(c),
            LexState::Idle => self.idle(c),
            LexState::Comment => self.comment(c),
            LexState::Identifier => self.identifier(c),
            LexState::Number =>  self.number(c),
            LexState::Operator => self.operator(c),
            LexState::InString => self.in_string(c),
        }
    }

    pub fn read<Input: Read>(&mut self, input: &mut Input) -> Result<(), CompileError>
    {
        for line in BufReader::new(input).lines()
        {
            for c in try!(line).chars()
            {
                try!(self.feed(c));
                self.offset += 1;
            }

            try!(self.feed('\n'));
            self.offset = 1;
            self.line += 1;
        }

        Ok(())
    }

    pub fn dump(&self)
    {
        for tok in &self.tokens
        {
            println!("{:?}", tok);
        }
    }
}

impl Iterator for Lexer
{
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item>
    {
        self.tokens.pop_front()
    }
}



#[cfg(test)]
mod tests
{
    use super::*;
    use std::io::Cursor;

    #[test]
    fn test_keywords()
    {
        let mut cursor = Cursor::new("import var const func if else while for return");
        let mut lexer = Lexer::new();
        assert!(lexer.read(&mut cursor).is_ok());

        let tokens = lexer.map(|tok| tok.kind).collect::<Vec<TokenKind>>();
        assert_eq!(tokens, vec![
            TokenKind::Indent(1),
            TokenKind::Import,
            TokenKind::Var,
            TokenKind::Const,
            TokenKind::Func,
            TokenKind::If,
            TokenKind::Else,
            TokenKind::While,
            TokenKind::For,
            TokenKind::Return,
        ]);
    }

    #[test]
    fn test_identifiers_and_numbers()
    {
        let mut cursor = Cursor::new("blaat 8888 _foo_16");
        let mut lexer = Lexer::new();
        assert!(lexer.read(&mut cursor).is_ok());

        let tokens = lexer.map(|tok| tok.kind).collect::<Vec<TokenKind>>();
        assert_eq!(tokens, vec![
            TokenKind::Indent(1),
            TokenKind::Identifier("blaat".into()),
            TokenKind::Number("8888".into()),
            TokenKind::Identifier("_foo_16".into()),
        ]);
    }

    #[test]
    fn test_operators()
    {
        let mut cursor = Cursor::new("++ -- + - * / % < <= > >= == = != ! || && .. -> : ,");
        let mut lexer = Lexer::new();
        assert!(lexer.read(&mut cursor).is_ok());

        let tokens = lexer.map(|tok| tok.kind).collect::<Vec<TokenKind>>();
        assert_eq!(tokens, vec![
            TokenKind::Indent(1),
            TokenKind::Operator(Operator::Increment),
            TokenKind::Operator(Operator::Decrement),
            TokenKind::Operator(Operator::Add),
            TokenKind::Operator(Operator::Sub),
            TokenKind::Operator(Operator::Mul),
            TokenKind::Operator(Operator::Div),
            TokenKind::Operator(Operator::Mod),
            TokenKind::Operator(Operator::LessThan),
            TokenKind::Operator(Operator::LessThanEquals),
            TokenKind::Operator(Operator::GreaterThan),
            TokenKind::Operator(Operator::GreaterThanEquals),
            TokenKind::Operator(Operator::Equals),
            TokenKind::Operator(Operator::Assign),
            TokenKind::Operator(Operator::NotEquals),
            TokenKind::Operator(Operator::Not),
            TokenKind::Operator(Operator::Or),
            TokenKind::Operator(Operator::And),
            TokenKind::Operator(Operator::Range),
            TokenKind::Operator(Operator::Arrow),
            TokenKind::Colon,
            TokenKind::Comma,
        ]);
    }

    #[test]
    fn test_string()
    {
        let mut cursor = Cursor::new(r#""This is a string" "Blaat\n" "$a""#);
        let mut lexer = Lexer::new();
        assert!(lexer.read(&mut cursor).is_ok());

        let tokens = lexer.map(|tok| tok.kind).collect::<Vec<TokenKind>>();
        assert_eq!(tokens, vec![
            TokenKind::Indent(1),
            TokenKind::StringLiteral("This is a string".into()),
            TokenKind::StringLiteral("Blaat\n".into()),
            TokenKind::StringLiteral("$a".into()),
        ]);
    }
}
