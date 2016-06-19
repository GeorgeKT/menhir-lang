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
pub enum Token
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
                self.tokens.push_back(Token::Indent(self.offset));
                self.state = LexState::Idle;
                self.idle(c)
            },
        }
    }

    fn add(&mut self, tok: Token) -> Result<(), CompileError>
    {
        self.tokens.push_back(tok);
        Ok(())
    }

    fn idle(&mut self, c: char) -> Result<(), CompileError>
    {
        match c
        {
            '\n' => {self.state = LexState::StartOfLine; Ok(())},
            ' ' | '\t' => Ok(()),
            '#' => {self.state = LexState::Comment; Ok(())},
            ':' => self.add(Token::Colon),
            ',' => self.add(Token::Comma),
            '(' => self.add(Token::OpenParen),
            ')' => self.add(Token::CloseParen),
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

    fn add_identifier(&mut self) -> Token
    {
        let tok = match &self.data[..]
        {
            "import" => Token::Import,
            "var" => Token::Var,
            "const" => Token::Const,
            "for" => Token::For,
            "while" => Token::While,
            "if" => Token::If,
            "else" => Token::Else,
            "func" => Token::Func,
            "return" => Token::Return,
            _ => Token::Identifier(mem::replace(&mut self.data, String::new())),
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
            self.tokens.push_back(tok);
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
            self.tokens.push_back(Token::Number(mem::replace(&mut self.data, String::new())));
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
            self.tokens.push_back(Token::Operator(op));
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
            self.tokens.push_back(Token::StringLiteral(mem::replace(&mut self.data, String::new())));
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

        let tokens = lexer.collect::<Vec<Token>>();
        assert_eq!(tokens, vec![
            Token::Indent(1),
            Token::Import,
            Token::Var,
            Token::Const,
            Token::Func,
            Token::If,
            Token::Else,
            Token::While,
            Token::For,
            Token::Return,
        ]);
    }

    #[test]
    fn test_identifiers_and_numbers()
    {
        let mut cursor = Cursor::new("blaat 8888 _foo_16");
        let mut lexer = Lexer::new();
        assert!(lexer.read(&mut cursor).is_ok());

        let tokens = lexer.collect::<Vec<Token>>();
        assert_eq!(tokens, vec![
            Token::Indent(1),
            Token::Identifier("blaat".into()),
            Token::Number("8888".into()),
            Token::Identifier("_foo_16".into()),
        ]);
    }

    #[test]
    fn test_operators()
    {
        let mut cursor = Cursor::new("++ -- + - * / % < <= > >= == = != ! || && .. -> : ,");
        let mut lexer = Lexer::new();
        assert!(lexer.read(&mut cursor).is_ok());

        let tokens = lexer.collect::<Vec<Token>>();
        assert_eq!(tokens, vec![
            Token::Indent(1),
            Token::Operator(Operator::Increment),
            Token::Operator(Operator::Decrement),
            Token::Operator(Operator::Add),
            Token::Operator(Operator::Sub),
            Token::Operator(Operator::Mul),
            Token::Operator(Operator::Div),
            Token::Operator(Operator::Mod),
            Token::Operator(Operator::LessThan),
            Token::Operator(Operator::LessThanEquals),
            Token::Operator(Operator::GreaterThan),
            Token::Operator(Operator::GreaterThanEquals),
            Token::Operator(Operator::Equals),
            Token::Operator(Operator::Assign),
            Token::Operator(Operator::NotEquals),
            Token::Operator(Operator::Not),
            Token::Operator(Operator::Or),
            Token::Operator(Operator::And),
            Token::Operator(Operator::Range),
            Token::Operator(Operator::Arrow),
            Token::Colon,
            Token::Comma,
        ]);
    }

    #[test]
    fn test_string()
    {
        let mut cursor = Cursor::new(r#""This is a string" "Blaat\n" "$a""#);
        let mut lexer = Lexer::new();
        assert!(lexer.read(&mut cursor).is_ok());

        let tokens = lexer.collect::<Vec<Token>>();
        assert_eq!(tokens, vec![
            Token::Indent(1),
            Token::StringLiteral("This is a string".into()),
            Token::StringLiteral("Blaat\n".into()),
            Token::StringLiteral("$a".into()),
        ]);
    }
}
