use std::io::{Read, BufReader, BufRead};
use std::mem;
use compileerror::{Pos, Span, CompileError, ErrorType};
use tokens::*;
use tokenqueue::TokenQueue;


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
    tokens: TokenQueue,
    pos: Pos,
    token_start_pos: Pos,
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
            tokens: TokenQueue::new(),
            pos: Pos::new(1, 1),
            token_start_pos: Pos::new(1, 1),
            data: String::new(),
            escape_code: false,
        }
    }



    fn start(&mut self, c: char, ns: LexState)
    {
        self.token_start_pos = self.pos;
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
                let offset = self.pos.offset;
                let pos = self.pos;
                self.add(TokenKind::Indent(offset), Span::new(pos, pos));
                self.state = LexState::Idle;
                self.idle(c)
            },
        }
    }


    fn idle(&mut self, c: char) -> Result<(), CompileError>
    {
        let pos = self.pos;
        match c
        {
            '\n' => {self.state = LexState::StartOfLine; Ok(())},
            ' ' | '\t' => Ok(()),
            '#' => {self.state = LexState::Comment; Ok(())},
            ':' => {self.add(TokenKind::Colon, Span::single(pos)); Ok(())},
            ',' => {self.add(TokenKind::Comma, Span::single(pos)); Ok(())},
            '(' => {self.add(TokenKind::OpenParen, Span::single(pos)); Ok(())},
            ')' => {self.add(TokenKind::CloseParen, Span::single(pos)); Ok(())},
            '0'...'9' => {self.start(c, LexState::Number); Ok(())},
            '\"' => {self.start(c, LexState::InString); Ok(())},
            ch if is_identifier_start(ch) => {self.start(c, LexState::Identifier); Ok(())},
            ch if is_operator_start(ch) => {self.start(c, LexState::Operator); Ok(())}
            _ => {
                Err(CompileError::new(self.pos, ErrorType::UnexpectedChar(c)))
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
            "pub" => TokenKind::Pub,
            "struct" => TokenKind::Struct,
            "in" => TokenKind::In,
            "match" => TokenKind::Match,
            "union" => TokenKind::Union,
            "extern" => TokenKind::Extern,
            _ => TokenKind::Identifier(mem::replace(&mut self.data, String::new())),
        };

        self.data.clear();
        tok
    }

    fn current_span(&self) -> Span
    {
        Span::new(self.token_start_pos, Pos::new(self.pos.line, self.pos.offset - 1))
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
            let span = self.current_span();
            let tok = self.add_identifier();
            self.add(tok, span);
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
            let span = self.current_span();
            let num = mem::replace(&mut self.data, String::new());
            self.add(TokenKind::Number(num), span);
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
            "+=" => Ok(Operator::AddAssign),
            "-=" => Ok(Operator::SubAssign),
            "*=" => Ok(Operator::MulAssign),
            "/=" => Ok(Operator::DivAssign),
            "." => Ok(Operator::Dot),
            _ => Err(CompileError::new(self.pos, ErrorType::InvalidOperator(self.data.clone()))),
        }
    }

    fn operator(&mut self, c: char) -> Result<(), CompileError>
    {
        if c.is_whitespace() || c.is_alphanumeric()
        {
            let op = try!(self.data_to_operator());
            self.state = LexState::Idle;
            let span = self.current_span();
            self.add(TokenKind::Operator(op), span);
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
            let mut span = self.current_span();
            span.end.offset += 1; // Need to include the quote
            self.add(TokenKind::StringLiteral(s), span);
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

    fn add(&mut self, tok: TokenKind, span: Span)
    {
        self.tokens.add(Token::new(tok, span));
    }

    pub fn read<Input: Read>(&mut self, input: &mut Input) -> Result<TokenQueue, CompileError>
    {
        for line in BufReader::new(input).lines()
        {
            for c in try!(line).chars()
            {
                try!(self.feed(c));
                self.pos.offset += 1;
            }

            try!(self.feed('\n'));
            self.pos.offset = 1;
            self.pos.line += 1;
        }

        let pos = self.pos;
        self.add(TokenKind::EOF, Span::single(pos));
        //self.tokens.dump();
        Ok(mem::replace(&mut self.tokens, TokenQueue::new()))
    }
}





#[cfg(test)]
mod tests
{
    use super::*;
    use std::io::Cursor;
    use tokens::*;
    use compileerror::*;

    fn tok(kind: TokenKind, sline: usize, soffset: usize, eline: usize, eoffset: usize) -> Token
    {
        Token::new(kind, span(sline, soffset, eline, eoffset))
    }

    #[test]
    fn test_keywords()
    {
        let mut cursor = Cursor::new("import var const func if else while for return pub struct in match union extern");
        let mut lexer = Lexer::new();
        let tokens: Vec<Token> = lexer
            .read(&mut cursor)
            .expect("Lexing failed")
            .collect();

        assert_eq!(tokens, vec![
            tok(TokenKind::Indent(1), 1, 1, 1, 1),
            tok(TokenKind::Import, 1, 1, 1, 6),
            tok(TokenKind::Var, 1, 8, 1, 10),
            tok(TokenKind::Const, 1, 12, 1, 16),
            tok(TokenKind::Func, 1, 18, 1, 21),
            tok(TokenKind::If, 1, 23, 1, 24),
            tok(TokenKind::Else, 1, 26, 1, 29),
            tok(TokenKind::While, 1, 31, 1, 35),
            tok(TokenKind::For, 1, 37, 1, 39),
            tok(TokenKind::Return, 1, 41, 1, 46),
            tok(TokenKind::Pub, 1, 48, 1, 50),
            tok(TokenKind::Struct, 1, 52, 1, 57),
            tok(TokenKind::In, 1, 59, 1, 60),
            tok(TokenKind::Match, 1, 62, 1, 66),
            tok(TokenKind::Union, 1, 68, 1, 72),
            tok(TokenKind::Extern, 1, 74, 1, 79),
            tok(TokenKind::EOF, 2, 1, 2, 1),
        ]);
    }

    #[test]
    fn test_identifiers_and_numbers()
    {
        let mut cursor = Cursor::new("blaat 8888 _foo_16");
        let tokens: Vec<Token> = Lexer::new()
            .read(&mut cursor)
            .expect("Lexing failed")
            .collect();

        assert_eq!(tokens, vec![
            tok(TokenKind::Indent(1), 1, 1, 1, 1),
            tok(TokenKind::Identifier("blaat".into()), 1, 1, 1, 5),
            tok(TokenKind::Number("8888".into()), 1, 7, 1, 10),
            tok(TokenKind::Identifier("_foo_16".into()), 1, 12, 1, 18),
            tok(TokenKind::EOF, 2, 1, 2, 1),
        ]);
    }

    #[test]
    fn test_operators()
    {
        let mut cursor = Cursor::new("++ -- + - * / % < <= > >= == = != ! || && .. -> += -= *= /= . : ,");
        let tokens: Vec<Token> = Lexer::new()
            .read(&mut cursor)
            .expect("Lexing failed")
            .collect();

        assert_eq!(tokens, vec![
            tok(TokenKind::Indent(1), 1, 1, 1, 1),
            tok(TokenKind::Operator(Operator::Increment), 1, 1, 1, 2),
            tok(TokenKind::Operator(Operator::Decrement), 1, 4, 1, 5),
            tok(TokenKind::Operator(Operator::Add), 1, 7, 1, 7),
            tok(TokenKind::Operator(Operator::Sub), 1, 9, 1, 9),
            tok(TokenKind::Operator(Operator::Mul), 1, 11, 1, 11),
            tok(TokenKind::Operator(Operator::Div), 1, 13, 1, 13),
            tok(TokenKind::Operator(Operator::Mod), 1, 15, 1, 15),
            tok(TokenKind::Operator(Operator::LessThan), 1, 17, 1, 17),
            tok(TokenKind::Operator(Operator::LessThanEquals), 1, 19, 1, 20),
            tok(TokenKind::Operator(Operator::GreaterThan), 1, 22, 1, 22),
            tok(TokenKind::Operator(Operator::GreaterThanEquals), 1, 24, 1, 25),
            tok(TokenKind::Operator(Operator::Equals), 1, 27, 1, 28),
            tok(TokenKind::Operator(Operator::Assign), 1, 30, 1, 30),
            tok(TokenKind::Operator(Operator::NotEquals), 1, 32, 1, 33),
            tok(TokenKind::Operator(Operator::Not), 1, 35, 1, 35),
            tok(TokenKind::Operator(Operator::Or), 1, 37, 1, 38),
            tok(TokenKind::Operator(Operator::And), 1, 40, 1, 41),
            tok(TokenKind::Operator(Operator::Range), 1, 43, 1, 44),
            tok(TokenKind::Operator(Operator::Arrow), 1, 46, 1, 47),
            tok(TokenKind::Operator(Operator::AddAssign), 1, 49, 1, 50),
            tok(TokenKind::Operator(Operator::SubAssign), 1, 52, 1, 53),
            tok(TokenKind::Operator(Operator::MulAssign), 1, 55, 1, 56),
            tok(TokenKind::Operator(Operator::DivAssign), 1, 58, 1, 59),
            tok(TokenKind::Operator(Operator::Dot), 1, 61, 1, 61),
            tok(TokenKind::Colon, 1, 63, 1, 63),
            tok(TokenKind::Comma, 1, 65, 1, 65),
            tok(TokenKind::EOF, 2, 1, 2, 1),
        ]);
    }

    #[test]
    fn test_string()
    {
        let mut cursor = Cursor::new(r#""This is a string" "Blaat\n" "$a""#);
        let tokens: Vec<Token> = Lexer::new()
            .read(&mut cursor)
            .expect("Lexing failed")
            .collect();

        assert_eq!(tokens, vec![
            tok(TokenKind::Indent(1), 1, 1, 1, 1),
            tok(TokenKind::StringLiteral("This is a string".into()), 1, 1, 1, 18),
            tok(TokenKind::StringLiteral("Blaat\n".into()), 1, 20, 1, 28),
            tok(TokenKind::StringLiteral("$a".into()), 1, 30, 1, 33),
            tok(TokenKind::EOF, 2, 1, 2, 1),
        ]);
    }
}
