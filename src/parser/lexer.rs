use std::io::{Read, BufReader, BufRead};
use std::mem;
use compileerror::{CompileResult, ErrorCode, err};
use parser::{TokenQueue, TokenKind, Operator, Token};
use span::{Span, Pos};


#[derive(Debug, Clone, Eq, PartialEq)]
enum LexState
{
    Idle,
    Comment,
    Identifier,
    Number,
    Operator,
    InString,
    InChar,
}

pub struct Lexer
{
    state: LexState,
    tokens: TokenQueue,
    pos: Pos,
    token_start_pos: Pos,
    data: String,
    escape_code: bool,
    file_name: String,
}

fn is_operator_start(c: char) -> bool
{
    for &op in ['+', '-', '*', '/', '%', '>', '<', '=', '!', '.', '|', '&', ':'].iter()
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
    pub fn new(file_name: &str) -> Lexer
    {
        Lexer {
            state: LexState::Idle,
            tokens: TokenQueue::new(),
            pos: Pos::new(1, 1),
            token_start_pos: Pos::new(1, 1),
            data: String::new(),
            escape_code: false,
            file_name: file_name.into(),
        }
    }

    fn start(&mut self, c: char, ns: LexState)
    {
        self.token_start_pos = self.pos;
        self.state = ns;
        self.data.clear();
        if self.state != LexState::InString && self.state != LexState::InChar {
            self.data.push(c);
        }
    }

    fn idle(&mut self, c: char) -> CompileResult<()>
    {
        let span = self.current_single_span();
        match c
        {
            '\n' | ' ' | '\t' => Ok(()),
            '#' => {self.state = LexState::Comment; Ok(())},
            ',' => {self.add(TokenKind::Comma, span); Ok(())},
            '(' => {self.add(TokenKind::OpenParen, span); Ok(())},
            ')' => {self.add(TokenKind::CloseParen, span); Ok(())},
            '{' => {self.add(TokenKind::OpenCurly, span); Ok(())},
            '}' => {self.add(TokenKind::CloseCurly, span); Ok(())},
            '[' => {self.add(TokenKind::OpenBracket, span); Ok(())},
            ']' => {self.add(TokenKind::CloseBracket, span); Ok(())},
            '@' => {self.add(TokenKind::Lambda, span); Ok(())},
            '$' => {self.add(TokenKind::Dollar, span); Ok(())},
            ';' => {self.add(TokenKind::SemiColon, span); Ok(())},
            '0'...'9' => {self.start(c, LexState::Number); Ok(())},
            '\"' => {self.start(c, LexState::InString); Ok(())},
            '\'' => {self.start(c, LexState::InChar); Ok(())},
            ch if is_identifier_start(ch) => {self.start(c, LexState::Identifier); Ok(())},
            ch if is_operator_start(ch) => {self.start(c, LexState::Operator); Ok(())}
            _ => {
                err(&span, ErrorCode::UnexpectedChar, format!("Unexpected char {}", c))
            }
        }
    }

    fn comment(&mut self, c: char) -> CompileResult<()>
    {
        if c == '\n' {self.state = LexState::Idle;}
        Ok(())
    }

    fn add_identifier(&mut self) -> TokenKind
    {
        let tok = match &self.data[..]
        {
            "import" => TokenKind::Import,
            "match" => TokenKind::Match,
            "let" => TokenKind::Let,
            "in" => TokenKind::In,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "type" => TokenKind::Type,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "extern" => TokenKind::Extern,
            _ => TokenKind::Identifier(mem::replace(&mut self.data, String::new())),
        };

        self.data.clear();
        tok
    }

    fn current_span(&self) -> Span
    {
        Span::new(&self.file_name, self.token_start_pos, Pos::new(self.pos.line, self.pos.offset - 1))
    }

    fn current_single_span(&self) -> Span
    {
        Span::single(&self.file_name, self.pos.clone())
    }

    fn identifier(&mut self, c: char) -> CompileResult<()>
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

    fn number(&mut self, c: char) -> CompileResult<()>
    {
        if c.is_numeric() || c == '.' || c == 'e'
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

    fn data_to_token_kind(&self) -> CompileResult<TokenKind>
    {
        match &self.data[..]
        {
            "+" => Ok(TokenKind::Operator(Operator::Add)),
            "-" => Ok(TokenKind::Operator(Operator::Sub)),
            "*" => Ok(TokenKind::Operator(Operator::Mul)),
            "/" => Ok(TokenKind::Operator(Operator::Div)),
            "%" => Ok(TokenKind::Operator(Operator::Mod)),
            ">" => Ok(TokenKind::Operator(Operator::GreaterThan)),
            ">=" => Ok(TokenKind::Operator(Operator::GreaterThanEquals)),
            "<" => Ok(TokenKind::Operator(Operator::LessThan)),
            "<=" => Ok(TokenKind::Operator(Operator::LessThanEquals)),
            "=" => Ok(TokenKind::Assign),
            "==" => Ok(TokenKind::Operator(Operator::Equals)),
            "!" => Ok(TokenKind::Operator(Operator::Not)),
            "!=" => Ok(TokenKind::Operator(Operator::NotEquals)),
            "&&" => Ok(TokenKind::Operator(Operator::And)),
            "||" => Ok(TokenKind::Operator(Operator::Or)),
            "->" => Ok(TokenKind::Arrow),
            "=>" => Ok(TokenKind::FatArrow),
            ":" => Ok(TokenKind::Colon),
            "::" => Ok(TokenKind::DoubleColon),
            "|" => Ok(TokenKind::Pipe),
            "<-" => Ok(TokenKind::Operator(Operator::Extract)),
            "." => Ok(TokenKind::Operator(Operator::Dot)),
            _ => err(&self.current_single_span(), ErrorCode::InvalidOperator, format!("Invalid operator {}", self.data)),
        }
    }

    fn operator(&mut self, c: char) -> CompileResult<()>
    {
        if c.is_whitespace() || c.is_alphanumeric() ||
            c == '{' || c == '(' || c == '[' ||
            c == '}' || c == ')' || c == ']' ||
            c == '$' || c == ',' || c == '_'
        {
            let kind = try!(self.data_to_token_kind());
            self.state = LexState::Idle;
            let span = self.current_span();
            self.add(kind, span);
            self.idle(c)
        }
        else
        {
            self.data.push(c);
            Ok(())
        }
    }

    fn in_string_or_char_literal(&mut self, c: char, end: char)
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
        }
        else if c == '\\'
        {
            self.escape_code = true;
        }
        else if c != end
        {
            self.data.push(c);
        }
    }

    fn in_string(&mut self, c: char) -> CompileResult<()>
    {
        self.in_string_or_char_literal(c, '"');
        if c == '"'
        {
            let s = mem::replace(&mut self.data, String::new());
            let mut span = self.current_span();
            span.end.offset += 1; // Need to include the quote
            self.add(TokenKind::StringLiteral(s), span);
            self.escape_code = false;
            self.state = LexState::Idle;

        }
        Ok(())
    }

    fn in_char(&mut self, c: char) -> CompileResult<()>
    {
        self.in_string_or_char_literal(c, '\'');
        if c == '\''
        {
            let mut span = self.current_span();
            span.end.offset += 1; // Need to include the single quote
            if self.data.len() != 1 {
                return err(&span, ErrorCode::InvalidCharLiteral, format!("Invalid char literal"));
            }

            let c = self.data.chars().nth(0).expect("Invalid char literal");
            self.data.clear();
            self.add(TokenKind::CharLiteral(c), span);
            self.escape_code = false;
            self.state = LexState::Idle;
        }

        Ok(())
    }

    fn feed(&mut self, c: char) -> CompileResult<()>
    {
        match self.state
        {
            LexState::Idle => self.idle(c),
            LexState::Comment => self.comment(c),
            LexState::Identifier => self.identifier(c),
            LexState::Number =>  self.number(c),
            LexState::Operator => self.operator(c),
            LexState::InString => self.in_string(c),
            LexState::InChar => self.in_char(c),
        }
    }

    fn add(&mut self, tok: TokenKind, span: Span)
    {
        self.tokens.add(Token::new(tok, span));
    }

    pub fn read<Input: Read>(&mut self, input: &mut Input) -> CompileResult<TokenQueue>
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

        let span = self.current_single_span();
        self.add(TokenKind::EOF, span);
        //self.tokens.dump();
        Ok(mem::replace(&mut self.tokens, TokenQueue::new()))
    }
}





#[cfg(test)]
mod tests
{
    use std::io::Cursor;
    use parser::*;
    use span::*;

    fn tok(kind: TokenKind, sline: usize, soffset: usize, eline: usize, eoffset: usize) -> Token
    {
        Token::new(kind, Span::new("", Pos::new(sline, soffset), Pos::new(eline, eoffset)))
    }

    #[test]
    fn test_keywords()
    {
        let mut cursor = Cursor::new("import match");
        let mut lexer = Lexer::new("");
        let tokens: Vec<Token> = lexer
            .read(&mut cursor)
            .expect("Lexing failed")
            .collect();

        assert_eq!(tokens, vec![
            tok(TokenKind::Import, 1, 1, 1, 6),
            tok(TokenKind::Match, 1, 8, 1, 12),
            tok(TokenKind::EOF, 2, 1, 2, 1),
        ]);
    }

    #[test]
    fn test_identifiers_and_numbers()
    {
        let mut cursor = Cursor::new("blaat 8888 _foo_16");
        let tokens: Vec<Token> = Lexer::new("")
            .read(&mut cursor)
            .expect("Lexing failed")
            .collect();

        assert_eq!(tokens, vec![
            tok(TokenKind::Identifier("blaat".into()), 1, 1, 1, 5),
            tok(TokenKind::Number("8888".into()), 1, 7, 1, 10),
            tok(TokenKind::Identifier("_foo_16".into()), 1, 12, 1, 18),
            tok(TokenKind::EOF, 2, 1, 2, 1),
        ]);
    }

    #[test]
    fn test_specials()
    {
        let mut cursor = Cursor::new("+ - * / % < <= > >= == = != ! || && => -> : , :: $ @");
        let tokens: Vec<Token> = Lexer::new("")
            .read(&mut cursor)
            .expect("Lexing failed")
            .collect();

        assert_eq!(tokens, vec![
            tok(TokenKind::Operator(Operator::Add), 1, 1, 1, 1),
            tok(TokenKind::Operator(Operator::Sub), 1, 3, 1, 3),
            tok(TokenKind::Operator(Operator::Mul), 1, 5, 1, 5),
            tok(TokenKind::Operator(Operator::Div), 1, 7, 1, 7),
            tok(TokenKind::Operator(Operator::Mod), 1, 9, 1, 9),
            tok(TokenKind::Operator(Operator::LessThan), 1, 11, 1, 11),
            tok(TokenKind::Operator(Operator::LessThanEquals), 1, 13, 1, 14),
            tok(TokenKind::Operator(Operator::GreaterThan), 1, 16, 1, 16),
            tok(TokenKind::Operator(Operator::GreaterThanEquals), 1, 18, 1, 19),
            tok(TokenKind::Operator(Operator::Equals), 1, 21, 1, 22),
            tok(TokenKind::Assign, 1, 24, 1, 24),
            tok(TokenKind::Operator(Operator::NotEquals), 1, 26, 1, 27),
            tok(TokenKind::Operator(Operator::Not), 1, 29, 1, 29),
            tok(TokenKind::Operator(Operator::Or), 1, 31, 1, 32),
            tok(TokenKind::Operator(Operator::And), 1, 34, 1, 35),
            tok(TokenKind::FatArrow, 1, 37, 1, 38),
            tok(TokenKind::Arrow, 1, 40, 1, 41),
            tok(TokenKind::Colon, 1, 43, 1, 43),
            tok(TokenKind::Comma, 1, 45, 1, 45),
            tok(TokenKind::DoubleColon, 1, 47, 1, 48),
            tok(TokenKind::Dollar, 1, 50, 1, 50),
            tok(TokenKind::Lambda, 1, 52, 1, 52),
            tok(TokenKind::EOF, 2, 1, 2, 1),
        ]);
    }

    #[test]
    fn test_string()
    {
        let mut cursor = Cursor::new(r#""This is a string" "Blaat\n" "$a""#);
        let tokens: Vec<Token> = Lexer::new("")
            .read(&mut cursor)
            .expect("Lexing failed")
            .collect();

        assert_eq!(tokens, vec![
            tok(TokenKind::StringLiteral("This is a string".into()), 1, 1, 1, 18),
            tok(TokenKind::StringLiteral("Blaat\n".into()), 1, 20, 1, 28),
            tok(TokenKind::StringLiteral("$a".into()), 1, 30, 1, 33),
            tok(TokenKind::EOF, 2, 1, 2, 1),
        ]);
    }
}
