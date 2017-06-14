use std::io::{Read, BufReader, BufRead};
use std::mem;
use compileerror::{CompileResult, parse_error_result};
use super::tokenqueue::TokenQueue;
use super::tokens::{TokenKind, Token};
use ast::{BinaryOperator, UnaryOperator, AssignOperator};
use span::{Span, Pos};



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
    indent_level: usize,
}

fn is_operator_start(c: char) -> bool
{
    for op in &['+', '-', '*', '/', '%', '>', '<', '=', '!', '.', '|', '&', ':']
    {
        if *op == c {return true;}
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
            state: LexState::StartOfLine,
            tokens: TokenQueue::new(),
            pos: Pos::new(1, 1),
            token_start_pos: Pos::new(1, 1),
            data: String::new(),
            escape_code: false,
            file_name: file_name.into(),
            indent_level: 0,
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

    fn start_of_new_line(&mut self)
    {
        self.state = LexState::StartOfLine;
        self.indent_level = 0;
    }

    fn idle(&mut self, c: char) -> CompileResult<()>
    {
        let span = self.current_single_span();
        match c
        {
            '\n' => {
                self.start_of_new_line();
                Ok(())
            }
            ' ' | '\t' => Ok(()),
            '#' => {self.state = LexState::Comment; Ok(())},
            ',' => {self.add(TokenKind::Comma, span); Ok(())},
            '(' => {self.add(TokenKind::OpenParen, span); Ok(())},
            ')' => {self.add(TokenKind::CloseParen, span); Ok(())},
            '{' => {self.add(TokenKind::OpenCurly, span); Ok(())},
            '}' => {self.add(TokenKind::CloseCurly, span); Ok(())},
            '[' => {self.add(TokenKind::OpenBracket, span); Ok(())},
            ']' => {self.add(TokenKind::CloseBracket, span); Ok(())},
            '$' => {self.add(TokenKind::Dollar, span); Ok(())},
            ';' => {self.add(TokenKind::SemiColon, span); Ok(())},
            '~' => {self.add(TokenKind::Tilde, span); Ok(())},
            '?' => {self.add(TokenKind::QuestionMark, span); Ok(())},
            '@' => {self.add(TokenKind::At, span); Ok(())},
            '0'...'9' => {self.start(c, LexState::Number); Ok(())},
            '\"' => {self.start(c, LexState::InString); Ok(())},
            '\'' => {self.start(c, LexState::InChar); Ok(())},
            ch if is_identifier_start(ch) => {self.start(c, LexState::Identifier); Ok(())},
            ch if is_operator_start(ch) => {self.start(c, LexState::Operator); Ok(())}
            _ => {
                parse_error_result(&span, format!("Unexpected char {}", c))
            }
        }
    }

    fn comment(&mut self, c: char) -> CompileResult<()>
    {
        if c == '\n' {
            self.start_of_new_line();
        }
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
            "struct" => TokenKind::Struct,
            "enum" => TokenKind::Enum,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "extern" => TokenKind::Extern,
            "new" => TokenKind::New,
            "delete" => TokenKind::Delete,
            "while" => TokenKind::While,
            "for" => TokenKind::For,
            "nil" => TokenKind::Nil,
            "null" => TokenKind::Null,
            "var" => TokenKind::Var,
            "as" => TokenKind::BinaryOperator(BinaryOperator::As),
            "interface" => TokenKind::Interface,
            "fn" => TokenKind::Func,
            "return" => TokenKind::Return,
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
        Span::single(&self.file_name, self.pos)
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
            "+" => Ok(TokenKind::BinaryOperator(BinaryOperator::Add)),
            "-" => Ok(TokenKind::BinaryOperator(BinaryOperator::Sub)),
            "*" => Ok(TokenKind::BinaryOperator(BinaryOperator::Mul)),
            "/" => Ok(TokenKind::BinaryOperator(BinaryOperator::Div)),
            "%" => Ok(TokenKind::BinaryOperator(BinaryOperator::Mod)),
            ">" => Ok(TokenKind::BinaryOperator(BinaryOperator::GreaterThan)),
            ">=" => Ok(TokenKind::BinaryOperator(BinaryOperator::GreaterThanEquals)),
            "<" => Ok(TokenKind::BinaryOperator(BinaryOperator::LessThan)),
            "<=" => Ok(TokenKind::BinaryOperator(BinaryOperator::LessThanEquals)),
            "=" => Ok(TokenKind::Assign(AssignOperator::Assign)),
            "+=" => Ok(TokenKind::Assign(AssignOperator::Add)),
            "-=" => Ok(TokenKind::Assign(AssignOperator::Sub)),
            "*=" => Ok(TokenKind::Assign(AssignOperator::Mul)),
            "/=" => Ok(TokenKind::Assign(AssignOperator::Div)),
            "&&=" => Ok(TokenKind::Assign(AssignOperator::And)),
            "||=" => Ok(TokenKind::Assign(AssignOperator::Or)),
            "==" => Ok(TokenKind::BinaryOperator(BinaryOperator::Equals)),
            "!" => Ok(TokenKind::UnaryOperator(UnaryOperator::Not)),
            "!=" => Ok(TokenKind::BinaryOperator(BinaryOperator::NotEquals)),
            "&&" => Ok(TokenKind::BinaryOperator(BinaryOperator::And)),
            "||" => Ok(TokenKind::BinaryOperator(BinaryOperator::Or)),
            "->" => Ok(TokenKind::Arrow),
            "=>" => Ok(TokenKind::FatArrow),
            ":" => Ok(TokenKind::Colon),
            "::" => Ok(TokenKind::DoubleColon),
            "|" => Ok(TokenKind::Pipe),
            "." => Ok(TokenKind::BinaryOperator(BinaryOperator::Dot)),
            "&" => Ok(TokenKind::Ampersand),
            _ => parse_error_result(&self.current_single_span(), format!("Invalid operator {}", self.data)),
        }
    }

    fn operator(&mut self, c: char) -> CompileResult<()>
    {
        if c.is_whitespace() || c.is_alphanumeric() ||
            c == '{' || c == '(' || c == '[' ||
            c == '}' || c == ')' || c == ']' ||
            c == '$' || c == ',' || c == '_'
        {
            let kind = self.data_to_token_kind()?;
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
                't' => self.data.push('\t'),
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
                return parse_error_result(&span, "Invalid char literal");
            }

            let c = self.data.chars().nth(0).expect("Invalid char literal");
            self.data.clear();
            self.add(TokenKind::CharLiteral(c), span);
            self.escape_code = false;
            self.state = LexState::Idle;
        }

        Ok(())
    }

    fn start_of_line(&mut self, c: char) -> CompileResult<()>
    {
        match c {
            '\n' => {
                self.indent_level = 0;
                Ok(())
            }
            ' ' => {
                self.indent_level += 1;
                Ok(())
            }
            '\t' => {
                self.indent_level += 4;
                Ok(())
            }
            _ => {
                let level = self.indent_level;
                let span = Span::new(&self.file_name, self.token_start_pos, self.pos);
                self.add(TokenKind::Indent(level), span);
                self.start(c, LexState::Idle);
                self.feed(c)
            }
        }
    }

    fn feed(&mut self, c: char) -> CompileResult<()>
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
            for c in line?.chars()
            {
                self.feed(c)?;
                self.pos.offset += 1;
            }

            self.feed('\n')?;
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
    use ast::{BinaryOperator, UnaryOperator, AssignOperator};
    use parser::lexer::Lexer;
    use parser::tokens::*;
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
            tok(TokenKind::Indent(0), 1, 1, 1, 1),
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
            tok(TokenKind::Indent(0), 1, 1, 1, 1),
            tok(TokenKind::Identifier("blaat".into()), 1, 1, 1, 5),
            tok(TokenKind::Number("8888".into()), 1, 7, 1, 10),
            tok(TokenKind::Identifier("_foo_16".into()), 1, 12, 1, 18),
            tok(TokenKind::EOF, 2, 1, 2, 1),
        ]);
    }

    #[test]
    fn test_specials()
    {
        let mut cursor = Cursor::new("+ - * / % < <= > >= == = != ! || && => -> : , :: $");
        let tokens: Vec<Token> = Lexer::new("")
            .read(&mut cursor)
            .expect("Lexing failed")
            .collect();

        assert_eq!(tokens, vec![
            tok(TokenKind::Indent(0), 1, 1, 1, 1),
            tok(TokenKind::BinaryOperator(BinaryOperator::Add), 1, 1, 1, 1),
            tok(TokenKind::BinaryOperator(BinaryOperator::Sub), 1, 3, 1, 3),
            tok(TokenKind::BinaryOperator(BinaryOperator::Mul), 1, 5, 1, 5),
            tok(TokenKind::BinaryOperator(BinaryOperator::Div), 1, 7, 1, 7),
            tok(TokenKind::BinaryOperator(BinaryOperator::Mod), 1, 9, 1, 9),
            tok(TokenKind::BinaryOperator(BinaryOperator::LessThan), 1, 11, 1, 11),
            tok(TokenKind::BinaryOperator(BinaryOperator::LessThanEquals), 1, 13, 1, 14),
            tok(TokenKind::BinaryOperator(BinaryOperator::GreaterThan), 1, 16, 1, 16),
            tok(TokenKind::BinaryOperator(BinaryOperator::GreaterThanEquals), 1, 18, 1, 19),
            tok(TokenKind::BinaryOperator(BinaryOperator::Equals), 1, 21, 1, 22),
            tok(TokenKind::Assign(AssignOperator::Assign), 1, 24, 1, 24),
            tok(TokenKind::BinaryOperator(BinaryOperator::NotEquals), 1, 26, 1, 27),
            tok(TokenKind::UnaryOperator(UnaryOperator::Not), 1, 29, 1, 29),
            tok(TokenKind::BinaryOperator(BinaryOperator::Or), 1, 31, 1, 32),
            tok(TokenKind::BinaryOperator(BinaryOperator::And), 1, 34, 1, 35),
            tok(TokenKind::FatArrow, 1, 37, 1, 38),
            tok(TokenKind::Arrow, 1, 40, 1, 41),
            tok(TokenKind::Colon, 1, 43, 1, 43),
            tok(TokenKind::Comma, 1, 45, 1, 45),
            tok(TokenKind::DoubleColon, 1, 47, 1, 48),
            tok(TokenKind::Dollar, 1, 50, 1, 50),
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
            tok(TokenKind::Indent(0), 1, 1, 1, 1),
            tok(TokenKind::StringLiteral("This is a string".into()), 1, 1, 1, 18),
            tok(TokenKind::StringLiteral("Blaat\n".into()), 1, 20, 1, 28),
            tok(TokenKind::StringLiteral("$a".into()), 1, 30, 1, 33),
            tok(TokenKind::EOF, 2, 1, 2, 1),
        ]);
    }
}
