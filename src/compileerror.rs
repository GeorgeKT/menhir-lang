use std::error::Error;
use std::convert::From;
use std::iter::repeat;
use std::fs::File;
use std::io;
use std::io::BufRead;
use std::fmt;
use ast::Type;
use span::Span;


#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ErrorCode
{
    UnexpectedEOF,
    IOError,
    MutabilityError,
    UnexpectedChar,
    UnexpectedToken,
    ExpectedIdentifier,
    ExpectedIntLiteral,
    ExpectedOperator,
    InvalidOperator,
    InvalidUnaryOperator,
    InvalidBinaryOperator,
    InvalidFloatingPoint,
    InvalidInteger,
    InvalidCharLiteral,
    TypeError,
    UnknownName,
    MissingType,
    CallingNonCallable,
    //CodegenError,
    RedefinitionOfVariable,
    RedefinitionOfFunction,
    RedefinitionOfStruct,
    GenericTypeSubstitutionError,
    ExpressionNotAllowedAtTopLevel,
    LambdaDoesNotMatch,
    WrongArgumentCount,
    UnreachablePatternMatch,
    DuplicatePatternMatch,
    IncompletePatternMatch,
    UnknownType(String, Type), // Name and expected type
    UnknownStructMember,
    FileNotFound,
    SelfTypeUnknown,
}


#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CompileError
{
    pub span: Span,
    pub error: ErrorCode,
    pub msg: String,
}

pub fn print_message(msg: &str, span: &Span)
{
    let prefix = "| ";
    println!("{}: {}", span, msg);
    if let Ok(file) = File::open(&span.file) {
        let start_line = if span.start.line >= 4 {span.start.line - 4} else {0};
        let reader = io::BufReader::new(file);

        for (idx, line) in reader.lines().enumerate().skip(start_line)
        {
            let line = line.unwrap();
            let line_idx = idx + 1;
            println!("{:>4} {}{}", line_idx, prefix, line);
            if line_idx == span.start.line
            {
                let end = if line_idx == span.end.line {span.end.offset} else {line.len()};
                let carets = repeat_string("^", end - span.start.offset + 1);
                let whitespace = repeat_string(" ", span.start.offset - 1);
                println!("     {}{}{}", prefix, whitespace, carets);
            }
            else if line_idx == span.end.line
            {
                let carets = repeat_string("^", span.end.offset);
                println!("     {}{}", prefix, carets);
            }
            else if line_idx > span.start.line && line_idx < span.end.line && !line.is_empty()
            {
                let carets = repeat_string("^", line.len());
                println!("     {}{}", prefix, carets);
            }

            if line_idx >= span.end.line + 3 {break;}
        }
    }
}

impl CompileError
{
    pub fn new(span: &Span, error: ErrorCode, msg: String) -> CompileError
    {
        CompileError{
            span: span.clone(),
            error: error,
            msg: msg,
        }
    }

    pub fn print(&self)
    {
        print_message(&self.msg, &self.span);
    }
}

pub type CompileResult<T> = Result<T, CompileError>;

pub fn err<T: Sized, Msg: Into<String>>(span: &Span, e: ErrorCode, msg: Msg) -> CompileResult<T>
{
    Err(CompileError::new(span, e, msg.into()))
}

pub fn unknown_name(span: &Span, name: &str) -> CompileError
{
    CompileError::new(span, ErrorCode::UnknownName, format!("Unable to resolve name {}", name))
}

fn repeat_string(s: &str, count: usize) -> String
{
    repeat(s).take(count).collect()
}

impl fmt::Display for CompileError
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error>
    {
        writeln!(f, "{}: {}", self.span, self.msg)
    }
}

impl Error for CompileError
{
    fn description(&self) -> &str
    {
        "Compile error"
    }
}

impl From<io::Error> for CompileError
{
    fn from(e: io::Error) -> Self
    {
        CompileError{
            span: Span::default(),
            error: ErrorCode::IOError,
            msg: format!("IO Error: {}", e),
        }
    }
}
