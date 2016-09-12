use std::error::Error;
use std::convert::From;
use std::io;
use std::fmt;
use ast::Type;
use span::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ErrorCode
{
    UnexpectedEOF,
    IOError,
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
    TypeError,
    UnknownName,
    MissingType,
    CallingNonCallable,
    CodegenError,
    RedefinitionOfVariable,
    RedefinitionOfFunction,
    RedefinitionOfStruct,
    GenericTypeSubstitutionError,
    ExpressionNotAllowedAtTopLevel,
    LambdaDoesNotMatch,
    WrongArgumentCount,
    UnknownType(String, Type), // Name and expected type
    UnknownStructMember,
    FileNotFound,
}


#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CompileError
{
    pub span: Span,
    pub error: ErrorCode,
    pub msg: String,
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
}

pub type CompileResult<T> = Result<T, CompileError>;

pub fn err<T: Sized>(span: &Span, e: ErrorCode, msg: String) -> CompileResult<T>
{
    Err(CompileError::new(span, e, msg))
}

pub fn unknown_name(span: &Span, name: &str) -> CompileError
{
    CompileError::new(span, ErrorCode::UnknownName, format!("Unable to resolve name {}", name))
}

impl fmt::Display for CompileError
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error>
    {
        write!(f, "{}: {}", self.span, self.msg)
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
