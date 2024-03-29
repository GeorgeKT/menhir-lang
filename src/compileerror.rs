use crate::ast::Type;
use crate::span::Span;
use console::Style;
use std::convert::From;
use std::error::Error;
use std::fmt;
use std::fs::File;
use std::io;
use std::io::BufRead;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ErrorData {
    pub span: Span,
    pub msg: String,
}

impl ErrorData {
    pub fn new<S: Into<String>>(span: &Span, msg: S) -> ErrorData {
        ErrorData {
            span: span.clone(),
            msg: msg.into(),
        }
    }
}

impl fmt::Display for ErrorData {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        writeln!(f, "{}: {}", self.span, self.msg)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompileError {
    Other(String),
    IO(String),
    Parse(ErrorData),
    Type(ErrorData),
    UnknownName(ErrorData),
    UnknownType(String, Type), // Name and expected type
    Many(Vec<CompileError>),
    CodeGeneration(String),
}

impl CompileError {
    pub fn print(&self) {
        match self {
            CompileError::Other(msg) | CompileError::IO(msg) => println!("{}", msg),
            CompileError::Parse(ed) | CompileError::Type(ed) | CompileError::UnknownName(ed) => {
                print_message(&ed.msg, &ed.span)
            }
            CompileError::UnknownType(name, typ) => println!("{} has unknown type, expecting {}", name, typ),
            CompileError::Many(errors) => {
                for e in errors {
                    e.print();
                }
            }
            CompileError::CodeGeneration(msg) => {
                println!("Code generation error: {}", msg);
            }
        }
    }
}

impl Error for CompileError {
    fn description(&self) -> &str {
        "CompileError"
    }
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            CompileError::Other(msg) | CompileError::IO(msg) | CompileError::CodeGeneration(msg) => {
                writeln!(f, "{}", msg)
            }
            CompileError::Parse(ed) | CompileError::Type(ed) | CompileError::UnknownName(ed) => ed.fmt(f),
            CompileError::UnknownType(name, typ) => writeln!(f, "{} has unknown type, expecting {}", name, typ),
            CompileError::Many(errors) => {
                for err in errors {
                    err.fmt(f)?;
                }
                Ok(())
            }
        }
    }
}

pub fn print_message(msg: &str, span: &Span) {
    let red = Style::new().red();
    fn repeat_string(s: &str, count: usize) -> String {
        s.repeat(count)
    }

    let Span::File { file, start, end } = span else {
        println!("{}", red.apply_to(msg));
        return;
    };

    let prefix = "| ";
    println!("{}: {}", span, red.apply_to(msg));
    if let Ok(file) = File::open(file) {
        let start_line = if start.line >= 4 { start.line - 4 } else { 0 };
        let reader = io::BufReader::new(file);

        for (idx, line) in reader.lines().enumerate().skip(start_line) {
            let line = line.unwrap();
            let line_idx = idx + 1;
            println!("{:>4} {}{}", line_idx, prefix, line);
            if line_idx == start.line {
                let end = if line_idx == end.line { end.offset } else { line.len() };
                let carets = repeat_string("^", end - start.offset + 1);
                let whitespace = repeat_string(" ", start.offset - 1);
                println!("     {}{}{}", prefix, whitespace, red.apply_to(carets));
            } else if line_idx == end.line {
                let carets = repeat_string("^", end.offset);
                println!("     {}{}", prefix, red.apply_to(carets));
            } else if line_idx > start.line && line_idx < end.line && !line.is_empty() {
                let carets = repeat_string("^", line.len());
                println!("     {}{}", prefix, red.apply_to(carets));
            }

            if line_idx >= end.line + 3 {
                break;
            }
        }
    }
}

pub type CompileResult<T> = Result<T, CompileError>;

pub fn parse_error<Msg: Into<String>>(span: &Span, msg: Msg) -> CompileError {
    CompileError::Parse(ErrorData::new(span, msg.into()))
}

pub fn parse_error_result<T, Msg: Into<String>>(span: &Span, msg: Msg) -> CompileResult<T> {
    Err(CompileError::Parse(ErrorData::new(span, msg.into())))
}

pub fn type_error_result<T, Msg: Into<String>>(span: &Span, msg: Msg) -> CompileResult<T> {
    Err(CompileError::Type(ErrorData::new(span, msg.into())))
}

pub fn type_error<Msg: Into<String>>(span: &Span, msg: Msg) -> CompileError {
    CompileError::Type(ErrorData::new(span, msg))
}

pub fn unknown_name<Msg: Into<String>>(span: &Span, msg: Msg) -> CompileError {
    CompileError::UnknownName(ErrorData::new(span, msg))
}

pub fn unknown_name_result<T, Msg: Into<String>>(span: &Span, msg: Msg) -> CompileResult<T> {
    Err(CompileError::UnknownName(ErrorData::new(span, msg)))
}

pub fn unknown_type_result<T>(name: &str, typ: &Type) -> CompileResult<T> {
    Err(CompileError::UnknownType(name.into(), typ.clone()))
}

pub fn code_gen_error(msg: impl Into<String>) -> CompileError {
    CompileError::CodeGeneration(msg.into())
}

pub fn code_gen_result<T>(msg: impl Into<String>) -> CompileResult<T> {
    Err(CompileError::CodeGeneration(msg.into()))
}

impl From<io::Error> for CompileError {
    fn from(e: io::Error) -> Self {
        CompileError::IO(format!("IO Error: {}", e))
    }
}

impl From<String> for CompileError {
    fn from(e: String) -> Self {
        CompileError::Other(e)
    }
}
