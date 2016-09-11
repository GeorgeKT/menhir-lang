mod lexer;
mod tokens;
mod tokenqueue;
mod parser;

#[cfg(test)]
mod tests;

#[cfg(test)]
pub use self::tests::{th_expr, th_mod};

pub use self::lexer::{Lexer};
pub use self::tokenqueue::{TokenQueue};
pub use self::tokens::{Operator, Token, TokenKind};
pub use self::parser::{parse_expression, parse_file, parse_module};

use std::path::PathBuf;

pub struct ParserOptions
{
    pub import_dirs: Vec<PathBuf>,
}

impl Default for ParserOptions
{
    fn default() -> Self
    {
        ParserOptions{
            import_dirs: Vec::new(),
        }
    }
}
