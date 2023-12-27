use std::error::Error;

use crate::ast::{prefix, Expression, Type};
use crate::span::Span;
use serde_derive::{Deserialize, Serialize};

use super::{DropFlag, TreePrinter};

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct Block {
    pub expressions: Vec<Expression>,
    // expressions to execute before leaving the block
    // (destructors mostly)
    pub drop_flags: Vec<DropFlag>,
    pub deferred_expressions: Vec<Expression>,
    pub typ: Type,
    pub span: Span,
}

pub fn block_expr(e: Vec<Expression>, span: Span) -> Expression {
    Expression::Block(Box::new(block(e, span)))
}

pub fn block(e: Vec<Expression>, span: Span) -> Block {
    Block {
        expressions: e,
        drop_flags: Vec::new(),
        deferred_expressions: Vec::new(),
        typ: Type::Unknown,
        span,
    }
}

impl Block {
    pub fn visit_mut<E, Op>(&mut self, op: &mut Op) -> Result<(), E>
    where
        E: Error,
        Op: FnMut(&mut Expression) -> Result<(), E>,
    {
        for e in &mut self.expressions {
            e.visit_mut(op)?;
        }
        for e in &mut self.deferred_expressions {
            e.visit_mut(op)?;
        }
        Ok(())
    }

    pub fn visit<E, Op>(&self, op: &mut Op) -> Result<(), E>
    where
        E: Error,
        Op: FnMut(&Expression) -> Result<(), E>,
    {
        for e in &self.expressions {
            e.visit(op)?;
        }
        for e in &self.deferred_expressions {
            e.visit(op)?;
        }
        Ok(())
    }
}

impl TreePrinter for Block {
    fn print(&self, level: usize) {
        println!("{}block: ({})", prefix(level), self.span);
        for e in &self.expressions {
            e.print(level + 1);
        }
    }
}
