use std::fmt;
use ast::{Expression, NameRef, DereferenceExpression, MemberAccess, IndexOperation, TreePrinter, prefix};
use span::{Span};


#[derive(Debug, Eq, PartialEq, Clone, Copy, Serialize, Deserialize)]
pub enum AssignOperator
{
    Assign,
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or
}

impl fmt::Display for AssignOperator
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match *self {
            AssignOperator::Assign => write!(f, "="),
            AssignOperator::Add => write!(f, "+="),
            AssignOperator::Sub => write!(f, "-="),
            AssignOperator::Mul => write!(f, "*="),
            AssignOperator::Div => write!(f, "/="),
            AssignOperator::And => write!(f, "&&="),
            AssignOperator::Or => write!(f, "||="),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub enum AssignTarget
{
    Var(NameRef),
    MemberAccess(MemberAccess),
    Dereference(DereferenceExpression),
    IndexOperation(IndexOperation)
}

impl TreePrinter for AssignTarget
{
    fn print(&self, level: usize)
    {
        match *self {
            AssignTarget::Var(ref nr) => nr.print(level),
            AssignTarget::MemberAccess(ref ma) => ma.print(level),
            AssignTarget::Dereference(ref d) => d.print(level),
            AssignTarget::IndexOperation(ref iop) => iop.print(level),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct Assign
{
    pub operator: AssignOperator,
    pub left: AssignTarget,
    pub right: Expression,
    pub span: Span,
}

pub fn assign(operator: AssignOperator, left: AssignTarget, right: Expression, span: Span) -> Expression
{
    Expression::Assign(Box::new(Assign{
        operator,
        left,
        right,
        span,
    }))
}


impl TreePrinter for Assign
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        println!("{}assign {} (span: {})", p, self.operator, self.span);
        self.left.print(level + 1);
        self.right.print(level + 1);
    }
}
