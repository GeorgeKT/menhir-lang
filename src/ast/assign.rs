use ast::{Expression, NameRef, DereferenceExpression, MemberAccess, Type, TreePrinter, prefix};
use span::{Span};

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum AssignTarget
{
    Var(NameRef),
    MemberAccess(MemberAccess),
    Dereference(DereferenceExpression)
}

impl TreePrinter for AssignTarget
{
    fn print(&self, level: usize)
    {
        match *self {
            AssignTarget::Var(ref nr) => nr.print(level),
            AssignTarget::MemberAccess(ref ma) => ma.print(level),
            AssignTarget::Dereference(ref d) => d.print(level),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Assign
{
    pub left: AssignTarget,
    pub right: Expression,
    pub typ: Type,
    pub span: Span,
}

pub fn assign(left: AssignTarget, right: Expression, span: Span) -> Expression
{
    Expression::Assign(Box::new(Assign{
        left: left,
        right: right,
        typ: Type::Unknown,
        span: span,
    }))
}


impl TreePrinter for Assign
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        println!("{}assign (span: {}; type: {})", p, self.span, self.typ);
        self.left.print(level + 1);
        self.right.print(level + 1);
    }
}
