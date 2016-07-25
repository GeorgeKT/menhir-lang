use std::ops::Deref;
use ast::{Expression, NameRef, Call, TreePrinter, prefix};
use compileerror::{Span};

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Member
{
    Call(Call),
    Nested(Box<MemberAccess>),
    Var(NameRef),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct MemberAccess
{
    pub target: Box<Expression>,
    pub member: Member,
    pub span: Span,
}

impl MemberAccess
{
    pub fn new(target: Expression, member: Member, span: Span) -> MemberAccess
    {
        MemberAccess{
            target: Box::new(target),
            member: member,
            span: span,
        }
    }

    pub fn name(&self) -> Option<String>
    {
        match *self.target.deref()
        {
            Expression::NameRef(ref nr) => Some(nr.name.clone()),
            _ => None,
        }
    }
}

impl TreePrinter for MemberAccess
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        println!("{}member access ({})", p, self.span);
        self.target.print(level + 1);
        match self.member
        {
            Member::Call(ref c) => c.print(level + 1),
            Member::Nested(ref n) => n.print(level + 1),
            Member::Var(ref n) => n.print(level + 1),
        }
    }
}


#[cfg(test)]
pub fn member_access(name: NameRef, member: Member, span: Span) -> Expression
{
    Expression::MemberAccess(
        MemberAccess::new(
            Expression::NameRef(name),
            member,
            span))
}
