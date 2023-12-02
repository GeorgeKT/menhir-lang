use crate::ast::*;
use crate::span::Span;
use serde_derive::{Deserialize, Serialize};
use std::error::Error;

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct ToOptional {
    pub inner: Expression,
    pub optional_type: Type,
}

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct ToOkResult {
    pub inner: Expression,
    pub result_type: Type,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct ToErrResult {
    pub inner: Expression,
    pub result_type: Type,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct TypeCast {
    pub inner: Expression,
    pub destination_type: Type,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct Nil {
    pub typ: Type,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct Return {
    pub expression: Expression,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub enum Expression {
    Literal(Literal),
    UnaryOp(Box<UnaryOp>),
    BinaryOp(Box<BinaryOp>),
    Block(Box<Block>),
    Call(Box<Call>),
    NameRef(NameRef),
    Match(Box<MatchExpression>),
    If(Box<IfExpression>),
    Lambda(Box<Lambda>),
    Bindings(Box<BindingList>),
    StructInitializer(StructInitializer),
    MemberAccess(Box<MemberAccess>),
    New(Box<NewExpression>),
    Delete(Box<DeleteExpression>),
    ArrayToSlice(Box<ArrayToSlice>),
    AddressOf(Box<AddressOfExpression>),
    Dereference(Box<DereferenceExpression>),
    Assign(Box<Assign>),
    While(Box<WhileLoop>),
    For(Box<ForLoop>),
    Nil(Nil),
    OptionalToBool(Box<Expression>),
    ResultToBool(Box<Expression>),
    ToOptional(Box<ToOptional>),
    ToOkResult(Box<ToOkResult>),
    ToErrResult(Box<ToErrResult>),
    Cast(Box<TypeCast>),
    CompilerCall(CompilerCall),
    IndexOperation(Box<IndexOperation>),
    Return(Box<Return>),
    Void,
}

pub fn to_optional(e: Expression, typ: Type) -> Expression {
    Expression::ToOptional(Box::new(ToOptional {
        inner: e,
        optional_type: typ,
    }))
}

pub fn to_ok_result(e: Expression, typ: Type, span: Span) -> Expression {
    Expression::ToOkResult(Box::new(ToOkResult {
        inner: e,
        result_type: typ,
        span,
    }))
}

pub fn to_err_result(e: Expression, typ: Type, span: Span) -> Expression {
    Expression::ToErrResult(Box::new(ToErrResult {
        inner: e,
        result_type: typ,
        span,
    }))
}

pub fn type_cast(e: Expression, dst_type: Type, span: Span) -> Expression {
    Expression::Cast(Box::new(TypeCast {
        inner: e,
        destination_type: dst_type,
        span,
    }))
}

pub fn nil_expr(span: Span) -> Expression {
    Expression::Nil(Nil {
        typ: optional_type(Type::Unknown),
        span,
    })
}

pub fn return_expr(expression: Expression, span: Span) -> Expression {
    Expression::Return(Box::new(Return { expression, span }))
}

pub fn nil_expr_with_type(span: Span, optional_inner_type: Type) -> Expression {
    Expression::Nil(Nil {
        typ: optional_type(optional_inner_type),
        span,
    })
}

pub fn name_expr(name: &str, span: Span) -> Expression {
    Expression::NameRef(NameRef::new(name.into(), span))
}

impl Expression {
    pub fn precedence(&self) -> usize {
        match self {
            Expression::BinaryOp(op) => op.precedence,
            _ => 0,
        }
    }

    pub fn set_precedence(&mut self, precedence: usize) {
        if let Expression::BinaryOp(op) = self {
            op.precedence = precedence;
        }
    }

    pub fn is_binary_op(&self) -> bool {
        matches!(*self, Expression::BinaryOp(_))
    }

    pub fn extract_binary_op(self) -> Option<Box<BinaryOp>> {
        match self {
            Expression::BinaryOp(b) => Some(b),
            _ => None,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Expression::Literal(lit) => lit.span(),
            Expression::UnaryOp(op) => op.span.clone(),
            Expression::BinaryOp(op) => op.span.clone(),
            Expression::Block(b) => b.span.clone(),
            Expression::Call(c) => c.span.clone(),
            Expression::NameRef(nr) => nr.span.clone(),
            Expression::Match(m) => m.span.clone(),
            Expression::Lambda(l) => l.span.clone(),
            Expression::Bindings(l) => l.span.clone(),
            Expression::If(i) => i.span.clone(),
            Expression::StructInitializer(si) => si.span.clone(),
            Expression::MemberAccess(sma) => sma.span.clone(),
            Expression::New(n) => n.span.clone(),
            Expression::Delete(d) => d.span.clone(),
            Expression::ArrayToSlice(a) => a.inner.span(),
            Expression::AddressOf(a) => a.span.clone(),
            Expression::Dereference(d) => d.span.clone(),
            Expression::Assign(a) => a.span.clone(),
            Expression::While(w) => w.span.clone(),
            Expression::For(f) => f.span.clone(),
            Expression::Nil(nt) => nt.span.clone(),
            Expression::OptionalToBool(inner) => inner.span(),
            Expression::ToOptional(t) => t.inner.span(),
            Expression::Cast(t) => t.span.clone(),
            Expression::CompilerCall(CompilerCall::SizeOf(_, span)) => span.clone(),
            Expression::CompilerCall(CompilerCall::Slice { span, .. }) => span.clone(),
            Expression::IndexOperation(iop) => iop.span.clone(),
            Expression::Return(r) => r.span.clone(),
            Expression::Void => Span::default(),
            Expression::ResultToBool(r) => r.span(),
            Expression::ToOkResult(r) => r.inner.span(),
            Expression::ToErrResult(r) => r.inner.span(),
        }
    }

    pub fn get_type(&self, int_size: IntSize) -> Type {
        match self {
            Expression::Literal(lit) => lit.get_type(),
            Expression::UnaryOp(op) => op.typ.clone(),
            Expression::BinaryOp(op) => op.typ.clone(),
            Expression::Block(b) => b.typ.clone(),
            Expression::Call(c) => c.return_type.clone(),
            Expression::NameRef(nr) => nr.typ.clone(),
            Expression::Match(m) => m.typ.clone(),
            Expression::Lambda(l) => l.sig.get_type(),
            Expression::Bindings(l) => l
                .bindings
                .last()
                .map(|b| b.typ.clone())
                .expect("Binding types are not known"),
            Expression::If(i) => i.typ.clone(),
            Expression::StructInitializer(si) => si.typ.clone(),
            Expression::MemberAccess(sma) => sma.typ.clone(),
            Expression::New(n) => n.typ.clone(),
            Expression::ArrayToSlice(a) => a.slice_type.clone(),
            Expression::AddressOf(a) => ptr_type(a.inner.get_type(int_size)),
            Expression::Dereference(d) => d.typ.clone(),
            Expression::Assign(_) => Type::Void,
            Expression::Nil(nt) => nt.typ.clone(),
            Expression::OptionalToBool(_) => Type::Bool,
            Expression::ToOptional(t) => optional_type(t.inner.get_type(int_size)),
            Expression::Cast(t) => t.destination_type.clone(),
            Expression::CompilerCall(cc) => cc.get_type(int_size),
            Expression::IndexOperation(iop) => iop.typ.clone(),
            Expression::Return(r) => r.expression.get_type(int_size),
            Expression::Void | Expression::While(_) | Expression::Delete(_) | Expression::For(_) => Type::Void,
            Expression::ResultToBool(_) => Type::Bool,
            Expression::ToOkResult(r) => r.result_type.clone(),
            Expression::ToErrResult(r) => r.result_type.clone(),
        }
    }

    pub fn visit_mut<E, Op>(&mut self, op: &mut Op) -> Result<(), E>
    where
        E: Error,
        Op: FnMut(&mut Expression) -> Result<(), E>,
    {
        op(self)?;
        match self {
            Expression::UnaryOp(uop) => uop.expression.visit_mut(op),

            Expression::BinaryOp(bop) => {
                bop.left.visit_mut(op)?;
                bop.right.visit_mut(op)
            }

            Expression::Literal(Literal::Array(a)) => {
                for el in &mut a.elements {
                    el.visit_mut(op)?;
                }
                Ok(())
            }

            Expression::Call(call) => {
                for a in &mut call.args {
                    a.visit_mut(op)?;
                }
                Ok(())
            }

            Expression::Lambda(l) => l.expr.visit_mut(op),

            Expression::Match(m) => {
                m.target.visit_mut(op)?;
                for c in &mut m.cases {
                    if let Pattern::Literal(Literal::Array(al)) = &mut c.pattern {
                        for el in &mut al.elements {
                            el.visit_mut(op)?;
                        }
                    }
                    c.to_execute.visit_mut(op)?;
                }
                Ok(())
            }

            Expression::Bindings(l) => {
                for b in &mut l.bindings {
                    b.init.visit_mut(op)?
                }
                Ok(())
            }

            Expression::Block(b) => {
                for e in &mut b.expressions {
                    e.visit_mut(op)?;
                }
                for e in &mut b.deferred_expressions {
                    e.visit_mut(op)?;
                }
                Ok(())
            }

            Expression::New(n) => n.inner.visit_mut(op),

            Expression::Delete(d) => d.inner.visit_mut(op),

            Expression::ArrayToSlice(ats) => ats.inner.visit_mut(op),

            Expression::Return(r) => r.expression.visit_mut(op),

            Expression::If(i) => {
                i.condition.visit_mut(op)?;
                i.on_true.visit_mut(op)?;
                if let Some(e) = &mut i.on_false {
                    e.visit_mut(op)?;
                }
                Ok(())
            }

            Expression::StructInitializer(si) => {
                for e in &mut si.member_initializers {
                    e.visit_mut(op)?;
                }
                Ok(())
            }

            Expression::AddressOf(a) => a.inner.visit_mut(op),

            Expression::Dereference(d) => d.inner.visit_mut(op),

            Expression::While(w) => {
                w.cond.visit_mut(op)?;
                w.body.visit_mut(op)
            }

            Expression::Assign(a) => {
                match &mut a.left {
                    AssignTarget::Dereference(d) => d.inner.visit_mut(op)?,
                    AssignTarget::IndexOperation(iop) => {
                        iop.target.visit_mut(op)?;
                        iop.index_expr.visit_mut(op)?;
                    }
                    AssignTarget::MemberAccess(ma) => {
                        ma.left.visit_mut(op)?;
                        if let MemberAccessType::Call(call) = &mut ma.right {
                            for a in &mut call.args {
                                a.visit_mut(op)?;
                            }
                        }
                    }
                    _ => (),
                }
                a.right.visit_mut(op)
            }

            Expression::For(f) => {
                f.iterable.visit_mut(op)?;
                f.body.visit_mut(op)
            }

            Expression::OptionalToBool(o) => o.visit_mut(op),

            Expression::MemberAccess(ma) => {
                ma.left.visit_mut(op)?;
                if let MemberAccessType::Call(call) = &mut ma.right {
                    for a in &mut call.args {
                        a.visit_mut(op)?;
                    }
                }
                Ok(())
            }

            Expression::ToOptional(t) => t.inner.visit_mut(op),

            Expression::Cast(c) => c.inner.visit_mut(op),

            Expression::IndexOperation(iop) => {
                iop.target.visit_mut(op)?;
                iop.index_expr.visit_mut(op)
            }

            Expression::CompilerCall(CompilerCall::Slice { data, len, .. }) => {
                data.visit_mut(op)?;
                len.visit_mut(op)
            }

            Expression::Literal(_)
            | Expression::Void
            | Expression::CompilerCall(_)
            | Expression::Nil(_)
            | Expression::NameRef(_) => Ok(()),

            Expression::ResultToBool(r) => r.visit_mut(op),
            Expression::ToOkResult(r) => r.inner.visit_mut(op),
            Expression::ToErrResult(r) => r.inner.visit_mut(op),
        }
    }

    pub fn visit<E, Op>(&self, op: &mut Op) -> Result<(), E>
    where
        E: Error,
        Op: FnMut(&Expression) -> Result<(), E>,
    {
        op(self)?;
        match self {
            Expression::UnaryOp(uop) => uop.expression.visit(op),

            Expression::BinaryOp(bop) => {
                bop.left.visit(op)?;
                bop.right.visit(op)
            }

            Expression::Literal(Literal::Array(a)) => {
                for el in &a.elements {
                    el.visit(op)?;
                }
                Ok(())
            }

            Expression::Call(call) => {
                for a in &call.args {
                    a.visit(op)?;
                }
                Ok(())
            }

            Expression::Lambda(l) => l.expr.visit(op),

            Expression::Match(m) => {
                m.target.visit(op)?;
                for c in &m.cases {
                    if let Pattern::Literal(Literal::Array(al)) = &c.pattern {
                        for el in &al.elements {
                            el.visit(op)?;
                        }
                    }
                    c.to_execute.visit(op)?;
                }
                Ok(())
            }

            Expression::Bindings(l) => {
                for b in &l.bindings {
                    b.init.visit(op)?
                }
                Ok(())
            }

            Expression::Block(b) => {
                for e in &b.expressions {
                    e.visit(op)?;
                }
                for e in &b.deferred_expressions {
                    e.visit(op)?;
                }
                Ok(())
            }

            Expression::New(n) => n.inner.visit(op),

            Expression::Delete(d) => d.inner.visit(op),

            Expression::ArrayToSlice(ats) => ats.inner.visit(op),

            Expression::Return(r) => r.expression.visit(op),

            Expression::If(i) => {
                i.condition.visit(op)?;
                i.on_true.visit(op)?;
                if let Some(e) = &i.on_false {
                    e.visit(op)?;
                }
                Ok(())
            }

            Expression::StructInitializer(si) => {
                for e in &si.member_initializers {
                    e.visit(op)?;
                }
                Ok(())
            }

            Expression::AddressOf(a) => a.inner.visit(op),

            Expression::Dereference(d) => d.inner.visit(op),

            Expression::While(w) => {
                w.cond.visit(op)?;
                w.body.visit(op)
            }

            Expression::Assign(a) => {
                match &a.left {
                    AssignTarget::Dereference(d) => d.inner.visit(op)?,
                    AssignTarget::IndexOperation(iop) => {
                        iop.target.visit(op)?;
                        iop.index_expr.visit(op)?;
                    }
                    AssignTarget::MemberAccess(ma) => {
                        ma.left.visit(op)?;
                        if let MemberAccessType::Call(call) = &ma.right {
                            for a in &call.args {
                                a.visit(op)?;
                            }
                        }
                    }
                    _ => (),
                }
                a.right.visit(op)
            }

            Expression::For(f) => {
                f.iterable.visit(op)?;
                f.body.visit(op)
            }

            Expression::OptionalToBool(o) => o.visit(op),

            Expression::MemberAccess(ma) => {
                ma.left.visit(op)?;
                if let MemberAccessType::Call(call) = &ma.right {
                    for a in &call.args {
                        a.visit(op)?;
                    }
                }
                Ok(())
            }

            Expression::ToOptional(t) => t.inner.visit(op),

            Expression::Cast(c) => c.inner.visit(op),

            Expression::IndexOperation(iop) => {
                iop.target.visit(op)?;
                iop.index_expr.visit(op)
            }

            Expression::CompilerCall(CompilerCall::Slice { data, len, .. }) => {
                data.visit(op)?;
                len.visit(op)
            }

            Expression::Literal(_)
            | Expression::Void
            | Expression::CompilerCall(_)
            | Expression::Nil(_)
            | Expression::NameRef(_) => Ok(()),

            Expression::ResultToBool(r) => r.visit(op),
            Expression::ToOkResult(r) => r.inner.visit(op),
            Expression::ToErrResult(r) => r.inner.visit(op),
        }
    }
}

impl TreePrinter for Expression {
    fn print(&self, level: usize) {
        let p = prefix(level);
        match self {
            Expression::Literal(lit) => lit.print(level),
            Expression::UnaryOp(op) => {
                println!("{}unary {} ({})", p, op.operator, op.span);
                op.expression.print(level + 1)
            }
            Expression::BinaryOp(op) => {
                println!("{}binary {} ({}) (type: {})", p, op.operator, op.span, op.typ);
                op.left.print(level + 1);
                op.right.print(level + 1)
            }
            Expression::Block(b) => {
                println!("{}block ({}) (type: {})", p, b.span, b.typ);
                for e in &b.expressions {
                    e.print(level + 2);
                }
                println!("{} deferred: ", p);
                for e in &b.deferred_expressions {
                    e.print(level + 2);
                }
            }
            Expression::Call(c) => c.print(level),
            Expression::NameRef(nr) => nr.print(level),
            Expression::Match(m) => m.print(level),
            Expression::Lambda(l) => l.print(level),
            Expression::Bindings(l) => l.print(level),
            Expression::If(i) => i.print(level),
            Expression::StructInitializer(si) => si.print(level),
            Expression::MemberAccess(sma) => sma.print(level),
            Expression::New(n) => n.print(level),
            Expression::Delete(n) => n.print(level),
            Expression::ArrayToSlice(inner) => {
                println!("{}array to slice (type: {})", p, inner.slice_type);
                inner.inner.print(level + 1)
            }
            Expression::AddressOf(a) => a.print(level),
            Expression::Dereference(d) => d.print(level),
            Expression::Assign(a) => a.print(level),
            Expression::While(w) => w.print(level),
            Expression::For(f) => f.print(level),
            Expression::Nil(_) => println!("{}nil", p),
            Expression::OptionalToBool(n) => {
                println!("{}nil?", p);
                n.print(level + 1)
            }
            Expression::ToOptional(t) => {
                println!("{}to_optional (type: {})", p, t.optional_type);
                t.inner.print(level + 1)
            }
            Expression::Cast(t) => {
                println!("{}cast to {} ({})", p, t.destination_type, t.span);
                t.inner.print(level + 1)
            }
            Expression::CompilerCall(cc) => cc.print(level),
            Expression::IndexOperation(iop) => iop.print(level),
            Expression::Void => println!("{}void", p),
            Expression::Return(r) => {
                println!("{}return", p);
                r.expression.print(level + 1)
            }
            Expression::ResultToBool(r) => {
                println!("{}result to bool", p);
                r.print(level + 1)
            }
            Expression::ToOkResult(r) => {
                println!("{}ok", p);
                r.inner.print(level + 1)
            }
            Expression::ToErrResult(r) => {
                println!("{}err", p);
                r.inner.print(level + 1)
            }
        }
    }
}
