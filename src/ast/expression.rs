use std::error::Error;
use span::Span;
use ast::*;


#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ToOptional
{
    pub inner: Expression,
    pub optional_type: Type,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct TypeCast
{
    pub inner: Expression,
    pub destination_type: Type,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Nil
{
    pub typ: Type,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Return
{
    pub expression: Expression,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Expression
{
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
    ToOptional(Box<ToOptional>),
    Cast(Box<TypeCast>),
    CompilerCall(CompilerCall),
    IndexOperation(Box<IndexOperation>),
    Return(Box<Return>),
    Void,
}

pub fn to_optional(e: Expression, typ: Type) -> Expression
{
    Expression::ToOptional(Box::new(ToOptional{
        inner: e,
        optional_type: typ
    }))
}

pub fn type_cast(e: Expression, dst_type: Type, span: Span) -> Expression
{
    Expression::Cast(Box::new(TypeCast{
        inner: e,
        destination_type: dst_type,
        span: span,
    }))
}

pub fn nil_expr(span: Span) -> Expression
{
    Expression::Nil(Nil{
        typ: optional_type(Type::Unknown),
        span: span,
    })
}

pub fn return_expr(expression: Expression, span: Span) -> Expression
{
    Expression::Return(Box::new(Return{expression, span}))
}

pub fn nil_expr_with_type(span: Span, optional_inner_type: Type) -> Expression
{
    Expression::Nil(Nil{
        typ: optional_type(optional_inner_type),
        span: span,
    })
}

impl Expression
{
    pub fn precedence(&self) -> usize
    {
        match *self
        {
            Expression::BinaryOp(ref op) => op.precedence,
            _ => 0,
        }
    }

    pub fn set_precedence(&mut self, precedence: usize)
    {
        if let Expression::BinaryOp(ref mut op) = *self {
            op.precedence = precedence;
        }
    }

    pub fn is_binary_op(&self) -> bool
    {
        match *self
        {
            Expression::BinaryOp(_) => true,
            _ => false,
        }
    }

    pub fn extract_binary_op(self) -> Option<Box<BinaryOp>>
    {
        match self
        {
            Expression::BinaryOp(b) => Some(b),
            _ => None,
        }
    }

    pub fn span(&self) -> Span
    {
        match *self
        {
            Expression::Literal(ref lit) => lit.span(),
            Expression::UnaryOp(ref op) => op.span.clone(),
            Expression::BinaryOp(ref op) => op.span.clone(),
            Expression::Block(ref b) => b.span.clone(),
            Expression::Call(ref c) => c.span.clone(),
            Expression::NameRef(ref nr) => nr.span.clone(),
            Expression::Match(ref m) => m.span.clone(),
            Expression::Lambda(ref l) => l.span.clone(),
            Expression::Bindings(ref l) => l.span.clone(),
            Expression::If(ref i) => i.span.clone(),
            Expression::StructInitializer(ref si) => si.span.clone(),
            Expression::MemberAccess(ref sma) => sma.span.clone(),
            Expression::New(ref n) => n.span.clone(),
            Expression::Delete(ref d) => d.span.clone(),
            Expression::ArrayToSlice(ref a) => a.inner.span(),
            Expression::AddressOf(ref a) => a.span.clone(),
            Expression::Dereference(ref d) => d.span.clone(),
            Expression::Assign(ref a) => a.span.clone(),
            Expression::While(ref w) => w.span.clone(),
            Expression::For(ref f) => f.span.clone(),
            Expression::Nil(ref nt) => nt.span.clone(),
            Expression::OptionalToBool(ref inner) => inner.span(),
            Expression::ToOptional(ref t) => t.inner.span(),
            Expression::Cast(ref t) => t.span.clone(),
            Expression::CompilerCall(CompilerCall::SizeOf(_, ref span)) => span.clone(),
            Expression::IndexOperation(ref iop) => iop.span.clone(),
            Expression::Return(ref r) => r.span.clone(),
            Expression::Void => Span::default(),
        }
    }

    pub fn get_type(&self, int_size: IntSize) -> Type
    {
        match *self
        {
            Expression::Literal(ref lit) => lit.get_type(),
            Expression::UnaryOp(ref op) => op.typ.clone(),
            Expression::BinaryOp(ref op) => op.typ.clone(),
            Expression::Block(ref b) => b.typ.clone(),
            Expression::Call(ref c) => c.return_type.clone(),
            Expression::NameRef(ref nr) => nr.typ.clone(),
            Expression::Match(ref m) => m.typ.clone(),
            Expression::Lambda(ref l) => l.sig.get_type(),
            Expression::Bindings(ref l) => l.bindings.last().map(|b| b.typ.clone()).expect("Binding types are not known"),
            Expression::If(ref i) => i.typ.clone(),
            Expression::StructInitializer(ref si) => si.typ.clone(),
            Expression::MemberAccess(ref sma) => sma.typ.clone(),
            Expression::New(ref n) => n.typ.clone(),
            Expression::ArrayToSlice(ref a) => a.slice_type.clone(),
            Expression::AddressOf(ref a) => ptr_type(a.inner.get_type(int_size)),
            Expression::Dereference(ref d) => d.typ.clone(),
            Expression::Assign(ref a) => a.typ.clone(),
            Expression::Nil(ref nt) => nt.typ.clone(),
            Expression::OptionalToBool(_) => Type::Bool,
            Expression::ToOptional(ref t) => optional_type(t.inner.get_type(int_size)),
            Expression::Cast(ref t) => t.destination_type.clone(),
            Expression::CompilerCall(ref cc) => cc.get_type(int_size),
            Expression::IndexOperation(ref iop) => iop.typ.clone(),
            Expression::Return(ref r) => r.expression.get_type(int_size),
            Expression::Void |
            Expression::While(_) |
            Expression::Delete(_) |
            Expression::For(_) => Type::Void,
        }
    }

    pub fn visit_mut<E, Op>(&mut self, op: &mut Op) -> Result<(), E>
        where E: Error,
              Op: FnMut(&mut Expression) -> Result<(), E>
    {
        op(self)?;
        match *self
        {
            Expression::UnaryOp(ref mut uop) => {
                uop.expression.visit_mut(op)
            },

            Expression::BinaryOp(ref mut bop) => {
                bop.left.visit_mut(op)?;
                bop.right.visit_mut(op)
            },

            Expression::Literal(Literal::Array(ref mut a)) => {
                for el in &mut a.elements {
                    el.visit_mut(op)?;
                }
                Ok(())
            },

            Expression::Call(ref mut call) => {
                for a in &mut call.args {
                    a.visit_mut(op)?;
                }
                Ok(())
            },

            Expression::Lambda(ref mut l) => {
                l.expr.visit_mut(op)
            },

            Expression::Match(ref mut m) => {
                m.target.visit_mut(op)?;
                for c in &mut m.cases
                {
                    if let Pattern::Literal(Literal::Array(ref mut al)) = c.pattern {
                        for el in &mut al.elements {
                            el.visit_mut(op)?;
                        }
                    }
                    c.to_execute.visit_mut(op)?;
                }
                Ok(())
            },

            Expression::Bindings(ref mut l) => {
                for b in &mut l.bindings {
                    b.init.visit_mut(op)?
                }
                Ok(())
            },

            Expression::Block(ref mut b) => {
                for e in &mut b.expressions {
                    e.visit_mut(op)?;
                }
                Ok(())
            },

            Expression::New(ref mut n) => {
                n.inner.visit_mut(op)
            },

            Expression::Delete(ref mut d) => {
                d.inner.visit_mut(op)
            },

            Expression::ArrayToSlice(ref mut ats) => {
                ats.inner.visit_mut(op)
            },

            Expression::Return(ref mut r) => {
                r.expression.visit_mut(op)
            },

            Expression::If(ref mut i) => {
                i.condition.visit_mut(op)?;
                i.on_true.visit_mut(op)?;
                if let Some(ref mut e) = i.on_false {
                    e.visit_mut(op)?;
                }
                Ok(())
            }

            Expression::StructInitializer(ref mut si) => {
                for e in &mut si.member_initializers {
                    e.visit_mut(op)?;
                }
                Ok(())
            }

            Expression::AddressOf(ref mut a) => {
                a.inner.visit_mut(op)
            }

            Expression::Dereference(ref mut d) => {
                d.inner.visit_mut(op)
            }

            Expression::While(ref mut w) => {
                w.cond.visit_mut(op)?;
                w.body.visit_mut(op)
            }

            Expression::Assign(ref mut a) => {
                match a.left {
                    AssignTarget::Dereference(ref mut d) => d.inner.visit_mut(op)?,
                    AssignTarget::IndexOperation(ref mut iop) => {
                        iop.target.visit_mut(op)?;
                        iop.index_expr.visit_mut(op)?;
                    },
                    AssignTarget::MemberAccess(ref mut ma) => {
                        ma.left.visit_mut(op)?;
                        if let MemberAccessType::Call(ref mut call) = ma.right {
                            for a in &mut call.args {
                                a.visit_mut(op)?;
                            }
                        }
                    }
                    _ => (),
                }
                a.right.visit_mut(op)
            }

            Expression::For(ref mut f) => {
                f.iterable.visit_mut(op)?;
                f.body.visit_mut(op)
            }

            Expression::OptionalToBool(ref mut o) => {
                o.visit_mut(op)
            }

            Expression::MemberAccess(ref mut ma) => {
                ma.left.visit_mut(op)?;
                if let MemberAccessType::Call(ref mut call) = ma.right {
                    for a in &mut call.args {
                        a.visit_mut(op)?;
                    }
                }
                Ok(())
            }

            Expression::ToOptional(ref mut t) => {
                t.inner.visit_mut(op)
            }

            Expression::Cast(ref mut c) => {
                c.inner.visit_mut(op)
            }

            Expression::IndexOperation(ref mut iop) => {
                iop.target.visit_mut(op)?;
                iop.index_expr.visit_mut(op)
            }

            Expression::Literal(_) |
            Expression::Void |
            Expression::CompilerCall(_) |
            Expression::Nil(_) |
            Expression::NameRef(_) => Ok(())
        }
    }

    pub fn visit<E, Op>(&self, op: &mut Op) -> Result<(), E>
        where E: Error,
              Op: FnMut(&Expression) -> Result<(), E>
    {
        op(self)?;
        match *self
        {
            Expression::UnaryOp(ref uop) => {
                uop.expression.visit(op)
            },

            Expression::BinaryOp(ref bop) => {
                bop.left.visit(op)?;
                bop.right.visit(op)
            },

            Expression::Literal(Literal::Array(ref a)) => {
                for el in &a.elements {
                    el.visit(op)?;
                }
                Ok(())
            },

            Expression::Call(ref call) => {
                for a in &call.args {
                    a.visit(op)?;
                }
                Ok(())
            },

            Expression::Lambda(ref l) => {
                l.expr.visit(op)
            },

            Expression::Match(ref m) => {
                m.target.visit(op)?;
                for c in &m.cases
                    {
                        if let Pattern::Literal(Literal::Array(ref al)) = c.pattern {
                            for el in &al.elements {
                                el.visit(op)?;
                            }
                        }
                        c.to_execute.visit(op)?;
                    }
                Ok(())
            },

            Expression::Bindings(ref l) => {
                for b in &l.bindings {
                    b.init.visit(op)?
                }
                Ok(())
            },

            Expression::Block(ref b) => {
                for e in &b.expressions {
                    e.visit(op)?;
                }
                Ok(())
            },

            Expression::New(ref n) => {
                n.inner.visit(op)
            },

            Expression::Delete(ref d) => {
                d.inner.visit(op)
            },

            Expression::ArrayToSlice(ref ats) => {
                ats.inner.visit(op)
            },

            Expression::Return(ref r) => {
                r.expression.visit(op)
            },

            Expression::If(ref i) => {
                i.condition.visit(op)?;
                i.on_true.visit(op)?;
                if let Some(ref e) = i.on_false {
                    e.visit(op)?;
                }
                Ok(())
            }

            Expression::StructInitializer(ref si) => {
                for e in &si.member_initializers {
                    e.visit(op)?;
                }
                Ok(())
            }

            Expression::AddressOf(ref a) => {
                a.inner.visit(op)
            }

            Expression::Dereference(ref d) => {
                d.inner.visit(op)
            }

            Expression::While(ref w) => {
                w.cond.visit(op)?;
                w.body.visit(op)
            }

            Expression::Assign(ref a) => {
                match a.left {
                    AssignTarget::Dereference(ref d) => d.inner.visit(op)?,
                    AssignTarget::IndexOperation(ref iop) => {
                        iop.target.visit(op)?;
                        iop.index_expr.visit(op)?;
                    },
                    AssignTarget::MemberAccess(ref ma) => {
                        ma.left.visit(op)?;
                        if let MemberAccessType::Call(ref call) = ma.right {
                            for a in &call.args {
                                a.visit(op)?;
                            }
                        }
                    },
                    _ => (),
                }
                a.right.visit(op)
            }

            Expression::For(ref f) => {
                f.iterable.visit(op)?;
                f.body.visit(op)
            }

            Expression::OptionalToBool(ref o) => {
                o.visit(op)
            }

            Expression::MemberAccess(ref ma) => {
                ma.left.visit(op)?;
                if let MemberAccessType::Call(ref call) = ma.right {
                    for a in &call.args {
                        a.visit(op)?;
                    }
                }
                Ok(())
            }

            Expression::ToOptional(ref t) => {
                t.inner.visit(op)
            }

            Expression::Cast(ref c) => {
                c.inner.visit(op)
            }

            Expression::IndexOperation(ref iop) => {
                iop.target.visit(op)?;
                iop.index_expr.visit(op)
            }

            Expression::Literal(_) |
            Expression::Void |
            Expression::CompilerCall(_) |
            Expression::Nil(_) |
            Expression::NameRef(_) => Ok(())
        }
    }
}


impl TreePrinter for Expression
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        match *self
        {
            Expression::Literal(ref lit) => lit.print(level),
            Expression::UnaryOp(ref op) => {
                println!("{}unary {} ({})", p, op.operator, op.span);
                op.expression.print(level + 1)
            },
            Expression::BinaryOp(ref op) => {
                println!("{}binary {} ({}) (type: {})", p, op.operator, op.span, op.typ);
                op.left.print(level + 1);
                op.right.print(level + 1)
            },
            Expression::Block(ref b) => {
                println!("{}block ({}) (type: {})", p, b.span, b.typ);
                for e in &b.expressions {
                    e.print(level + 1);
                }
            },
            Expression::Call(ref c) => c.print(level),
            Expression::NameRef(ref nr) => nr.print(level),
            Expression::Match(ref m) => m.print(level),
            Expression::Lambda(ref l) => l.print(level),
            Expression::Bindings(ref l) => l.print(level),
            Expression::If(ref i) => i.print(level),
            Expression::StructInitializer(ref si) => si.print(level),
            Expression::MemberAccess(ref sma) => sma.print(level),
            Expression::New(ref n) => n.print(level),
            Expression::Delete(ref n) => n.print(level),
            Expression::ArrayToSlice(ref inner) => {
                println!("{}array to slice (type: {})", p, inner.slice_type);
                inner.inner.print(level + 1)
            },
            Expression::AddressOf(ref a) => a.print(level),
            Expression::Dereference(ref d) => d.print(level),
            Expression::Assign(ref a) => a.print(level),
            Expression::While(ref w) => w.print(level),
            Expression::For(ref f) => f.print(level),
            Expression::Nil(_) => println!("{}nil", p),
            Expression::OptionalToBool(ref n) => {
                println!("{}nil?", p);
                n.print(level + 1)
            },
            Expression::ToOptional(ref t) => {
                println!("{}to_optional (type: {})", p, t.optional_type);
                t.inner.print(level + 1)
            },
            Expression::Cast(ref t) => {
                println!("{}cast to {} ({})", p, t.destination_type, t.span);
                t.inner.print(level + 1)
            },
            Expression::CompilerCall(ref cc) => cc.print(level),
            Expression::IndexOperation(ref iop) => iop.print(level),
            Expression::Void => println!("{}void", p),
            Expression::Return(ref r) => {
                println!("{}return", p);
                r.expression.print(level + 1)
            }
        }
    }
}
