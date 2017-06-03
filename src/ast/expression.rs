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
    Binding(Box<BindingExpression>),
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
            Expression::Binding(ref l) => l.span.clone(),
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
            Expression::Binding(ref l) => l.typ.clone(),
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
            Expression::Void |
            Expression::While(_) |
            Expression::Delete(_) |
            Expression::For(_) => Type::Void,
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
            Expression::Binding(ref l) => l.print(level),
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
            Expression::Void => println!("{}void", p),
        }
    }
}
