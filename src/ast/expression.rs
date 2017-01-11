use span::Span;
use ast::*;


#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Expression
{
    Literal(Literal),
    UnaryOp(Box<UnaryOp>),
    BinaryOp(Box<BinaryOp>),
    Block(Box<Block>),
    Call(Call),
    NameRef(NameRef),
    Match(Box<MatchExpression>),
    If(Box<IfExpression>),
    Lambda(Box<Lambda>),
    Let(Box<LetExpression>),
    LetBindings(Box<LetBindingList>),
    StructInitializer(StructInitializer),
    MemberAccess(MemberAccess),
    New(Box<NewExpression>),
    Delete(Box<DeleteExpression>),
    ArrayToSlice(Box<ArrayToSlice>),
    AddressOf(Box<AddressOfExpression>),
    Void,
}


impl Expression
{
    pub fn precedence(&self) -> usize
    {
        match *self
        {
            Expression::BinaryOp(ref op) => op.operator.precedence(),
            _ => 0,
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

    pub fn to_binary_op(self) -> Option<Box<BinaryOp>>
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
            Expression::Let(ref l) => l.span.clone(),
            Expression::LetBindings(ref l) => l.span.clone(),
            Expression::If(ref i) => i.span.clone(),
            Expression::StructInitializer(ref si) => si.span.clone(),
            Expression::MemberAccess(ref sma) => sma.span.clone(),
            Expression::New(ref n) => n.span.clone(),
            Expression::Delete(ref d) => d.span.clone(),
            Expression::ArrayToSlice(ref a) => a.inner.span(),
            Expression::AddressOf(ref a) => a.span.clone(),
            Expression::Void => Span::default(),
        }
    }

    pub fn get_type(&self) -> Type
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
            Expression::Let(ref l) => l.typ.clone(),
            Expression::LetBindings(ref l) => l.bindings.last().map(|b| b.typ.clone()).expect("Binding types are not known"),
            Expression::If(ref i) => i.typ.clone(),
            Expression::StructInitializer(ref si) => si.typ.clone(),
            Expression::MemberAccess(ref sma) => sma.typ.clone(),
            Expression::New(ref n) => n.typ.clone(),
            Expression::Delete(_) => Type::Void,
            Expression::ArrayToSlice(ref a) => a.slice_type.clone(),
            Expression::AddressOf(ref a) => ptr_type(a.inner.get_type()),
            Expression::Void => Type::Void,
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
            Expression::Let(ref l) => l.print(level),
            Expression::LetBindings(ref l) => l.print(level),
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
            Expression::Void => println!("{}void", p),
        }
    }
}
