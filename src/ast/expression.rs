use compileerror::{Span, CompileResult, ErrorCode, err};
use ast::*;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Expression
{
    IntLiteral(Span, u64),
    BoolLiteral(Span, bool),
    FloatLiteral(Span, String), // Keep as string until we generate code, so we can compare it
    StringLiteral(Span, String),
    ArrayLiteral(ArrayLiteral),
    ArrayPattern(ArrayPattern), // [hd | tail]
    EmptyArrayPattern(EmptyArrayPattern),
    ArrayGenerator(Box<ArrayGenerator>),
    UnaryOp(Box<UnaryOp>),
    BinaryOp(Box<BinaryOp>),
    Enclosed(Span, Box<Expression>), // Expression enclosed between parens
    Call(Call),
    NameRef(NameRef),
    Match(Box<MatchExpression>),
    If(Box<IfExpression>),
    Lambda(Box<Lambda>),
    Let(Box<LetExpression>),
    StructInitializer(StructInitializer),
    StructMemberAccess(StructMemberAccess),
    StructPattern(StructPattern),
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
            Expression::IntLiteral(span, _) => span,
            Expression::FloatLiteral(span, _) => span,
            Expression::BoolLiteral(span, _) => span,
            Expression::StringLiteral(span, _) => span,
            Expression::ArrayLiteral(ref a) => a.span,
            Expression::ArrayGenerator(ref a) => a.span,
            Expression::ArrayPattern(ref a) => a.span,
            Expression::EmptyArrayPattern(ref a) => a.span,
            Expression::UnaryOp(ref op) => op.span,
            Expression::BinaryOp(ref op) => op.span,
            Expression::Enclosed(span, _) => span,
            Expression::Call(ref c) => c.span,
            Expression::NameRef(ref nr) => nr.span,
            Expression::Match(ref m) => m.span,
            Expression::Lambda(ref l) => l.span,
            Expression::Let(ref l) => l.span,
            Expression::If(ref i) => i.span,
            Expression::StructInitializer(ref si) => si.span,
            Expression::StructMemberAccess(ref sma) => sma.span,
            Expression::StructPattern(ref p) => p.span,
        }
    }

    pub fn get_type(&self) -> Type
    {
        match *self
        {
            Expression::IntLiteral(_, _) => Type::Int,
            Expression::FloatLiteral(_, _) => Type::Float,
            Expression::BoolLiteral(_, _) => Type::Bool,
            Expression::StringLiteral(_, _) => array_type(Type::Char),
            Expression::ArrayLiteral(ref a) => a.array_type.clone(),
            Expression::ArrayGenerator(ref a) => a.array_type.clone(),
            Expression::ArrayPattern(_) => Type::Unknown,
            Expression::EmptyArrayPattern(_) => Type::Unknown,
            Expression::UnaryOp(ref op) => op.typ.clone(),
            Expression::BinaryOp(ref op) => op.typ.clone(),
            Expression::Enclosed(_, ref e) => e.get_type(),
            Expression::Call(ref c) => c.return_type.clone(),
            Expression::NameRef(ref nr) => nr.typ.clone(),
            Expression::Match(ref m) => m.typ.clone(),
            Expression::Lambda(ref l) => l.sig.return_type.clone(),
            Expression::Let(ref l) => l.typ.clone(),
            Expression::If(ref i) => i.typ.clone(),
            Expression::StructInitializer(ref si) => si.typ.clone(),
            Expression::StructMemberAccess(ref sma) => sma.typ.clone(),
            Expression::StructPattern(_) => Type::Unknown,
        }
    }

    pub fn to_name_ref(self) -> CompileResult<NameRef>
    {
        match self
        {
            Expression::NameRef(nr) => Ok(nr),
            _ => err(self.span().start, ErrorCode::TypeError, format!("Expected name reference")),
        }
    }


    pub fn to_pattern(self) -> Expression
    {
        match self
        {
            Expression::StructInitializer(si) => si.to_struct_pattern(),
            Expression::ArrayLiteral(al) => {
                if al.elements.is_empty() {
                    empty_array_pattern(al.span)
                } else {
                    Expression::ArrayLiteral(al)
                }
            }
            _ => self,
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
            Expression::BoolLiteral(ref span, b) => {
                println!("{}bool {} ({})", p, b, span);
            },

            Expression::IntLiteral(ref span, integer) => {
                println!("{}int {} ({})", p, integer, span);
            },
            Expression::FloatLiteral(ref span, ref s) => {
                println!("{}float {} ({})", p, s, span);
            },
            Expression::StringLiteral(ref span, ref s) => {
                println!("{}string \"{}\" ({})", p, s, span);
            },
            Expression::ArrayLiteral(ref a) => {
                println!("{}array ({})", p, a.span);
                for e in &a.elements {
                    e.print(level + 1);
                }
            },
            Expression::ArrayPattern(ref a) => {
                println!("{}array pattern [{} | {}] ({})", p, a.head, a.tail, a.span);
            },
            Expression::EmptyArrayPattern(ref a) => {
                println!("{}empty array pattern [] ({})", p, a.span);
            },
            Expression::ArrayGenerator(ref a) => a.print(level),
            Expression::UnaryOp(ref op) => {
                println!("{}unary {} ({})", p, op.operator, op.span);
                op.expression.print(level + 1)
            },
            Expression::BinaryOp(ref op) => {
                println!("{}binary {} ({})", p, op.operator, op.span);
                op.left.print(level + 1);
                op.right.print(level + 1)
            },
            Expression::Enclosed(ref span, ref e) => {
                println!("{}enclosed ({})", p, span);
                e.print(level + 1);
            },
            Expression::Call(ref c) => c.print(level),
            Expression::NameRef(ref nr) => nr.print(level),
            Expression::Match(ref m) => m.print(level),
            Expression::Lambda(ref l) => l.print(level),
            Expression::Let(ref l) => l.print(level),
            Expression::If(ref i) => i.print(level),
            Expression::StructInitializer(ref si) => si.print(level),
            Expression::StructMemberAccess(ref sma) => sma.print(level),
            Expression::StructPattern(ref p) => p.print(level),
        }
    }
}
