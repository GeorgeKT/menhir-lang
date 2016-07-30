use compileerror::{Span, CompileResult, ErrorCode, err};
use ast::{Call, ArrayLiteral, ArrayInitializer, ArrayPattern, NameRef, BinaryOp, UnaryOp, Function,
    MatchExpression, TreePrinter, Lambda, prefix};

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Expression
{
    IntLiteral(Span, u64),
    BoolLiteral(Span, bool),
    FloatLiteral(Span, String), // Keep as string until we generate code, so we can compare it
    StringLiteral(Span, String),
    ArrayLiteral(ArrayLiteral),
    ArrayInitializer(ArrayInitializer), // [x; times]
    ArrayPattern(ArrayPattern), // [hd | tail]
    UnaryOp(UnaryOp),
    BinaryOp(Box<BinaryOp>),
    Enclosed(Span, Box<Expression>), // Expression enclosed between parens
    Call(Call),
    NameRef(NameRef),
    Function(Function),
    Match(MatchExpression),
    Lambda(Lambda),
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
            Expression::ArrayInitializer(ref a) => a.span,
            Expression::ArrayPattern(ref a) => a.span,
            Expression::UnaryOp(ref op) => op.span,
            Expression::BinaryOp(ref op) => op.span,
            Expression::Enclosed(span, _) => span,
            Expression::Call(ref c) => c.span,
            Expression::NameRef(ref nr) => nr.span,
            Expression::Function(ref f) => f.span,
            Expression::Match(ref m) => m.span,
            Expression::Lambda(ref l) => l.span,
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
            Expression::ArrayInitializer(ref a) => {
                println!("{}array initializer {} ({})", p, a.times, a.span);
                a.init.print(level + 1);
            },
            Expression::ArrayPattern(ref a) => {
                println!("{}array pattern [{} | {}] ({})", p, a.head, a.tail, a.span);
            },
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
            Expression::Function(ref f) => f.print(level),
            Expression::Match(ref m) => m.print(level),
            Expression::Lambda(ref l) => l.print(level),
        }
    }
}
