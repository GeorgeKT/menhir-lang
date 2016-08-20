use ast::{Expression, Argument, TreePrinter, FunctionSignature, Type, prefix, sig};
use compileerror::{CompileResult, ErrorCode, Span, err};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Lambda
{
    pub sig: FunctionSignature,
    pub expr: Box<Expression>,
    pub span: Span,
}

pub fn lambda(args: Vec<Argument>, expr: Expression, span: Span) -> Lambda
{
    Lambda{
        sig: sig("lambda", Type::Unknown, args, span),
        expr: Box::new(expr),
        span: span,
    }
}

impl Lambda
{
    pub fn is_generic(&self) -> bool
    {
        self.sig.args.iter().any(|a| a.typ.is_generic())
    }

    pub fn apply_type(&mut self, typ: &Type) -> CompileResult<()>
    {
        match *typ
        {
            Type::Func(ref args, ref ret) => {
                if args.len() != self.sig.args.len() {
                    return err(self.span.start, ErrorCode::LambdaDoesNotMatch,
                        format!("Lambda expression has {} arguments, not {} arguments", self.sig.args.len(), args.len()));
                }

                for (arg_typ, ref mut arg) in args.iter().zip(self.sig.args.iter_mut())
                {
                    if arg.typ.is_generic() {
                        arg.typ = arg_typ.clone();
                    } else if arg.typ != *arg_typ {
                        return err(self.span.start, ErrorCode::TypeError,
                            format!("Type mismatch in lambda expression, argument {}, has type {} not {}",
                                arg.name, arg.typ, arg_typ));
                    }
                }

                use std::ops::Deref;
                self.sig.return_type = ret.deref().clone();
                Ok(())
            },
            _ => err(self.span.start, ErrorCode::LambdaDoesNotMatch,
                format!("Lambda expression does not match the type {}", typ)),
        }
    }

    pub fn set_return_type(&mut self, return_type: Type)
    {
        self.sig.return_type = return_type;
    }
}

impl TreePrinter for Lambda
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        println!("{}lambda ({})", p, self.span);
        self.sig.print(level + 1);
    }
}
