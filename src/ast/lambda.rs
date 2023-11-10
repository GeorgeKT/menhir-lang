use crate::ast::{generic_type, prefix, sig, Argument, Expression, FunctionSignature, TreePrinter, Type};
use crate::compileerror::{type_error_result, CompileResult};
use crate::span::Span;
use serde_derive::{Deserialize, Serialize};

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct Lambda {
    pub sig: FunctionSignature,
    pub expr: Expression,
    pub span: Span,
}

pub fn lambda(args: Vec<Argument>, expr: Expression, span: Span) -> Expression {
    Expression::Lambda(Box::new(Lambda {
        sig: sig("lambda", generic_type("$ret$"), args, span.clone()),
        expr,
        span,
    }))
}

impl Lambda {
    pub fn is_generic(&self) -> bool {
        self.sig.args.iter().any(|a| a.typ.is_generic())
    }

    pub fn apply_type(&mut self, typ: &Type) -> CompileResult<()> {
        match *typ {
            Type::Func(ref ft) => {
                if ft.args.len() != self.sig.args.len() {
                    return type_error_result(
                        &self.span,
                        format!(
                            "Lambda expression has {} arguments, not {} arguments",
                            self.sig.args.len(),
                            ft.args.len()
                        ),
                    );
                }

                for (arg_typ, ref mut arg) in ft.args.iter().zip(self.sig.args.iter_mut()) {
                    if arg.typ.is_generic() {
                        arg.typ = arg_typ.clone();
                    } else if arg.typ != *arg_typ {
                        return type_error_result(
                            &self.span,
                            format!(
                                "Type mismatch in lambda expression, argument {}, has type {} not {}",
                                arg.name, arg.typ, arg_typ
                            ),
                        );
                    }
                }

                self.sig.return_type = ft.return_type.clone();
                self.sig.typ = typ.clone();
                Ok(())
            }
            _ => type_error_result(&self.span, format!("Lambda expression does not match the type {}", typ)),
        }
    }

    pub fn set_return_type(&mut self, return_type: Type) {
        self.sig.return_type = return_type;
    }
}

impl TreePrinter for Lambda {
    fn print(&self, level: usize) {
        let p = prefix(level);
        println!("{}lambda ({})", p, self.span);
        self.sig.print(level + 1);
        self.expr.print(level + 1);
    }
}
