use ast::{Expression, Module};
use compileerror::{CompileResult};

struct MarkTailCallContext
{
    function_stack: Vec<String>
}

impl MarkTailCallContext
{
    pub fn new() -> MarkTailCallContext
    {
        MarkTailCallContext{
            function_stack: Vec::new()
        }
    }

    pub fn push(&mut self, name: &str)
    {
        self.function_stack.push(name.into())
    }

    pub fn pop(&mut self)
    {
        self.function_stack.pop().expect("Stack is empty");
    }

    pub fn is_recursive_call(&self, name: &str) -> bool
    {
        self.function_stack.last().map(|f| f == name).unwrap_or(false)
    }
}

fn mark_tail_call(ctx: &mut MarkTailCallContext, e: &mut Expression) -> CompileResult<()>
{
    match *e
    {
        Expression::Call(ref mut c) => {
            if ctx.is_recursive_call(&c.callee.name) {
                c.tail_call = true;
            }
            Ok(())
        },

        Expression::UnaryOp(ref mut op) => mark_tail_call(ctx, &mut op.expression),
        Expression::BinaryOp(ref mut op) => mark_tail_call(ctx, &mut op.right),

        Expression::Function(ref mut f) => {
            ctx.push(&f.sig.name);
            try!(mark_tail_call(ctx, &mut f.expression));
            ctx.pop();
            Ok(())
        },

        Expression::Match(ref mut m) => {
            let mut mc = m.cases.last_mut().expect("No cases in match statement");
            mark_tail_call(ctx, &mut mc.to_execute)
        },

        Expression::Enclosed(_, ref mut inner) => mark_tail_call(ctx, inner),
        Expression::Let(ref mut l) => mark_tail_call(ctx, &mut l.expression),
        _ => Ok(())
    }
}

/*
    Type check and infer all the unkown types
*/
pub fn mark_tail_calls(module: &mut Module) -> CompileResult<()>
{
    let mut ctx = MarkTailCallContext::new();
    for ref mut e in module.expressions.iter_mut()
    {
        try!(mark_tail_call(&mut ctx, e));
    }

    Ok(())
}
