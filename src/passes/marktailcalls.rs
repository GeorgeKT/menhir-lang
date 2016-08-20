use ast::{Expression, Module, Function};
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

fn mark_tail_call_in_function(ctx: &mut MarkTailCallContext, f: &mut Function) ->  CompileResult<()>
{
    ctx.push(&f.sig.name);
    try!(mark_tail_call(ctx, &mut f.expression));
    ctx.pop();
    Ok(())
}

fn mark_tail_call(ctx: &mut MarkTailCallContext, e: &mut Expression) -> CompileResult<()>
{
    match *e
    {
        Expression::Call(ref mut c) => {
            // We only get here if this is a tail call, so only check if it is recursive
            if ctx.is_recursive_call(&c.callee.name) {
                c.tail_call = true;
            }
            Ok(())
        },

        Expression::UnaryOp(ref mut op) => mark_tail_call(ctx, &mut op.expression),
        Expression::BinaryOp(ref mut op) => mark_tail_call(ctx, &mut op.right), // Only right can be a tail call

        Expression::Function(ref mut f) => {
            mark_tail_call_in_function(ctx, f)
        },

        Expression::Match(ref mut m) => {
            let mut mc = m.cases.last_mut().expect("No cases in match statement"); // Only the last case can have a tail call
            mark_tail_call(ctx, &mut mc.to_execute)
        },

        Expression::Enclosed(_, ref mut inner) => mark_tail_call(ctx, inner),
        Expression::Let(ref mut l) => mark_tail_call(ctx, &mut l.expression),
        _ => Ok(())
    }
}

/*
    Mark calls as tail calls
*/
pub fn mark_tail_calls(module: &mut Module) -> CompileResult<()>
{
    let mut ctx = MarkTailCallContext::new();
    for (_, ref mut f) in module.functions.iter_mut()
    {
        try!(mark_tail_call_in_function(&mut ctx, f));
    }

    Ok(())
}
