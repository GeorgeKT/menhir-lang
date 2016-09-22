use std::rc::Rc;
use codegen::*;
use codegen::expressions::gen_function_sig;
use ast::*;
use span::*;

pub fn add_builtin_functions(ctx: &mut Context)
{
    /*
    As defined in cobra-runtime:
    void* arc_alloc(size_t size);
    void arc_inc_ref(void* ptr);
    void arc_dec_ref(void* ptr);
    */

    let functions = vec![
        sig(
            "arc_alloc",
            Type::VoidPtr,
            vec![
                Argument::new("size".into(), Type::Int, Span::default())
            ],
            Span::default()
        ),
        sig(
            "arc_inc_ref",
            Type::Void,
            vec![
                Argument::new("ptr".into(), Type::VoidPtr, Span::default())
            ],
            Span::default()
        ),
        sig(
            "arc_dec_ref",
            Type::Void,
            vec![
                Argument::new("ptr".into(), Type::VoidPtr, Span::default())
            ],
            Span::default()
        )
    ];

    for func_sig in &functions {
        let instance = unsafe {
            gen_function_sig(ctx, &func_sig)
        };
        ctx.add_builtin(Rc::new(instance));
    }
}
