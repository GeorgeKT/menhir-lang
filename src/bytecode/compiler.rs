use ast::*;
use bytecode::*;

fn func_to_bc(sig: &FunctionSignature, expression: &Expression) -> ByteCodeFunction
{
    let mut llfunc = ByteCodeFunction::new(&sig);
    match expr_to_bc(&mut llfunc, &expression)
    {
        Some(var) => {
            llfunc.add(ret_instr(&var));
        },

        None => {
            llfunc.add(Instruction::ReturnVoid);
        }
    }
    llfunc
}

pub fn compile_to_byte_code(md: &Module) -> ByteCodeModule
{
    let mut ll_mod = ByteCodeModule{
        name: md.name.clone(),
        functions: Vec::new(),
    };

    for func in md.externals.values() {
        ll_mod.functions.push(ByteCodeFunction::new(&func.sig));
    }

    for func in md.functions.values() {
        if !func.is_generic() {
            ll_mod.functions.push(func_to_bc(&func.sig, &func.expression));
        }
    }

    ll_mod
}
