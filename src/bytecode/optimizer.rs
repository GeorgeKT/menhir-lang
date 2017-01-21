use super::function::ByteCodeFunction;
use super::ByteCodeModule;

fn optimize_function(_func: &mut ByteCodeFunction)
{

}

pub fn optimize_module(module: &mut ByteCodeModule)
{
    for func in module.functions.values_mut() {
        optimize_function(func);
    }
}
