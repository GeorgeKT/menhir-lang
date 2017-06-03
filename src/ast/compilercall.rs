use ast::{Type, IntSize, TreePrinter, prefix};
use span::Span;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum CompilerCall
{
    SizeOf(Type, Span)
}


impl CompilerCall
{
    pub fn get_type(&self, int_size: IntSize) -> Type
    {
        match *self {
            CompilerCall::SizeOf(_, _) => Type::UInt(int_size)
        }
    }
}

impl TreePrinter for CompilerCall
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        match *self {
            CompilerCall::SizeOf(ref typ, ref span) => println!("{}@size({}) ({})", p, typ, span)
        }
    }
}