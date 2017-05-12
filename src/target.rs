use ast::{IntSize, Type};

#[derive(Debug)]
pub struct Target
{
    pub int_size: IntSize,
    pub native_int_type: Type,
    pub native_uint_type: Type,
}

impl Target
{
    pub fn new(int_size: IntSize) -> Target
    {
        Target{
            int_size,
            native_int_type: Type::Int(int_size),
            native_uint_type: Type::UInt(int_size)
        }
    }
}
