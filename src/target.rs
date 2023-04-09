use crate::ast::{IntSize, Type};

#[derive(Debug)]
pub struct Target {
    pub int_size: IntSize,
    pub native_int_type: Type,
    pub native_uint_type: Type,
    pub triplet: String,
}

impl Target {
    pub fn new<S: Into<String>>(int_size: IntSize, triplet: S) -> Target {
        Target {
            int_size,
            native_int_type: Type::Int(int_size),
            native_uint_type: Type::UInt(int_size),
            triplet: triplet.into(),
        }
    }
}
