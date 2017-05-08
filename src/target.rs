use ast::{IntSize, Type};

pub trait Target
{
    fn int_size(&self) -> IntSize;
}

static mut NATIVE_INT_SIZE: IntSize = IntSize::I64;

pub fn native_int_size() -> IntSize
{
    unsafe {
        NATIVE_INT_SIZE
    }
}

pub fn native_int_type() -> Type
{
    unsafe {
        Type::Int(NATIVE_INT_SIZE)
    }
}

pub fn native_uint_type() -> Type
{
    unsafe {
        Type::UInt(NATIVE_INT_SIZE)
    }
}

pub fn register_target(target: &Target)
{
    unsafe {
        NATIVE_INT_SIZE = target.int_size();
    }
}