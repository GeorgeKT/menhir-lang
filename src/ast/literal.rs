use ast::{Type, ArrayLiteral, TreePrinter, FloatSize, IntSize, prefix};
use span::Span;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Literal
{
    Int(Span, i64, IntSize),
    UInt(Span, u64, IntSize),
    Bool(Span, bool),
    Char(Span, char),
    Float(Span, String, FloatSize), // Keep as string until we generate code, so we can compare it
    String(Span, String),
    Array(ArrayLiteral),
}

impl Literal
{
    pub fn get_type(&self) -> Type
    {
        match *self
        {
            Literal::Int(_, _, int_size) => Type::Int(int_size),
            Literal::UInt(_, _, int_size) => Type::UInt(int_size),
            Literal::Float(_, _, float_size) => Type::Float(float_size),
            Literal::Bool(_, _) => Type::Bool,
            Literal::Char(_, _) => Type::Char,
            Literal::String(_, _) => Type::String,
            Literal::Array(ref a) => a.array_type.clone(),
        }
    }

    pub fn span(&self) -> Span
    {
        match *self
        {
            Literal::Int(ref span, _, _) |
            Literal::UInt(ref span, _, _) |
            Literal::Float(ref span, _, _) |
            Literal::Bool(ref span, _) |
            Literal::Char(ref span, _) |
            Literal::String(ref span, _) => span.clone(),
            Literal::Array(ref a) => a.span.clone(),
        }
    }

    pub fn try_convert(&self, typ: &Type) -> Option<Literal>
    {
        match (self, typ) {
            (&Literal::Int(ref span, value, _), &Type::Int(int_size)) => {
                let target_bit_size = int_size.size_in_bits();
                let target_min = -2i64.pow(target_bit_size - 1);
                let target_max = (2u64.pow(target_bit_size - 1) - 1) as i64;
                if value >= target_min && value <= target_max {
                    Some(Literal::Int(span.clone(), value, int_size))
                } else {
                    None
                }
            }

            (&Literal::Int(ref span, value, _), &Type::UInt(int_size)) => {
                let target_bit_size = int_size.size_in_bits();
                if value >= 0 && (value as u64) < 2u64.pow(target_bit_size - 1) - 1 {
                    Some(Literal::UInt(span.clone(), value as u64, int_size))
                } else {
                    None
                }
            }

            (&Literal::UInt(ref span, value, _), &Type::Int(int_size)) => {
                let target_bit_size = int_size.size_in_bits();
                if value < 2u64.pow(target_bit_size) {
                    Some(Literal::Int(span.clone(), value as i64, int_size))
                } else {
                    None
                }
            }

            (&Literal::UInt(ref span, value, _), &Type::UInt(int_size)) => {
                let target_bit_size = int_size.size_in_bits();
                if value < 2u64.pow(target_bit_size) {
                    Some(Literal::UInt(span.clone(), value, int_size))
                } else {
                    None
                }
            }

            (&Literal::Float(ref span, ref value, FloatSize::F64), &Type::Float(FloatSize::F32)) => {
                use std::f32;
                // Number was already verified during parsing
                let v = value.parse::<f64>().expect("Invalid floating point number");
                if v >= (f32::MIN as f64) && v <= (f32::MAX as f64) {
                    Some(Literal::Float(span.clone(), value.clone(), FloatSize::F32))
                } else {
                    None
                }
            }

            (&Literal::Float(ref span, ref value, FloatSize::F32), &Type::Float(FloatSize::F64)) => {
                Some(Literal::Float(span.clone(), value.clone(), FloatSize::F64))
            }

            _ => None,
        }
    }
}

impl TreePrinter for Literal
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        match *self
        {
            Literal::Int(ref s, v, int_size) => println!("{}int{} {} ({})", p, int_size, v, s),
            Literal::UInt(ref s, v, int_size) => println!("{}uint{} {} ({})", p, int_size, v, s),
            Literal::Float(ref s, ref v, float_size) => println!("{}float{} {} ({})", p, float_size, v, s),
            Literal::Bool(ref s, v) => println!("{}bool {} ({})", p, v, s),
            Literal::Char(ref s, v) => println!("{}char {} ({})", p, v, s),
            Literal::String(ref s, ref v) => println!("{}string {} ({})", p, v, s),
            Literal::Array(ref a) => {
                println!("{}array ({})", p, a.span);
                for e in &a.elements {
                    e.print(level + 1);
                }
            },
        }
    }
}
