mod typechecker;
mod marktailcalls;
mod instantiategenerics;
#[cfg(test)]
mod tests;

pub use self::typechecker::infer_and_check_types;
pub use self::marktailcalls::mark_tail_calls;
pub use self::instantiategenerics::instantiate_generics;
