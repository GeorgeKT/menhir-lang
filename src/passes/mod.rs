mod typechecker;
mod marktailcalls;
#[cfg(test)]
mod tests;

pub use self::typechecker::infer_and_check_types;
pub use self::marktailcalls::mark_tail_calls;
