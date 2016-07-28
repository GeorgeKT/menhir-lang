mod typechecker;
#[cfg(test)]
mod tests;

pub use self::typechecker::infer_and_check_types;
