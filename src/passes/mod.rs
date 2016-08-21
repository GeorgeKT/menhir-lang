mod typechecker;
mod instantiategenerics;
mod genericmapper;
#[cfg(test)]
mod tests;

pub use self::typechecker::infer_and_check_types;
pub use self::instantiategenerics::instantiate_generics;
pub use self::genericmapper::{substitute_types, fill_in_generics};
