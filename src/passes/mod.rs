mod typechecker;
mod instantiategenerics;
mod genericmapper;
mod typeresolver;
#[cfg(test)]
mod tests;

pub use self::typeresolver::{resolve_types, resolve_function_args_and_ret_type};
pub use self::typechecker::{TypeCheckerContext, type_check_module};
pub use self::instantiategenerics::instantiate_generics;
pub use self::genericmapper::{substitute_types, fill_in_generics};
