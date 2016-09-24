mod typechecker;
mod typecheckercontext;
mod instantiategenerics;
mod genericmapper;
mod matchchecker;
mod typeresolver;
#[cfg(test)]
mod tests;

pub use self::typeresolver::{resolve_types};
pub use self::typechecker::{type_check_module};
pub use self::typecheckercontext::{TypeCheckerContext};
pub use self::instantiategenerics::instantiate_generics;
pub use self::genericmapper::{GenericMapper, fill_in_generics};
pub use self::matchchecker::check_match_is_exhaustive;
