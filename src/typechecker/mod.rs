mod genericmapper;
mod instantiate;
mod instantiategenerics;
mod matchchecker;
#[cfg(test)]
mod tests;
mod typecheck;
mod typecheckercontext;
mod typeresolver;

pub use self::typecheck::type_check_module;
