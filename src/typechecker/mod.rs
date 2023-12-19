mod destructors;
mod genericmapper;
mod instantiate;
mod instantiategenerics;
mod matchchecker;
mod patterns;
#[cfg(test)]
mod tests;
mod typecheck;
mod typecheckercontext;
mod typeresolver;

pub use self::typecheck::type_check_module;
