use std::fmt;

use itertools::join;

use super::scope::Scope;
use super::stack::StackPtr;
use crate::ast::{FunctionSignature, TreeFormatter};

#[derive(Debug)]
pub struct ByteCodeFunction {
    pub sig: FunctionSignature,
    pub external: bool,
    pub toplevel_scope: Scope,
}

impl ByteCodeFunction {
    pub fn new(sig: &FunctionSignature, external: bool, stack: StackPtr) -> ByteCodeFunction {
        let mut sig = sig.clone();
        sig.do_rvo();

        ByteCodeFunction {
            sig: sig.clone(),
            external,
            toplevel_scope: Scope::new(sig, stack),
        }
    }
}

impl fmt::Display for ByteCodeFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        writeln!(
            f,
            "{}({}) -> {}:",
            self.sig.name,
            join(
                self.sig
                    .args
                    .iter()
                    .map(|arg| format!("{}: {}", arg.name, arg.typ)),
                ", "
            ),
            self.sig.return_type
        )?;
        self.toplevel_scope.fmt(0, f)
    }
}
