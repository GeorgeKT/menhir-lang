extern crate itertools;
extern crate uuid;
extern crate shrust;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate serde_cbor;


pub mod ast;
pub mod compileerror;
pub mod bytecode;
pub mod parser;
pub mod typechecker;
#[cfg(test)] mod testcode;
pub mod span;