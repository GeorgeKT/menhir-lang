use std::rc::{Rc, Weak};
use std::cell::RefCell;
use std::fmt;
use super::*;

#[derive(Debug, Clone)]
pub enum ValueRef
{
    Owner(Rc<RefCell<Value>>),
    Ptr(Weak<RefCell<Value>>),
    Null,
}
impl ValueRef
{
    pub fn new(v: Value) -> ValueRef
    {
        ValueRef::Owner(Rc::new(RefCell::new(v)))
    }

    pub fn clone_value(&self) -> Result<Value, ExecutionError>
    {
        match *self
        {
            ValueRef::Owner(ref v) => Ok(v.borrow().clone()),
            ValueRef::Ptr(ref v) => {
                if let Some(rv) = v.upgrade() {
                    Ok(rv.borrow().clone())
                } else {
                    Err(ExecutionError(format!("Dangling pointer, owner of element pointed to is gone")))
                }
            }
            ValueRef::Null => {
                Err(ExecutionError(format!("Dangling pointer, pointer has been deleted")))
            }
        }
    }

    pub fn to_ptr(&self) -> ValueRef
    {
        if let ValueRef::Owner(ref v) = *self {
            ValueRef::Ptr(Rc::downgrade(v))
        } else {
            self.clone()
        }
    }

    pub fn apply<Op, R>(&self, op: Op) -> Result<R, ExecutionError>
        where Op: Fn(&Value) -> Result<R, ExecutionError>, R: Sized
    {
        match *self
        {
            ValueRef::Owner(ref v) => {
                op(&v.borrow())
            },
            ValueRef::Ptr(ref v) => {
                if let Some(rv) = v.upgrade() {
                    op(&rv.borrow())
                } else {
                    Err(ExecutionError(format!("Dangling pointer, owner of element pointed to is gone")))
                }
            }
            ValueRef::Null => {
                Err(ExecutionError(format!("Dangling pointer, pointer has been deleted")))
            }
        }
    }
}

impl fmt::Display for ValueRef
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error>
    {
        match *self
        {
            ValueRef::Owner(ref v) => {
                let inner = v.borrow();
                write!(f, "{}", *inner)
            },

            ValueRef::Ptr(ref v) => {
                if let Some(rv) = v.upgrade() {
                    let inner = rv.borrow();
                    write!(f, "{}", *inner)
                } else {
                    println!("Danging pointer access");
                    Err(fmt::Error)
                }
            },

            ValueRef::Null => {
                write!(f, "null")
            },
        }
    }
}
