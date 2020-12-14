use std::{
    fmt,
    error
};

#[derive(Debug, Eq, PartialEq)]
pub enum Error {
    PointerBelowZero,
    PointerAboveLimit,
    UnbalancedBrackets,
    InfiniteLoop,
}

impl error::Error for Error {}
impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Error::*;
        match self {
            PointerBelowZero => write!(f, "Error: mem pointer went below zero"),
            PointerAboveLimit => write!(f, "Error: mem pointer went above limit"),
            UnbalancedBrackets => write!(f, "Error: unbalanced brackets in source"),
            InfiniteLoop => write!(f, "Error: potential infinite loop in source"),
        }
    }
}