#![forbid(unsafe_code)]

pub mod ast;
pub mod error;
pub mod lexer;
pub mod parser;
pub mod interpreter;

pub const ARRAY_LEN: usize = 30_000;
pub const EOF: u8 = 0;