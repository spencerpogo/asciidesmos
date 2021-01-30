pub mod ast;
pub mod chars;
pub mod parser;
pub mod simple;

use nom;

pub type ParseResult<'a, T> = nom::IResult<&'a str, T>;
