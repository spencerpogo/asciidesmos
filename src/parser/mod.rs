pub mod ast;
pub mod chars;
pub mod parser;

use nom;

pub type ParseResult<'a, T> = nom::IResult<&'a str, T>;
