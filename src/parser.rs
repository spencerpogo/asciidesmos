use crate::types::{AssertionError, Expression};
use pest::{
    error::Error as PestError,
    iterators::{Pair, Pairs},
    Parser,
};
use pest_derive;

#[derive(pest_derive::Parser)]
#[grammar = "grammar.pest"] // relative to src
pub struct DesmosParser;

pub fn parse(i: &str) -> Result<Pairs<'_, Rule>, PestError<Rule>> {
    DesmosParser::parse(Rule::Program, i)
}

pub fn try_unwrap<T>(i: Option<T>) -> Result<T, AssertionError> {
    i.ok_or(AssertionError)
}

pub fn process_token(t: Pair<'_, Rule>) -> Result<Expression, AssertionError> {
    println!("{:#?}", t);
    match t.as_rule() {
        Rule::Expression => process_token(try_unwrap(t.into_inner().next())?),
        Rule::Number => Ok(Expression::Num { val: t.as_str() }),
        _ => unimplemented!(),
    }
}
