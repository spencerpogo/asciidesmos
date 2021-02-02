use pest::{error::Error as PestError, iterators::Pairs, Parser};
use pest_derive;

#[derive(pest_derive::Parser)]
#[grammar = "grammar.pest"] // relative to src
struct DesmosParser;

fn parse(i: &str) -> Result<Pairs<'_, Rule>, PestError<Rule>> {
  DesmosParser::parse(Rule::Program, i)
}
