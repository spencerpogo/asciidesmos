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
        Rule::Variable => Ok(Expression::Variable { val: t.as_str() }),
        Rule::BinaryExpression => {
            let mut inner = t.into_inner();
            let left = process_token(try_unwrap(inner.next())?)?;
            let operator = try_unwrap(inner.next())?.as_str();
            let right = process_token(try_unwrap(inner.next())?)?;
            Ok(Expression::BinaryExpr {
                left: Box::new(left),
                operator: operator,
                right: Box::new(right),
            })
        }
        Rule::UnaryExpression => {
            let mut inner = t.into_inner();
            let val = process_token(try_unwrap(inner.next())?)?;
            let operator = try_unwrap(inner.next())?.as_str();
            Ok(Expression::UnaryExpr {
                val: Box::new(val),
                operator: operator,
            })
        }
        Rule::Call => {
            let mut inner = t.into_inner();
            let func = try_unwrap(inner.next())?.as_str();
            let arg_tokens_result = inner.next();

            match arg_tokens_result {
                // Arguments were supplied
                Some(arg_tokens) => {
                    // parse arguments
                    let mut args_ast = Vec::new();

                    for arg_token in arg_tokens.into_inner() {
                        args_ast.push(Box::new(process_token(arg_token)?));
                    }

                    Ok(Expression::Call {
                        func: func,
                        args: args_ast,
                    })
                }
                // No arguments were supplied
                _ => Ok(Expression::Call {
                    func: func,
                    args: Vec::new(),
                }),
            }
        }
        _ => unimplemented!(),
    }
}
