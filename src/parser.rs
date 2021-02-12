use crate::types::{AssertionError, Expression, LocatedExpression};
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

pub fn process_token(t: Pair<'_, Rule>) -> Result<LocatedExpression, AssertionError> {
    println!("{:#?}", t);

    let s = t.as_span();
    match t.as_rule() {
        Rule::Program => process_token(try_unwrap(t.into_inner().next())?),
        Rule::ExpressionNoList | Rule::Expression => {
            process_token(try_unwrap(t.into_inner().next())?)
        }
        Rule::Term => process_token(try_unwrap(t.into_inner().next())?),
        Rule::Number => Ok((s, Expression::Num { val: t.as_str() })),
        Rule::Variable => Ok((s, Expression::Variable { val: t.as_str() })),
        Rule::BinaryExpression => {
            let mut inner = t.into_inner();
            let left = process_token(try_unwrap(inner.next())?)?;
            let operator = try_unwrap(inner.next())?.as_str();
            let right = process_token(try_unwrap(inner.next())?)?;
            Ok((
                s,
                Expression::BinaryExpr {
                    left: Box::new(left),
                    operator: operator,
                    right: Box::new(right),
                },
            ))
        }
        Rule::UnaryExpression => {
            let mut inner = t.into_inner();
            let val = process_token(try_unwrap(inner.next())?)?;
            let operator = try_unwrap(inner.next())?.as_str();
            Ok((
                s,
                Expression::UnaryExpr {
                    val: Box::new(val),
                    operator: operator,
                },
            ))
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

                    Ok((
                        s,
                        Expression::Call {
                            func: func,
                            args: args_ast,
                        },
                    ))
                }
                // No arguments were supplied
                _ => Ok((
                    s,
                    Expression::Call {
                        func: func,
                        args: Vec::new(),
                    },
                )),
            }
        }
        Rule::List => {
            let mut inner = t.into_inner();

            match inner.next() {
                None => Ok((s, Expression::List(vec![]))),
                Some(val_tokens) => {
                    let mut vals = Vec::new();

                    for v in val_tokens.into_inner() {
                        vals.push(Box::new(process_token(v)?))
                    }

                    Ok((s, Expression::List(vals)))
                }
            }
        }
        _ => unimplemented!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pest::Span;

    macro_rules! parse_test {
        ($i:expr, $r:expr) => {
            assert_eq!(
                process_token(parse($i).unwrap().next().unwrap()).unwrap(),
                (spn($i, 0, $i.len()), $r)
            );
        };
    }

    #[inline]
    fn spn<'a>(i: &'a str, start: usize, end: usize) -> Span<'a> {
        Span::new(i, start, end).unwrap()
    }

    #[test]
    fn number() {
        macro_rules! num_test {
            ($v:expr) => {
                parse_test!($v, Expression::Num { val: $v });
            };
        }

        num_test!("1");
        num_test!("-2");
        num_test!("+3");
    }

    #[test]
    fn variable() {
        parse_test!("w3c", Expression::Variable { val: "w3c" });
        assert_eq!(parse("3wc").is_err(), true);
    }

    #[test]
    fn binary_expression() {
        let i = "1 + 2";
        parse_test!(
            i,
            Expression::BinaryExpr {
                left: Box::new((spn(i, 0, 1), Expression::Num { val: "1" })),
                operator: "+",
                right: Box::new((spn(i, 4, 5), Expression::Num { val: "2" }))
            }
        );
    }

    #[test]
    fn long_binary_expression() {
        let i = "1 + 2 + 3";
        parse_test!(
            i,
            Expression::BinaryExpr {
                left: Box::new((spn(i, 0, 1), Expression::Num { val: "1" })),
                operator: "+",
                right: Box::new((
                    spn(i, 4, 9),
                    Expression::BinaryExpr {
                        left: Box::new((spn(i, 4, 5), Expression::Num { val: "2" })),
                        operator: "+",
                        right: Box::new((spn(i, 8, 9), Expression::Num { val: "3" }))
                    }
                ))
            }
        );
    }

    #[test]
    fn unary_expression() {
        let i = "1!";
        parse_test!(
            i,
            Expression::UnaryExpr {
                val: Box::new((spn(i, 0, 1), Expression::Num { val: "1" })),
                operator: "!",
            }
        );
    }

    #[test]
    fn call() {
        parse_test!(
            "a()",
            Expression::Call {
                func: "a",
                args: Vec::new(),
            }
        );
        let j = "a(1, 2, 3)";
        parse_test!(
            j,
            Expression::Call {
                func: "a",
                args: vec![
                    Box::new((spn(j, 2, 3), Expression::Num { val: "1" })),
                    Box::new((spn(j, 5, 6), Expression::Num { val: "2" })),
                    Box::new((spn(j, 8, 9), Expression::Num { val: "3" })),
                ]
            }
        );
    }

    #[test]
    fn list() {
        let i = "[1, 2,3]";
        parse_test!(
            i,
            Expression::List(vec![
                Box::new((spn(i, 1, 2), Expression::Num { val: "1" })),
                Box::new((spn(i, 4, 5), Expression::Num { val: "2" })),
                Box::new((spn(i, 6, 7), Expression::Num { val: "3" })),
            ])
        );
    }
}
