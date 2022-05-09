use chumsky::prelude::*;

pub type Err = Simple<char, types::Span>;

fn parser() -> impl Parser<char, ast::LocatedExpression, Error = Err> {
    recursive(|expr| {
        let int = text::int(10)
            .map_with_span(|s: String, span| -> ast::LocatedExpression {
                (span, ast::Expression::Num(s))
            })
            .padded();

        // parenthesis have highest precedence
        let atom = int.or(expr.clone().delimited_by(just('('), just(')')));

        let op = |c| just(c).padded();

        let unary = op('-')
            .then(atom.clone())
            .map_with_span(|(_, v), s| {
                (
                    s,
                    ast::Expression::UnaryExpr {
                        val: Box::new(v),
                        operator: ast::UnaryOperator::Negate,
                    },
                )
            })
            .or(atom);

        let mult_divide = unary
            .clone()
            .then(
                op('*')
                    .to(ast::BinaryOperator::Multiply)
                    .or(op('/').to(ast::BinaryOperator::Divide))
                    .or(op('%').to(ast::BinaryOperator::Mod))
                    .then(unary.clone())
                    .map(
                        |v| -> Option<(ast::BinaryOperator, ast::LocatedExpression)> {
                            Option::Some(v)
                        },
                    )
                    .or(empty().to(Option::None)),
            )
            .map_with_span(
                |(lhs, maybe_rhs): (
                    ast::LocatedExpression,
                    Option<(ast::BinaryOperator, ast::LocatedExpression)>,
                ),
                 s| {
                    match maybe_rhs {
                        Some((op, rhs)) => (
                            s,
                            ast::Expression::BinaryExpr {
                                left: Box::new(lhs),
                                operator: op,
                                right: Box::new(rhs),
                            },
                        ),
                        None => lhs,
                    }
                },
            );

        let add_sub = mult_divide
            .clone()
            .then(
                op('+')
                    .to(ast::BinaryOperator::Add)
                    .or(op('-').to(ast::BinaryOperator::Subtract))
                    .then(mult_divide)
                    .map(Option::Some)
                    .or(empty().to(Option::None)),
            )
            .map_with_span(
                |(lhs, maybe_rhs): (
                    ast::LocatedExpression,
                    Option<(ast::BinaryOperator, ast::LocatedExpression)>,
                ),
                 s| {
                    match maybe_rhs {
                        Some((op, rhs)) => (
                            s,
                            ast::Expression::BinaryExpr {
                                left: Box::new(lhs),
                                operator: op,
                                right: Box::new(rhs),
                            },
                        ),
                        None => lhs,
                    }
                },
            );

        add_sub.or(unary)
    })
    .then_ignore(end())
}

pub type ParseResult = Result<ast::LocatedExpression, Vec<Err>>;

fn parse(source: types::FileID, input: String) -> ParseResult {
    let s: chumsky::Stream<'_, char, types::Span, _> = chumsky::Stream::from_iter(
        types::Span::new(source, input.len()..input.len()),
        input
            .chars()
            .enumerate()
            .map(|(i, x)| (x, types::Span::new(source, i..i + 1))),
    );
    parser().parse(s)
}

fn main() {
    let input = std::env::args().nth(1).unwrap();
    // TODO: Use slab crate to keep track of filenames
    println!("{:#?}", parse(0, input));
}

#[cfg(test)]
mod tests {
    use super::*;
    const FILENO: usize = 1234;

    fn check_result(l: &str, r: ParseResult) {
        assert_eq!(parse(FILENO, l.to_string()), r);
    }

    fn check(l: &str, r: ast::LocatedExpression) {
        check_result(l, Ok(r));
    }

    fn s(r: std::ops::Range<usize>) -> types::Span {
        types::Span::new(FILENO, r)
    }

    fn num(s: &str) -> ast::Expression {
        ast::Expression::Num(s.to_string())
    }

    #[test]
    fn basic_math() {
        println!("basic_math");
        check(
            "-1 + 2",
            (
                s(0..6),
                ast::Expression::BinaryExpr {
                    left: Box::new((
                        s(0..3),
                        ast::Expression::UnaryExpr {
                            val: Box::new((s(1..2), num("1"))),
                            operator: ast::UnaryOperator::Negate,
                        },
                    )),
                    operator: ast::BinaryOperator::Add,
                    right: Box::new((s(5..6), num("2"))),
                },
            ),
        );
    }

    #[test]
    fn precedence() {
        println!("precedence");
        println!("{:#?}", parse(1337, "1*2 - 3/4 + 5%6".to_string()));
        check(
            "1*2 - 3/4 + 5%6",
            (
                s(0..15),
                ast::Expression::BinaryExpr {
                    left: Box::new((
                        s(0..4),
                        ast::Expression::BinaryExpr {
                            left: Box::new((
                                s(0..1),
                                ast::Expression::BinaryExpr {
                                    left: Box::new((s(0..1), num("1"))),
                                    operator: ast::BinaryOperator::Multiply,
                                    right: Box::new((s(2..3), num("2"))),
                                },
                            )),
                            operator: ast::BinaryOperator::Subtract,
                            right: Box::new((
                                s(6..10),
                                ast::Expression::BinaryExpr {
                                    left: Box::new((s(6..7), num("3"))),
                                    operator: ast::BinaryOperator::Divide,
                                    right: Box::new((s(8..9), num("4"))),
                                },
                            )),
                        },
                    )),
                    operator: ast::BinaryOperator::Add,
                    right: Box::new((
                        s(12..15),
                        ast::Expression::BinaryExpr {
                            left: Box::new((s(12..13), num("5"))),
                            operator: ast::BinaryOperator::Mod,
                            right: Box::new((s(14..15), num("6"))),
                        },
                    )),
                },
            ),
        )
    }
}
