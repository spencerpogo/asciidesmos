use chumsky::prelude::*;

pub type Err = Simple<char, types::Span>;

macro_rules! to_binary_exprs {
    ($e:expr) => {
        ($e).foldl(|lhs: ast::LocatedExpression, (op, rhs)| {
            (
                lhs.0.with_end_of(&rhs.0).expect("Parsing the same file"),
                ast::Expression::BinaryExpr {
                    left: Box::new(lhs),
                    operator: op,
                    right: Box::new(rhs),
                },
            )
        })
    };
}

fn parser() -> impl Parser<char, ast::LocatedExpression, Error = Err> {
    recursive(|expr| {
        let int = text::int(10)
            .map_with_span(|s: String, span| -> ast::LocatedExpression {
                (span, ast::Expression::Num(s))
            })
            .padded();

        let ident = text::ident()
            .map_with_span(|s: String, span| (span, ast::Expression::Variable(s)))
            .padded();

        // parenthesis have highest precedence
        let atom = int
            .or(ident)
            .or(expr.clone().delimited_by(just('('), just(')')));

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

        let mult_divide = to_binary_exprs!(unary.clone().then(
            op('*')
                .to(ast::BinaryOperator::Multiply)
                .or(op('/').to(ast::BinaryOperator::Divide))
                .or(op('%').to(ast::BinaryOperator::Mod))
                .then(unary.clone())
                .repeated(),
        ));

        let add_sub = to_binary_exprs!(mult_divide.clone().then(
            op('+')
                .to(ast::BinaryOperator::Add)
                .or(op('-').to(ast::BinaryOperator::Subtract))
                .then(mult_divide)
                .repeated(),
        ));

        let call_inner = expr
            .clone()
            .then(just(',').padded().ignore_then(expr).repeated())
            .map(|(lhs, rhs)| std::iter::once(lhs).chain(rhs).collect())
            .or(empty().map(|_| vec![]));

        let call = text::ident::<char, Err>()
            .then(
                just('@')
                    .to(ast::CallModifier::MapCall)
                    .or(empty().to(ast::CallModifier::NormalCall)),
            )
            .then(call_inner.delimited_by(just('('), just(')')))
            .map_with_span(
                |((name, modifier), args): ((String, ast::CallModifier), _), s| {
                    (
                        s,
                        ast::Expression::Call {
                            modifier,
                            func: ast::Function::Normal { name },
                            args,
                        },
                    )
                },
            );

        call.or(add_sub).or(unary)
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

    fn var(s: &str) -> ast::Expression {
        ast::Expression::Variable(s.to_string())
    }

    #[test]
    fn basic_math() {
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
        check(
            "1*2 - 3/4 + 5%6",
            (
                s(0..15),
                ast::Expression::BinaryExpr {
                    left: Box::new((
                        s(0..9),
                        ast::Expression::BinaryExpr {
                            left: Box::new((
                                s(0..3),
                                ast::Expression::BinaryExpr {
                                    left: Box::new((s(0..1), num("1"))),
                                    operator: ast::BinaryOperator::Multiply,
                                    right: Box::new((s(2..3), num("2"))),
                                },
                            )),
                            operator: ast::BinaryOperator::Subtract,
                            right: Box::new((
                                s(6..9),
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

    #[test]
    fn variable() {
        check("a", (s(0..1), var("a")));
        check("_1", (s(0..2), var("_1")));
    }

    #[test]
    fn call() {
        check(
            "_a1(1+2, 3*4)",
            (
                s(0..13),
                ast::Expression::Call {
                    modifier: ast::CallModifier::NormalCall,
                    func: ast::Function::Normal {
                        name: "_a1".to_string(),
                    },
                    args: vec![
                        (
                            s(4..7),
                            ast::Expression::BinaryExpr {
                                left: Box::new((s(4..5), num("1"))),
                                operator: ast::BinaryOperator::Add,
                                right: Box::new((s(6..7), num("2"))),
                            },
                        ),
                        (
                            s(9..12),
                            ast::Expression::BinaryExpr {
                                left: Box::new((s(9..10), num("3"))),
                                operator: ast::BinaryOperator::Multiply,
                                right: Box::new((s(11..12), num("4"))),
                            },
                        ),
                    ],
                },
            ),
        );
    }

    #[test]
    fn mapcall() {
        check(
            "a@( 1,2 )",
            (
                s(0..9),
                ast::Expression::Call {
                    modifier: ast::CallModifier::MapCall,
                    func: ast::Function::Normal {
                        name: "a".to_string(),
                    },
                    args: vec![(s(4..5), num("1")), (s(6..7), num("2"))],
                },
            ),
        );
    }
}
