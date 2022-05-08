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
        let atom = int.or(expr.delimited_by(just('('), just(')')));

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
            .or(atom.clone());

        let mult_divide = unary
            .clone()
            .then(
                op('*')
                    .to(ast::BinaryOperator::Multiply)
                    .or(op('/').to(ast::BinaryOperator::Divide))
                    .then(unary.clone())
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| -> ast::LocatedExpression {
                (
                    types::Span::new(0, 0..0),
                    ast::Expression::BinaryExpr {
                        left: Box::new(lhs),
                        operator: op,
                        right: Box::new(rhs),
                    },
                )
            });

        let add_sub = mult_divide
            .clone()
            .then(
                op('+')
                    .to(ast::BinaryOperator::Add)
                    .or(op('-').to(ast::BinaryOperator::Subtract))
                    .then(mult_divide)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| {
                (
                    types::Span::new(0, 0..0),
                    ast::Expression::BinaryExpr {
                        left: Box::new(lhs),
                        operator: op,
                        right: Box::new(rhs),
                    },
                )
            });

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

    fn check_result(l: &str, r: ParseResult) {
        assert_eq!(parse(0, l.to_string()), r);
    }

    fn check(l: &str, r: ast::LocatedExpression) {
        check_result(l, Ok(r));
    }

    fn s(r: std::ops::Range<usize>) -> types::Span {
        types::Span::new(0, r)
    }

    fn num(s: &str) -> ast::Expression {
        ast::Expression::Num(s.to_string())
    }

    #[test]
    fn basic_math() {
        check(
            "-1 * 2",
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
                    operator: ast::BinaryOperator::Multiply,
                    right: Box::new((s(5..6), num("2"))),
                },
            ),
        );
    }
}
