use chumsky::prelude::*;

fn parser() -> impl Parser<char, ast::LocatedExpression, Error = Simple<char, types::Span>> {
    recursive(|expr| {
        let int = text::int(10)
            .map_with_span(|s: String, span| -> ast::LocatedExpression {
                (span, ast::Expression::Num(s))
            })
            .padded();

        // parenthesis have highest precedence
        let atom = int.or(expr.delimited_by(just('('), just(')')));

        let op = |c| just(c).padded();

        let unary = op('-').then(atom.clone()).map_with_span(|(_, v), s| {
            (
                s,
                ast::Expression::UnaryExpr {
                    val: Box::new(v),
                    operator: ast::UnaryOperator::Negate,
                },
            )
        });

        unary.or(atom)
    })
    .then_ignore(end())
}

fn main() {
    let input = std::env::args().nth(1).unwrap();
    // TODO: Use slab crate to keep track of filenames
    let source: types::FileID = 0;
    let s: chumsky::Stream<'_, char, types::Span, _> = chumsky::Stream::from_iter(
        types::Span::new(source, input.len()..input.len()),
        input
            .chars()
            .enumerate()
            .map(|(i, x)| (x, types::Span::new(source, i..i + 1))),
    );
    println!("{:#?}", parser().parse(s));
}
