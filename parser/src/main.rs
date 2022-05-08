use chumsky::prelude::*;

fn parser() -> impl Parser<char, ast::LocatedExpression, Error = Simple<char, types::Span>> {
    recursive(|expr| {
        let int = text::int(10)
            .map_with_span(|s: String, span| -> ast::LocatedExpression {
                (span, ast::Expression::Num(s))
            })
            .padded();

        let atom = int.or(expr.delimited_by(just('('), just(')')));

        atom
    })
    .then_ignore(end())
}

fn main() {
    let input = std::env::args().nth(1).unwrap();
    println!("{:#?}", parser().parse(input));
}
