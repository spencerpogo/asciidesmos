use chumsky::prelude::*;

#[derive(Debug)]
enum Expr {
    Num(f64),
    /*Var(String),

    Neg(Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),

    Call(String, Vec<Expr>),
    Let {
        name: String,
        rhs: Box<Expr>,
        then: Box<Expr>,
    },
    Fn {
        name: String,
        args: Vec<String>,
        body: Box<Expr>,
        then: Box<Expr>,
    },*/
}

fn parser() -> impl Parser<char, Expr, Error = Simple<char>> {
    recursive(|expr| {
        let int = text::int(10)
            .map(|s: String| Expr::Num(s.parse().unwrap()))
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