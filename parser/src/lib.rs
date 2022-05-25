use chumsky::prelude::*;

pub type LexErr = Simple<char, types::Span>;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    Num(String),
    Ident(String),
    OpMinus,
    OpPlus,
    OpMult,
    OpDiv,
    OpMod,
    OpCmpLt,
    OpCmpLe,
    OpCmpGt,
    OpCmpGe,
    OpCmpEq,
    OpEq,
    CtrlLParen,
    CtrlRParen,
    CtrlListStart,
    CtrlListEnd,
    CtrlComma,
    CtrlMap,
    CtrlThen,
    CtrlSemi,
    KeywordWhere,
    KeywordOtherwise,
}

fn lexer() -> impl Parser<char, Vec<ast::Spanned<Token>>, Error = LexErr> {
    let int = text::int(10).map(Token::Num);

    let mkop = |c, t| just(c).to(t);
    let op = just("<=")
        .to(Token::OpCmpLe)
        .or(mkop('>', Token::OpCmpGt))
        .or(just(">=").to(Token::OpCmpGe))
        .or(just("==").to(Token::OpCmpEq))
        .or(mkop('-', Token::OpMinus))
        .or(mkop('+', Token::OpPlus))
        .or(mkop('*', Token::OpMult))
        .or(mkop('/', Token::OpDiv))
        .or(mkop('%', Token::OpMod))
        .or(mkop('<', Token::OpCmpLt))
        .or(mkop('=', Token::OpEq));

    let ctrl = just("->")
        .to(Token::CtrlThen)
        .or(mkop('(', Token::CtrlLParen))
        .or(mkop(')', Token::CtrlRParen))
        .or(mkop('[', Token::CtrlListStart))
        .or(mkop(']', Token::CtrlListEnd))
        .or(mkop(',', Token::CtrlComma))
        .or(mkop('@', Token::CtrlMap))
        .or(mkop(';', Token::CtrlSemi));

    let ident = text::ident().map(|i: String| match i.as_str() {
        "where" => Token::KeywordWhere,
        "otherwise" => Token::KeywordOtherwise,
        _ => Token::Ident(i),
    });

    let token = int.or(ctrl).or(op).or(ident);

    token.map_with_span(|t, span| (span, t)).padded().repeated()
}

pub type ParseErr = Simple<Token, types::Span>;

fn expr_parser() -> impl Parser<Token, ast::LocatedExpression, Error = ParseErr> + Clone {
    recursive(|expr: Recursive<Token, ast::LocatedExpression, _>| {
        let comma_joined_exprs = expr
            .clone()
            .then(just(Token::CtrlComma).ignore_then(expr.clone()).repeated())
            .map(|(first, rest)| std::iter::once(first).chain(rest).collect())
            .or(empty().map(|_| vec![]));
        let call = select! {
            Token::Ident(name) => name,
        }
        .then(
            just(Token::CtrlMap)
                .to(ast::CallModifier::MapCall)
                .or(empty().to(ast::CallModifier::NormalCall)),
        )
        .then(
            comma_joined_exprs
                .clone()
                .delimited_by(just(Token::CtrlLParen), just(Token::CtrlRParen)),
        )
        .map_with_span(|((func, modifier), args), s| {
            (
                s,
                ast::Expression::Call {
                    modifier,
                    func: ast::Function::Normal { name: func },
                    args,
                },
            )
        });

        let list = comma_joined_exprs
            .delimited_by(just(Token::CtrlListStart), just(Token::CtrlListEnd))
            .map_with_span(|v, s| (s, ast::Expression::List(v)));

        let val = select! {
            Token::Num(n) => ast::Expression::Num(n),
            Token::Ident(i) => ast::Expression::Variable(i)
        }
        .map_with_span(|v, s| (s, v));

        let atom = list.or(call).or(val).or(expr
            .clone()
            .delimited_by(just(Token::CtrlLParen), just(Token::CtrlRParen)));

        let negate = just(Token::OpMinus)
            .ignore_then(atom.clone())
            .map_with_span(|v, s| {
                (
                    s,
                    ast::Expression::UnaryExpr {
                        val: Box::new(v),
                        operator: ast::UnaryOperator::Negate,
                    },
                )
            })
            .or(atom.clone());

        macro_rules! binop {
            ($prev:expr, $op:expr) => {
                $prev
                    .clone()
                    .then($op.then($prev).repeated())
                    .foldl(|l, (op, r)| {
                        (
                            l.0.with_end_of(&r.0).expect("Parsing the same file"),
                            ast::Expression::BinaryExpr {
                                left: Box::new(l),
                                operator: op,
                                right: Box::new(r),
                            },
                        )
                    })
            };
        }

        let product = binop!(
            negate,
            just(Token::OpMult)
                .to(ast::BinaryOperator::Multiply)
                .or(just(Token::OpDiv).to(ast::BinaryOperator::Divide))
                .or(just(Token::OpMod).to(ast::BinaryOperator::Mod))
        );

        let sum = binop!(
            product,
            just(Token::OpPlus)
                .to(ast::BinaryOperator::Add)
                .or(just(Token::OpMinus).to(ast::BinaryOperator::Subtract))
        );

        let cond_op = just(Token::OpCmpLt)
            .to(types::CompareOperator::LessThan)
            .or(just(Token::OpCmpLe).to(types::CompareOperator::LessThanEqual))
            .or(just(Token::OpCmpGt).to(types::CompareOperator::GreaterThan))
            .or(just(Token::OpCmpGt).to(types::CompareOperator::GreaterThanEqual))
            .or(just(Token::OpCmpEq).to(types::CompareOperator::Equal));
        let cond = atom
            .clone()
            .then(cond_op)
            .then(atom.clone())
            .map(|((l, op), r)| (l, op, r));
        let branch = cond
            .then_ignore(just(Token::CtrlThen))
            .then(atom.clone())
            .map_with_span(|((cond_left, cond, cond_right), val), s| {
                (
                    s,
                    ast::Branch {
                        cond_left,
                        cond,
                        cond_right,
                        val,
                    },
                )
            });
        let otherwise_branch = just(Token::KeywordOtherwise).ignore_then(atom);
        let where_block = just(Token::KeywordWhere)
            .ignore_then(branch.clone())
            .then(just(Token::CtrlComma).ignore_then(branch).repeated())
            .then(just(Token::CtrlComma).ignore_then(otherwise_branch))
            .map_with_span(|((first, rest), default), s| {
                (
                    s,
                    ast::Expression::Piecewise {
                        first: Box::new(first),
                        rest,
                        default: Box::new(default),
                    },
                )
            });

        where_block.or(sum)
    })
}

fn statement_parser() -> impl Parser<Token, Vec<ast::Spanned<ast::Statement>>, Error = ParseErr> {
    let expr = expr_parser();
    let expr_stmt = expr
        .clone()
        .map(|(s, e)| (s, ast::Statement::Expression(e)));

    let declaration = select! {
        Token::Ident(i) => i,
    }
    .then_ignore(just(Token::OpEq))
    .then(expr)
    .map_with_span(|(name, val), s| (s, ast::Statement::VarDef(name, val)));

    let line = declaration.or(expr_stmt).then_ignore(just(Token::CtrlSemi));

    line.repeated().collect().then_ignore(end())
}

pub type LexErrors = Vec<LexErr>;
pub type LexResult = Result<Vec<ast::Spanned<Token>>, LexErrors>;
pub type ParseErrors = Vec<ParseErr>;
pub type ExprParseResult = Result<ast::Spanned<ast::Statement>, ParseErrors>;
pub type ParseResult = Result<Vec<ast::Spanned<ast::Statement>>, ParseErrors>;

pub fn lex(source: types::FileID, input: String) -> LexResult {
    let s: chumsky::Stream<'_, char, types::Span, _> = chumsky::Stream::from_iter(
        types::Span::new(source, input.len()..input.len()),
        input
            .chars()
            .enumerate()
            .map(|(i, x)| (x, types::Span::new(source, i..i + 1))),
    );
    lexer().parse(s)
}

pub fn parse(source: types::FileID, tokens: Vec<ast::Spanned<Token>>) -> ParseResult {
    statement_parser().parse(chumsky::Stream::from_iter(
        types::Span::new(source, tokens.len()..tokens.len() + 1),
        tokens.into_iter().map(|(s, t)| (t, s)),
    ))
}

#[derive(Clone, Debug, PartialEq)]
pub enum LexParseErrors {
    LexErrors(LexErrors),
    ParseErrors(ParseErrors),
}

impl From<LexErrors> for LexParseErrors {
    fn from(err: LexErrors) -> Self {
        Self::LexErrors(err)
    }
}

impl From<ParseErrors> for LexParseErrors {
    fn from(err: ParseErrors) -> Self {
        Self::ParseErrors(err)
    }
}

pub type LexParseResult = Result<Vec<ast::Spanned<ast::Statement>>, LexParseErrors>;

pub fn lex_and_parse(source: types::FileID, input: String) -> LexParseResult {
    let tokens = lex(source, input)?;
    let r = parse(source, tokens)?;
    Ok(r)
}

#[cfg(test)]
mod tests {
    use super::*;
    const FILENO: usize = 1234;

    fn check_result(l: &str, r: LexParseResult) {
        assert_eq!(lex_and_parse(FILENO, l.to_string()), r);
    }

    fn check(l: &str, expr: ast::LocatedExpression) {
        let (spn, expr) = expr;
        check_result(l, Ok(vec![(spn, ast::Statement::Expression(expr))]));
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
            "-1 + 2;",
            (
                s(0..6),
                ast::Expression::BinaryExpr {
                    left: Box::new((
                        s(0..2),
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
            "1*2 - 3/4 + 5%6;",
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
        check("a;", (s(0..1), var("a")));
        check("_1;", (s(0..2), var("_1")));
    }

    #[test]
    fn call() {
        check(
            "_a1(1+2, 3*4);",
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
            "a@( b(1),2 );",
            (
                s(0..12),
                ast::Expression::Call {
                    modifier: ast::CallModifier::MapCall,
                    func: ast::Function::Normal {
                        name: "a".to_string(),
                    },
                    args: vec![
                        (
                            s(4..8),
                            ast::Expression::Call {
                                modifier: ast::CallModifier::NormalCall,
                                func: ast::Function::Normal {
                                    name: "b".to_string(),
                                },
                                args: vec![(s(6..7), num("1"))],
                            },
                        ),
                        (s(9..10), num("2")),
                    ],
                },
            ),
        );
    }

    #[test]
    fn list() {
        check(
            "[ 1 , 2 ];",
            (
                s(0..9),
                ast::Expression::List(vec![(s(2..3), num("1")), (s(6..7), num("2"))]),
            ),
        );
        check(
            "[ a( 1 ) ];",
            (
                s(0..10),
                ast::Expression::List(vec![(
                    s(2..8),
                    ast::Expression::Call {
                        modifier: ast::CallModifier::NormalCall,
                        func: ast::Function::Normal {
                            name: "a".to_string(),
                        },
                        args: vec![(s(5..6), num("1"))],
                    },
                )]),
            ),
        )
    }

    #[test]
    fn piecewise() {
        check(
            "where a > 1 -> b , c < 2 -> d , otherwise e;",
            (
                s(0..43),
                ast::Expression::Piecewise {
                    first: Box::new((
                        s(6..16),
                        ast::Branch {
                            cond_left: (s(6..7), var("a")),
                            cond: types::CompareOperator::GreaterThan,
                            cond_right: (s(10..11), num("1")),
                            val: (s(15..16), var("b")),
                        },
                    )),
                    rest: vec![(
                        s(19..29),
                        ast::Branch {
                            cond_left: (s(19..20), var("c")),
                            cond: types::CompareOperator::LessThan,
                            cond_right: (s(23..24), num("2")),
                            val: (s(28..29), var("d")),
                        },
                    )],
                    default: Box::new((s(42..43), var("e"))),
                },
            ),
        );
    }
}
