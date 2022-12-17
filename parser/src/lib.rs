use chumsky::prelude::*;

pub type LexErr = Simple<char, types::Span>;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    Num(String),
    Ident(String),
    Str(String),
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
    OpExp,
    OpEq,
    OpColon,
    CtrlLParen,
    CtrlRParen,
    CtrlLBrac,
    CtrlRBrac,
    CtrlComma,
    CtrlMap,
    CtrlThen,
    CtrlSemi,
    CtrlGci,
    CtrlEllipses,
    KeywordWhere,
    KeywordElse,
    KeywordInline,
    KeywordImport,
    KeywordFrom,
    KeywordInclude,
    KeywordLatex,
    KeywordLatexList,
}

impl Token {
    pub fn to_str(self) -> &'static str {
        use Token::*;
        match self {
            Num(_) => "number",
            Ident(_) => "identifier",
            Str(_) => "string",
            OpMinus => "`-`",
            OpPlus => "`+`",
            OpMult => "`*`",
            OpDiv => "`/`",
            OpMod => "`%`",
            OpCmpLt => "`<`",
            OpCmpLe => "`<=`",
            OpCmpGt => "`>`",
            OpCmpGe => "`>=`",
            OpCmpEq => "`=`",
            OpExp => "`**`",
            OpEq => "`=`",
            OpColon => "`:`",
            CtrlLParen => "`(`",
            CtrlRParen => "`)`",
            CtrlLBrac => "`[`",
            CtrlRBrac => "`]`",
            CtrlComma => "`,`",
            CtrlMap => "`@`",
            CtrlThen => "`->`",
            CtrlSemi => "`;`",
            CtrlGci => "`.`",
            CtrlEllipses => "`...`",
            KeywordWhere => "`where`",
            KeywordElse => "`else`",
            KeywordInline => "`inline`",
            KeywordImport => "`import`",
            KeywordFrom => "`from`",
            KeywordInclude => "`include`",
            KeywordLatex => "`latex`",
            KeywordLatexList => "`latex_list`",
        }
    }
}

fn lexer() -> impl Parser<char, Vec<ast::Spanned<Token>>, Error = LexErr> {
    let int = text::int(10).map(Token::Num);

    let p_str = just('\"')
        .ignore_then(filter(|c| *c != '"' && *c != '\n').repeated())
        .then_ignore(just('\"'))
        .collect::<String>()
        .map(|s| Token::Str(s));

    let mkop = |c: char, t: Token| just(c).to(t);
    let mkops = |s: &'static str, t: Token| just(s).to(t);
    let op = just("<=")
        .to(Token::OpCmpLe)
        .or(mkop('>', Token::OpCmpGt))
        .or(mkops(">=", Token::OpCmpGe))
        .or(mkops("==", Token::OpCmpEq))
        .or(mkops("**", Token::OpExp))
        .or(mkop('-', Token::OpMinus))
        .or(mkop('+', Token::OpPlus))
        .or(mkop('*', Token::OpMult))
        .or(mkop('/', Token::OpDiv))
        .or(mkop('%', Token::OpMod))
        .or(mkop('<', Token::OpCmpLt))
        .or(mkop('=', Token::OpEq))
        .or(mkop(':', Token::OpColon));

    let ctrl = just("->")
        .to(Token::CtrlThen)
        .or(mkop('(', Token::CtrlLParen))
        .or(mkop(')', Token::CtrlRParen))
        .or(mkop('[', Token::CtrlLBrac))
        .or(mkop(']', Token::CtrlRBrac))
        .or(mkop(',', Token::CtrlComma))
        .or(mkop('@', Token::CtrlMap))
        .or(mkop(';', Token::CtrlSemi))
        .or(mkops("...", Token::CtrlEllipses))
        .or(mkop('.', Token::CtrlGci));

    let ident = text::ident().map(|i: String| match i.as_str() {
        "where" => Token::KeywordWhere,
        "else" => Token::KeywordElse,
        "inline" => Token::KeywordInline,
        "import" => Token::KeywordImport,
        "from" => Token::KeywordFrom,
        "include" => Token::KeywordInclude,
        "latex" => Token::KeywordLatex,
        "latex_list" => Token::KeywordLatexList,
        _ => Token::Ident(i),
    });

    let token = int
        .or(p_str)
        .or(ctrl)
        .or(op)
        .or(ident)
        .recover_with(skip_then_retry_until([]));

    token
        .map_with_span(|t, span| (span, t))
        .padded()
        .repeated()
        .then_ignore(end())
}

pub type ParseErr = Simple<Token, types::Span>;

fn expr_parser() -> impl Parser<Token, ast::LocatedExpression, Error = ParseErr> + Clone {
    recursive(|expr: Recursive<Token, ast::LocatedExpression, _>| {
        let call = select! {
            Token::Ident(name) => name,
        }
        .then(
            just(Token::CtrlMap)
                .to(ast::CallModifier::MapCall)
                .or(empty().to(ast::CallModifier::NormalCall)),
        )
        .then(
            expr.clone()
                .separated_by(just(Token::CtrlComma))
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

        let range = expr
            .clone()
            .map(Box::new)
            .then_ignore(just(Token::CtrlComma))
            .then(
                expr.clone()
                    .then_ignore(just(Token::CtrlComma))
                    .map(Box::new)
                    .map(Option::Some)
                    .or(empty().to(Option::None)),
            )
            .then_ignore(just(Token::CtrlEllipses))
            .then_ignore(just(Token::CtrlComma))
            .then(expr.clone().map(Box::new))
            .delimited_by(just(Token::CtrlLBrac), just(Token::CtrlRBrac))
            .map_with_span(|((first, second), end), s| {
                (s, ast::Expression::Range { first, second, end })
            });

        let list = expr
            .clone()
            .separated_by(just(Token::CtrlComma))
            .delimited_by(just(Token::CtrlLBrac), just(Token::CtrlRBrac))
            .map_with_span(|v, s| (s, ast::Expression::List(v)));

        let ident = select! {
            Token::Ident(i) => i,
        };
        // this isn't the best
        let qualified_var = ident
            .then_ignore(just(Token::CtrlGci))
            .then(ident.separated_by(just(Token::CtrlGci)))
            .map_with_span(|(first, rest), s| {
                if rest.is_empty() {
                    return (s, ast::Expression::Variable(first));
                }
                let mut path = rest;
                path.insert(0, first);
                let (last, path) = path.split_last().unwrap();
                (
                    s,
                    ast::Expression::FullyQualifiedVariable {
                        path: path.to_vec(),
                        item: last.clone(),
                    },
                )
            });

        let val = select! {
            Token::Num(n) => ast::Expression::Num(n),
            Token::Ident(i) => ast::Expression::Variable(i)
        }
        .map_with_span(|v, s| (s, v));

        let atom = range
            .or(list)
            .or(call)
            .or(qualified_var)
            .or(val)
            .or(expr
                .clone()
                .delimited_by(just(Token::CtrlLParen), just(Token::CtrlRParen)))
            .recover_with(nested_delimiters(
                Token::CtrlLParen,
                Token::CtrlRParen,
                [(Token::CtrlLBrac, Token::CtrlRBrac)],
                |span| (span, ast::Expression::Error),
            ))
            .recover_with(nested_delimiters(
                Token::CtrlLBrac,
                Token::CtrlRBrac,
                [(Token::CtrlLParen, Token::CtrlRParen)],
                |span| (span, ast::Expression::Error),
            ));

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

        let map = just(Token::CtrlMap)
            .ignore_then(negate.clone())
            .map_with_span(|v, s| (s, ast::Expression::Map(Box::new(v))))
            .or(negate);

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

        let exponent = binop!(map, just(Token::OpExp).to(ast::BinaryOperator::Exponent));

        let product = binop!(
            exponent,
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

        let ind = sum
            .clone()
            .then(
                just(Token::CtrlLBrac)
                    .ignore_then(sum)
                    .then_ignore(just(Token::CtrlRBrac))
                    .map(Option::Some)
                    .or(empty().to(Option::None)),
            )
            .map_with_span(|(sum, maybe_ind), s| match maybe_ind {
                Some(ind) => (
                    s,
                    ast::Expression::Index {
                        val: Box::new(sum),
                        ind: Box::new(ind),
                    },
                ),
                None => sum,
            });

        let cond_op = just(Token::OpCmpLt)
            .to(types::CompareOperator::LessThan)
            .or(just(Token::OpCmpLe).to(types::CompareOperator::LessThanEqual))
            .or(just(Token::OpCmpGt).to(types::CompareOperator::GreaterThan))
            .or(just(Token::OpCmpGt).to(types::CompareOperator::GreaterThanEqual))
            .or(just(Token::OpCmpEq).to(types::CompareOperator::Equal));
        let cond = expr
            .clone()
            .then(cond_op)
            .then(expr.clone())
            .map(|((l, op), r)| (l, op, r));
        let branch = cond
            .then_ignore(just(Token::CtrlThen))
            .then(expr.clone())
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
        let else_branch = just(Token::KeywordElse).ignore_then(expr);
        let where_block = just(Token::KeywordWhere)
            .ignore_then(branch.clone())
            .then(just(Token::CtrlComma).ignore_then(branch).repeated())
            .then(just(Token::CtrlComma).ignore_then(else_branch))
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

        let p_str = select! {
            Token::Str(s) => s,
        };
        let latex = just(Token::KeywordLatex)
            .to(types::ValType::Number)
            .or(just(Token::KeywordLatexList).to(types::ValType::List))
            .then(p_str)
            .map_with_span(|(ty, l), s| (s, ast::Expression::RawLatex(ty, l)));

        where_block.or(ind).or(latex)
    })
}

fn statement_parser() -> impl Parser<Token, Vec<ast::Spanned<ast::Statement>>, Error = ParseErr> {
    let expr = expr_parser();
    let expr_stmt = expr
        .clone()
        .map(|(s, e)| (s, ast::Statement::Expression(e)));

    let ident = select! {
        Token::Ident(i) => i,
    };
    let type_annotation = just(Token::OpColon)
        .ignore_then(ident.clone())
        .try_map(|typ, span| match typ.as_str() {
            "num" => Ok(types::ValType::Number),
            "list" => Ok(types::ValType::List),
            _ => Err(Simple::custom(
                span,
                format!("Invalid type '{}', expected 'num' or 'list'", typ),
            )),
        });
    let arg = ident
        .then(type_annotation.or(empty().to(types::ValType::Number)))
        .map_with_span(|(name, ty), s| (s, name, ty));
    let inline = just(Token::KeywordInline).or_not().map(|t| t.is_some());
    let func_dec = inline
        .clone()
        .then(ident.clone())
        .then(
            arg.separated_by(just(Token::CtrlComma))
                .delimited_by(just(Token::CtrlLParen), just(Token::CtrlRParen)),
        )
        .then_ignore(just(Token::OpEq))
        .then(expr.clone())
        .map_with_span(|(((inline, name), args), expr), s| {
            (
                s,
                ast::Statement::FuncDef(
                    ast::FunctionDefinition {
                        name,
                        args,
                        ret_annotation: None,
                        inline,
                    },
                    expr,
                ),
            )
        });
    let declaration = inline
        .then(ident)
        .then_ignore(just(Token::OpEq))
        .then(expr)
        .map_with_span(|((inline, name), val), s| {
            (s, ast::Statement::VarDef { name, val, inline })
        });

    let p_str = select! {
        Token::Str(s) => s,
    };
    let import = just(Token::KeywordImport)
        .ignore_then(ident)
        .then_ignore(just(Token::KeywordFrom))
        .then(p_str)
        .map_with_span(|(name, path), s| {
            (
                s,
                ast::Statement::Import(ast::Import {
                    mode: ast::ImportMode::Import { name },
                    path,
                }),
            )
        });
    let include = just(Token::KeywordInclude)
        .ignore_then(p_str)
        .map_with_span(|path, s| {
            (
                s,
                ast::Statement::Import(ast::Import {
                    mode: ast::ImportMode::Include,
                    path,
                }),
            )
        });

    let line = import
        .or(include)
        .or(func_dec)
        .or(declaration)
        .or(expr_stmt);

    line.clone()
        .then(just(Token::CtrlSemi).ignore_then(line.or_not()).repeated())
        .then_ignore(end())
        .map(|(first, rest)| {
            std::iter::once(first)
                .chain(rest.into_iter().flatten())
                .collect()
        })
}

pub type LexErrors = Vec<LexErr>;
pub type Tokens = Vec<ast::Spanned<Token>>;
pub type LexResult = (Option<Tokens>, LexErrors);
pub type ParseErrors = Vec<ParseErr>;
pub type ParseResult = (Option<ast::LStatements>, ParseErrors);

pub fn lex(source: types::FileID, input: String) -> LexResult {
    let s: chumsky::Stream<'_, char, types::Span, _> = chumsky::Stream::from_iter(
        types::Span::new(source, input.len()..input.len() + 1),
        input
            .chars()
            .enumerate()
            .map(|(i, x)| (x, types::Span::new(source, i..i + 1))),
    );
    lexer().parse_recovery(s)
}

pub fn parse(source: types::FileID, tokens: Vec<ast::Spanned<Token>>) -> ParseResult {
    statement_parser().parse_recovery(chumsky::Stream::from_iter(
        types::Span::new(source, tokens.len()..tokens.len() + 1),
        tokens.into_iter().map(|(s, t)| (t, s)),
    ))
}

#[derive(Clone, Debug, PartialEq, Default)]
pub struct LexParseErrors {
    pub lex_errors: LexErrors,
    pub parse_errors: ParseErrors,
}

impl LexParseErrors {
    pub fn new() -> Self {
        Self {
            lex_errors: vec![],
            parse_errors: vec![],
        }
    }

    pub fn is_empty(&self) -> bool {
        self.lex_errors.is_empty() && self.parse_errors.is_empty()
    }
}

impl From<LexErrors> for LexParseErrors {
    fn from(errs: LexErrors) -> Self {
        Self {
            lex_errors: errs,
            parse_errors: vec![],
        }
    }
}

impl From<ParseErrors> for LexParseErrors {
    fn from(errs: ParseErrors) -> Self {
        Self {
            lex_errors: vec![],
            parse_errors: errs,
        }
    }
}

pub type LexParseResult = (Option<ast::LStatements>, LexParseErrors);

// this function is not very error tolerant!
pub fn lex_and_parse(source: types::FileID, input: String) -> LexParseResult {
    let (tokens, lex_errs) = lex(source, input);
    if !lex_errs.is_empty() {
        return (None, lex_errs.into());
    }
    let (ast, errs) = parse(source, tokens.unwrap());
    (ast, errs.into())
}

#[cfg(test)]
mod tests {
    use super::*;
    const FILENO: usize = 1234;

    fn eval(l: &str) -> LexParseResult {
        lex_and_parse(FILENO, l.to_string())
    }

    fn check_result(l: &str, r: LexParseResult) {
        assert_eq!(eval(l), r);
    }

    fn check(l: &str, expr: ast::LocatedExpression) {
        let (spn, expr) = expr;
        check_result(
            l,
            (
                Some(vec![(spn, ast::Statement::Expression(expr))]),
                LexParseErrors::new(),
            ),
        );
    }

    fn check_stmt(l: &str, stmt: ast::Spanned<ast::Statement>) {
        check_result(l, (Some(vec![stmt]), LexParseErrors::new()));
    }

    fn assert_parses(l: &str) {
        assert_eq!(eval(l).1, LexParseErrors::new());
    }

    fn assert_does_not_parse(l: &str) {
        assert_ne!(eval(l).1, LexParseErrors::new());
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
        assert_parses("a() = 1;");
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
    fn type_annotations() {
        assert_parses("a ( x , y : num , z : list) = 1;");
        assert_does_not_parse("a ( x : str ) = 0;");
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
            "where a > 1 -> b , c < 2 -> d , else e;",
            (
                s(0..38),
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
                    default: Box::new((s(37..38), var("e"))),
                },
            ),
        );
    }

    #[test]
    fn funcdef() {
        check_stmt(
            "func( xy : num , yz : list ) = 7;",
            (
                s(0..32),
                ast::Statement::FuncDef(
                    ast::FunctionDefinition {
                        name: "func".to_string(),
                        args: vec![
                            (s(6..14), "xy".to_string(), types::ValType::Number),
                            (s(17..26), "yz".to_string(), types::ValType::List),
                        ],
                        ret_annotation: None,
                        inline: false,
                    },
                    (s(31..32), num("7")),
                ),
            ),
        );
    }

    #[test]
    fn inline_funcdef() {
        check_stmt(
            "inline func( xy : num , yz : list ) = 7;",
            (
                s(0..39),
                ast::Statement::FuncDef(
                    ast::FunctionDefinition {
                        name: "func".to_string(),
                        args: vec![
                            (s(13..21), "xy".to_string(), types::ValType::Number),
                            (s(24..33), "yz".to_string(), types::ValType::List),
                        ],
                        ret_annotation: None,
                        inline: true,
                    },
                    (s(38..39), num("7")),
                ),
            ),
        );
    }

    #[test]
    fn import() {
        check_stmt(
            "import abcd from \"efgh\";",
            (
                s(0..23),
                ast::Statement::Import(ast::Import {
                    mode: ast::ImportMode::Import {
                        name: "abcd".to_string(),
                    },
                    path: "efgh".to_string(),
                }),
            ),
        );
    }

    #[test]
    fn ind_prec() {
        assert_parses("sin@([0])[1]");
    }

    #[test]
    fn range() {
        check(
            "[1,...,2]",
            (
                s(0..9),
                ast::Expression::Range {
                    first: Box::new((s(1..2), num("1"))),
                    second: None,
                    end: Box::new((s(7..8), num("2"))),
                },
            ),
        );
        check(
            "[1,2,...,3]",
            (
                s(0..11),
                ast::Expression::Range {
                    first: Box::new((s(1..2), num("1"))),
                    second: Some(Box::new((s(3..4), num("2")))),
                    end: Box::new((s(9..10), num("3"))),
                },
            ),
        );
        assert_does_not_parse("[1,2,3...,4]");
        assert_does_not_parse("[1,...,2,3");
        assert_does_not_parse("[1,...,2,...,3]");
    }
}
