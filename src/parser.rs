use crate::types::ValType;
use pest::Span;
use pest_consume::{match_nodes, Error, Node as PestNode, Parser as PestConsumeParser};

// pest + result = pesult ;)
type Pesult<T> = std::result::Result<T, Error<Rule>>;
type Node<'i> = PestNode<'i, Rule, ()>;

// Expression is a component of a statement
#[derive(Clone, Debug, PartialEq)]
pub enum Expression<'a> {
    Num {
        val: &'a str,
    },
    Variable {
        val: &'a str,
    },
    BinaryExpr {
        left: Box<LocatedExpression<'a>>,
        // Should probably make an enum for this, but its not worth the work to encode
        //  it just to stringify it again later
        operator: &'a str,
        right: Box<LocatedExpression<'a>>,
    },
    UnaryExpr {
        val: Box<LocatedExpression<'a>>,
        operator: &'a str,
    },
    Call {
        func: &'a str,
        args: Vec<Box<LocatedExpression<'a>>>,
    },
    List(Vec<Box<LocatedExpression<'a>>>),
}

pub type LocatedExpression<'a> = (Span<'a>, Expression<'a>);

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionDefinition<'a> {
    pub name: &'a str,
    pub args: Vec<(&'a str, ValType)>,
    pub ret_annotation: Option<ValType>,
}

// A statement is a part of a program
#[derive(Clone, Debug, PartialEq)]
pub enum Statement<'a> {
    FuncDef(FunctionDefinition<'a>, LocatedExpression<'a>),
    Expression(Expression<'a>),
}

pub type LocatedStatement<'a> = (Span<'a>, Statement<'a>);

#[allow(clippy::upper_case_acronyms)]
#[derive(PestConsumeParser)]
#[grammar = "grammar.pest"] // relative to src
pub struct DesmosParser;

impl DesmosParser {
    // Shared rules
    fn arguments(input: Node) -> Pesult<Vec<Box<LocatedExpression>>> {
        let r = match_nodes!(
            input.into_children();
            [Expression(e)..] => e,
            [ExpressionNoList(e)..] => e,
        )
        .map(|t| Box::new(t))
        .collect();
        Ok(r)
    }

    fn expression(input: Node) -> Pesult<LocatedExpression> {
        Ok(match_nodes!(
            input.into_children();
            [List(n)] => n,
            [UnaryExpression(n)] => n,
            [BinaryExpression(n)] => n,
            [Term(n)] => n,
        ))
    }
}

#[pest_consume::parser]
impl DesmosParser {
    fn EOI(_input: Node) -> Pesult<()> {
        Ok(())
    }

    fn ExpressionNoList(input: Node) -> Pesult<LocatedExpression> {
        Self::expression(input)
    }

    fn Expression(input: Node) -> Pesult<LocatedExpression> {
        Self::expression(input)
    }

    fn Term(input: Node) -> Pesult<LocatedExpression> {
        Ok(match_nodes!(
            input.into_children();
            [Expression(e)] => e,
            [Number(n)] => n,
            [Variable(n)] => n,
            [Call(c)] => c,
        ))
    }

    fn UnaryOperator(input: Node) -> Pesult<&str> {
        Ok(input.as_str())
    }

    fn UnaryExpression(input: Node) -> Pesult<LocatedExpression> {
        let s = input.as_span();
        Ok(match_nodes!(
            input.into_children();
            [Term(t), UnaryOperator(op)] => (s, Expression::UnaryExpr { val: Box::new(t), operator: op })
        ))
    }

    fn BinaryOperator(input: Node) -> Pesult<&str> {
        Ok(input.as_str())
    }

    fn BinPair(input: Node) -> Pesult<(&str, LocatedExpression, Span)> {
        let s = input.as_span();
        Ok(match_nodes!(
            input.into_children();
            [BinaryOperator(op), Term(r)] => (op, r, s)
        ))
    }

    fn BinaryExpression(input: Node) -> Pesult<LocatedExpression> {
        Ok(match_nodes!(
            input.into_children();
            [Term(l), BinPair(p), BinPair(rest)..] => rest
                .collect::<Vec<(&str, LocatedExpression, Span)>>()
                .into_iter()
                .try_fold(
                    (l.0.start_pos().span(&p.2.end_pos()), Expression::BinaryExpr {
                        left: Box::new(l),
                        operator: p.0,
                        right: Box::new(p.1)
                    }),
                    |lastexpr, npair|
                        Ok((
                            (lastexpr.0.start_pos().span(&npair.2.end_pos())),
                            Expression::BinaryExpr {
                            left: Box::new(lastexpr),
                            operator: npair.0,
                            right: Box::new(npair.1),
                        }))
                )?,
        ))
    }

    fn Number(input: Node) -> Pesult<LocatedExpression> {
        let s = input.as_span();
        Ok((
            s,
            Expression::Num {
                val: input.as_str(),
            },
        ))
    }

    fn Identifier(input: Node) -> Pesult<&str> {
        Ok(input.as_str())
    }

    fn Variable(input: Node) -> Pesult<LocatedExpression> {
        let s = input.as_span();
        Ok((
            s,
            Expression::Variable {
                val: input.as_str(),
            },
        ))
    }

    fn List(input: Node) -> Pesult<LocatedExpression> {
        let s = input.as_span();
        Ok(match_nodes!(
            input.into_children();
            [] => (s, Expression::List(vec![])),
            [ArgumentsNoList(items)] => (s, Expression::List(items)),
        ))
    }

    fn Arguments(input: Node) -> Pesult<Vec<Box<LocatedExpression>>> {
        Self::arguments(input)
    }

    fn ArgumentsNoList(input: Node) -> Pesult<Vec<Box<LocatedExpression>>> {
        Self::arguments(input)
    }

    fn Call(input: Node) -> Pesult<LocatedExpression> {
        let s = input.as_span();
        Ok((
            s,
            match_nodes!(
                input.into_children();
                [Identifier(i)] => Expression::Call {
                    func: i,
                    args: Vec::new(),
                },
                [Identifier(i), Arguments(a)] => Expression::Call {
                    func: i,
                    args: a,
                }
            ),
        ))
    }

    fn Type(input: Node) -> Pesult<ValType> {
        Ok(match input.as_str() {
            "Number" => ValType::Number,
            "List" => ValType::List,
            _ => unreachable!(),
        })
    }

    fn TypeAnnotation(input: Node) -> Pesult<ValType> {
        Ok(match_nodes!(
            input.into_children();
            [Type(t)] => t
        ))
    }

    fn FuncDefParam(input: Node) -> Pesult<(&str, ValType)> {
        Ok(match_nodes!(
            input.into_children();
            [Identifier(name)] => (name, ValType::Number),
            [Identifier(name), TypeAnnotation(t)] => (name, t)
        ))
    }

    fn FuncDefParams(input: Node) -> Pesult<Vec<(&str, ValType)>> {
        Ok(match_nodes!(
            input.into_children();
            [FuncDefParam(params)..] => params.collect()
        ))
    }

    fn FuncDef(input: Node) -> Pesult<FunctionDefinition> {
        Ok(match_nodes!(
            input.into_children();
            [Identifier(n)] => FunctionDefinition {
                name: n,
                args: Vec::new(),
                ret_annotation: None
            },
            [Identifier(n), FuncDefParams(args)] => FunctionDefinition {
                name: n,
                args: args,
                ret_annotation: None
            },
            [Identifier(n), FuncDefParams(args), TypeAnnotation(ret)] => FunctionDefinition {
                name: n,
                args: args,
                ret_annotation: Some(ret)
            },
        ))
    }

    fn FuncDefStmt(input: Node) -> Pesult<LocatedStatement> {
        let s = input.as_span();
        Ok(match_nodes!(
            input.into_children();
            [FuncDef(d), Expression(e)] => (s, Statement::FuncDef(d, e))
        ))
    }

    fn Stmt(input: Node) -> Pesult<LocatedStatement> {
        Ok(match_nodes!(
            input.into_children();
            [FuncDefStmt(e)] => e,
            [Expression(e)] => (e.0, Statement::Expression(e.1)),
        ))
    }

    fn Program(input: Node) -> Pesult<LocatedStatement> {
        Ok(match_nodes!(
            input.into_children();
            [Stmt(s), EOI(_)] => s,
        ))
    }
}

pub fn parse(i: &str) -> Pesult<LocatedStatement> {
    let inputs = DesmosParser::parse(Rule::Program, i)?;
    let input = inputs.single()?;
    DesmosParser::Program(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use pest::Span;

    macro_rules! parse_test {
        ($i:expr, $r:expr) => {
            stmt_ptest!($i, Statement::Expression($r))
        };
    }

    macro_rules! stmt_ptest {
        ($i:expr, $r:expr) => {
            assert_eq!(parse($i).unwrap(), (spn($i, 0, $i.len()), $r));
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
                left: Box::new((
                    spn(i, 0, 5),
                    Expression::BinaryExpr {
                        left: Box::new((spn(i, 0, 1), Expression::Num { val: "1" })),
                        operator: "+",
                        right: Box::new((spn(i, 4, 5), Expression::Num { val: "2" }))
                    }
                )),
                operator: "+",
                right: Box::new((spn(i, 8, 9), Expression::Num { val: "3" })),
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

    #[test]
    fn func_def() {
        let i = "f(a, b) = 1";
        stmt_ptest!(
            i,
            Statement::FuncDef(
                FunctionDefinition {
                    name: "f",
                    args: vec![("a", ValType::Number), ("b", ValType::Number)],
                    ret_annotation: None
                },
                (spn(i, 10, 11), Expression::Num { val: "1" })
            )
        )
    }

    #[test]
    fn func_def_annotations() {
        let i = "f(a: Number, b:List): Number = 1";
        stmt_ptest!(
            i,
            Statement::FuncDef(
                FunctionDefinition {
                    name: "f",
                    args: vec![("a", ValType::Number), ("b", ValType::List)],
                    ret_annotation: Some(ValType::Number)
                },
                (spn(i, 31, 32), Expression::Num { val: "1" })
            )
        )
    }
}
