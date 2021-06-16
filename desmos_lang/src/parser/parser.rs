use crate::core::{
    ast::{
        BinaryOperator, Branch, CallModifier, Expression, FunctionDefinition, LocatedExpression,
        LocatedStatement, Statement, UnaryOperator,
    },
    latex::CompareOperator,
    runtime::ValType,
};
use pest::Span;
use pest_consume::{match_nodes, Error, Node as PestNode, Parser as PestConsumeParser};

// pest + result = pesult ;)
pub type ParseError = Error<Rule>;
pub type Pesult<T> = std::result::Result<T, ParseError>;
pub type Node<'i> = PestNode<'i, Rule, ()>;

#[allow(clippy::upper_case_acronyms)]
#[derive(PestConsumeParser)]
#[grammar = "parser/grammar.pest"] // relative to src
pub struct DesmosParser;

impl DesmosParser {
    // Shared rules
    fn arguments(input: Node) -> Pesult<Vec<LocatedExpression>> {
        Ok(match_nodes!(
            input.into_children();
            [Expression(e)..] => e,
            [ExpressionNoList(e)..] => e,
        )
        .collect())
    }

    fn expression(input: Node) -> Pesult<LocatedExpression> {
        Ok(match_nodes!(
            input.into_children();
            [List(n)] => n,
            [UnaryExpression(n)] => n,
            [BinaryExpression(n)] => n,
            [Term(n)] => n,
            [Piecewise(n)] => n,
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

    fn Factorial(input: Node) -> Pesult<UnaryOperator> {
        Ok(UnaryOperator::Factorial)
    }

    fn UnaryOperator(input: Node) -> Pesult<UnaryOperator> {
        Ok(match_nodes!(
            input.into_children();
            [Factorial(o)] => o,
        ))
    }

    fn UnaryExpression(input: Node) -> Pesult<LocatedExpression> {
        let s = input.as_span();
        Ok(match_nodes!(
            input.into_children();
            [Term(t), UnaryOperator(op)] => (s, Expression::UnaryExpr { val: Box::new(t), operator: op })
        ))
    }

    fn Add(input: Node) -> Pesult<BinaryOperator> {
        Ok(BinaryOperator::Add)
    }

    fn Subtract(input: Node) -> Pesult<BinaryOperator> {
        Ok(BinaryOperator::Subtract)
    }

    fn Multiply(input: Node) -> Pesult<BinaryOperator> {
        Ok(BinaryOperator::Multiply)
    }

    fn Divide(input: Node) -> Pesult<BinaryOperator> {
        Ok(BinaryOperator::Divide)
    }

    fn Mod(input: Node) -> Pesult<BinaryOperator> {
        Ok(BinaryOperator::Mod)
    }

    fn BinaryOperator(input: Node) -> Pesult<BinaryOperator> {
        Ok(match_nodes!(
            input.into_children();
            [Add(o)] => o,
            [Subtract(o)] => o,
            [Multiply(o)] => o,
            [Divide(o)] => o,
            [Mod(o)] => o,
        ))
    }

    fn BinPair(input: Node) -> Pesult<(BinaryOperator, LocatedExpression, Span)> {
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
                .collect::<Vec<_>>()
                .into_iter()
                .fold(
                    (l.0.start_pos().span(&p.2.end_pos()), Expression::BinaryExpr {
                        left: Box::new(l),
                        operator: p.0,
                        right: Box::new(p.1)
                    }),
                    |lastexpr, npair|
                        (
                            (lastexpr.0.start_pos().span(&npair.2.end_pos())),
                            Expression::BinaryExpr {
                                left: Box::new(lastexpr),
                                operator: npair.0,
                                right: Box::new(npair.1),
                            }
                        )
                ),
        ))
    }

    fn Piecewise(input: Node) -> Pesult<LocatedExpression> {
        let spn = input.as_span();
        Ok(match_nodes!(
            input.into_children();
            [PiecewiseContents(c)] => (spn, c),
        ))
    }

    fn PiecewiseContents(input: Node) -> Pesult<Expression> {
        Ok(match_nodes!(
            input.into_children();
            [
                PiecewiseBranch(first),
                PiecewiseBranches(rest),
                OtherwiseBranch(default)
            ] => Expression::Piecewise {
                first: Box::new(first),
                rest: rest,
                default: Box::new(default),
            },
        ))
    }

    fn PiecewiseBranches(input: Node) -> Pesult<Vec<Branch>> {
        Ok(match_nodes!(
            input.into_children();
            [PiecewiseBranch(branches)..] => branches.collect(),
        ))
    }

    fn PiecewiseBranch(input: Node) -> Pesult<Branch> {
        Ok(match_nodes!(
            input.into_children();
            [Condition(cond), Expression(val)] => {
                let (cond_left, cond, cond_right) = cond;
                Branch { cond_left, cond, cond_right, val }
            },
        ))
    }

    fn Condition(input: Node) -> Pesult<(LocatedExpression, CompareOperator, LocatedExpression)> {
        Ok(match_nodes!(
            input.into_children();
            [Expression(left), CompareOp(cond), Expression(right)] => (left, cond, right),
        ))
    }

    fn Equals(input: Node) -> Pesult<CompareOperator> {
        Ok(CompareOperator::Equal)
    }

    fn Less(input: Node) -> Pesult<CompareOperator> {
        Ok(CompareOperator::LessThan)
    }

    fn Greater(input: Node) -> Pesult<CompareOperator> {
        Ok(CompareOperator::GreaterThan)
    }

    fn LessEq(input: Node) -> Pesult<CompareOperator> {
        Ok(CompareOperator::LessThanEqual)
    }

    fn GreaterEq(input: Node) -> Pesult<CompareOperator> {
        Ok(CompareOperator::GreaterThanEqual)
    }

    fn CompareOp(input: Node) -> Pesult<CompareOperator> {
        Ok(match_nodes!(
            input.into_children();
            [Equals(v)] => v,
            [Less(v)] => v,
            [Greater(v)] => v,
            [LessEq(v)] => v,
            [GreaterEq(v)] => v,
        ))
    }

    fn OtherwiseBranch(input: Node) -> Pesult<LocatedExpression> {
        Ok(match_nodes!(input.into_children(); [Expression(e)] => e))
    }

    fn Number(input: Node) -> Pesult<LocatedExpression> {
        let s = input.as_span();
        Ok((s, Expression::Num(input.as_str())))
    }

    fn Identifier(input: Node) -> Pesult<&str> {
        Ok(input.as_str())
    }

    fn Variable(input: Node) -> Pesult<LocatedExpression> {
        let s = input.as_span();
        Ok((s, Expression::Variable(input.as_str())))
    }

    fn List(input: Node) -> Pesult<LocatedExpression> {
        let s = input.as_span();
        Ok(match_nodes!(
            input.into_children();
            [] => (s, Expression::List(vec![])),
            [ArgumentsNoList(items)] => (s, Expression::List(items)),
        ))
    }

    fn Arguments(input: Node) -> Pesult<Vec<LocatedExpression>> {
        Self::arguments(input)
    }

    fn ArgumentsNoList(input: Node) -> Pesult<Vec<LocatedExpression>> {
        Self::arguments(input)
    }

    fn Call(input: Node) -> Pesult<LocatedExpression> {
        let s = input.as_span();
        Ok((
            s,
            match_nodes!(
                input.into_children();
                [CallStart(s)] => Expression::Call {
                    modifier: s.1,
                    func: s.0,
                    args: Vec::new(),
                },
                [CallStart(s), Arguments(a)] => Expression::Call {
                    modifier: s.1,
                    func: s.0,
                    args: a,
                }
            ),
        ))
    }

    fn MapCall(input: Node) -> Pesult<CallModifier> {
        Ok(CallModifier::MapCall)
    }

    fn MacroCall(input: Node) -> Pesult<CallModifier> {
        Ok(CallModifier::MacroCall)
    }

    fn NormalCall(input: Node) -> Pesult<CallModifier> {
        Ok(CallModifier::NormalCall)
    }

    fn CallStart(input: Node) -> Pesult<(&str, CallModifier)> {
        Ok(match_nodes!(
            input.into_children();
            [Identifier(i), MapCall(c)] => (i,c ),
            [Identifier(i), MacroCall(c)] => (i,c ),
            [Identifier(i), NormalCall(c)] => (i,c ),
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
                parse_test!($v, Expression::Num($v));
            };
        }

        num_test!("1");
        num_test!("-2");
        num_test!("+3");
    }

    #[test]
    fn variable() {
        parse_test!("w3c", Expression::Variable("w3c"));
        assert_eq!(parse("3wc").is_err(), true);
    }

    #[test]
    fn binary_expression() {
        let i = "1 + 2";
        parse_test!(
            i,
            Expression::BinaryExpr {
                left: Box::new((spn(i, 0, 1), Expression::Num("1"))),
                operator: BinaryOperator::Add,
                right: Box::new((spn(i, 4, 5), Expression::Num("2")))
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
                        left: Box::new((spn(i, 0, 1), Expression::Num("1"))),
                        operator: BinaryOperator::Add,
                        right: Box::new((spn(i, 4, 5), Expression::Num("2")))
                    }
                )),
                operator: BinaryOperator::Add,
                right: Box::new((spn(i, 8, 9), Expression::Num("3"))),
            }
        );
    }

    #[test]
    fn unary_expression() {
        let i = "1!";
        parse_test!(
            i,
            Expression::UnaryExpr {
                val: Box::new((spn(i, 0, 1), Expression::Num("1"))),
                operator: UnaryOperator::Factorial,
            }
        );
    }

    #[test]
    fn call() {
        parse_test!(
            "a()",
            Expression::Call {
                modifier: CallModifier::NormalCall,
                func: "a",
                args: Vec::new(),
            }
        );
        let j = "a(1, 2, 3)";
        parse_test!(
            j,
            Expression::Call {
                modifier: CallModifier::NormalCall,
                func: "a",
                args: vec![
                    (spn(j, 2, 3), Expression::Num("1")),
                    (spn(j, 5, 6), Expression::Num("2")),
                    (spn(j, 8, 9), Expression::Num("3")),
                ]
            }
        );
    }

    #[test]
    fn mapcall() {
        let i = "sin@(1, 2)";
        parse_test!(
            i,
            Expression::Call {
                modifier: CallModifier::MapCall,
                func: "mac",
                args: vec![
                    (spn(i, 5, 6), Expression::Num("1")),
                    (spn(i, 8, 9), Expression::Num("2"))
                ]
            }
        )
    }

    #[test]
    fn macrocall() {
        let i = "mac!(1, 2)";
        parse_test!(
            i,
            Expression::Call {
                modifier: CallModifier::NormalCall,
                func: "mac",
                args: vec![
                    (spn(i, 5, 6), Expression::Num("1")),
                    (spn(i, 8, 9), Expression::Num("2"))
                ]
            }
        )
    }

    #[test]
    fn map_piecewise() {
        let i = "@{a=1:2,otherwise:3}";
        parse_test!(
            i,
            Expression::Piecewise {
                first: Box::new(Branch {
                    cond_left: (spn(i, 2, 3), Expression::Variable("a")),
                    cond: CompareOperator::Equal,
                    cond_right: (spn(i, 4, 5), Expression::Num("1")),
                    val: (spn(i, 6, 7), Expression::Num("2")),
                }),
                rest: vec![],
                default: Box::new((spn(i, 20, 21), Expression::Num("3")))
            }
        );
    }

    #[test]
    fn list() {
        let i = "[1, 2,3]";
        parse_test!(
            i,
            Expression::List(vec![
                (spn(i, 1, 2), Expression::Num("1")),
                (spn(i, 4, 5), Expression::Num("2")),
                (spn(i, 6, 7), Expression::Num("3")),
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
                (spn(i, 10, 11), Expression::Num("1"))
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
                (spn(i, 31, 32), Expression::Num("1"))
            )
        )
    }

    #[test]
    fn piecewise_single() {
        let i = "{ a = 1: 2, otherwise: 3 }";
        parse_test!(
            i,
            Expression::Piecewise {
                first: Box::new(Branch {
                    cond_left: (spn(i, 2, 3), Expression::Variable("a")),
                    cond: CompareOperator::Equal,
                    cond_right: (spn(i, 6, 7), Expression::Num("1")),
                    val: (spn(i, 9, 10), Expression::Num("2"))
                }),
                rest: vec![],
                default: Box::new((spn(i, 23, 24), Expression::Num("3")))
            }
        )
    }

    #[test]
    fn piecewise_multi() {
        let i = "{ a >= 1: 2, a <= 3: 4, a < 5: 6, a > 7: 8, otherwise: 9 }";
        parse_test!(
            i,
            Expression::Piecewise {
                first: Box::new(Branch {
                    cond_left: (spn(i, 2, 3), Expression::Variable("a")),
                    cond: CompareOperator::GreaterThanEqual,
                    cond_right: (spn(i, 7, 8), Expression::Num("1")),
                    val: (spn(i, 10, 11), Expression::Num("2"))
                }),
                rest: vec![
                    Branch {
                        cond_left: (spn(i, 13, 14), Expression::Variable("a")),
                        cond: CompareOperator::LessThanEqual,
                        cond_right: (spn(i, 18, 19), Expression::Num("3")),
                        val: (spn(i, 21, 22), Expression::Num("4"))
                    },
                    Branch {
                        cond_left: (spn(i, 24, 25), Expression::Variable("a")),
                        cond: CompareOperator::LessThan,
                        cond_right: (spn(i, 28, 29), Expression::Num("5")),
                        val: (spn(i, 31, 32), Expression::Num("6"))
                    },
                    Branch {
                        cond_left: (spn(i, 34, 35), Expression::Variable("a")),
                        cond: CompareOperator::GreaterThan,
                        cond_right: (spn(i, 38, 39), Expression::Num("7")),
                        val: (spn(i, 41, 42), Expression::Num("8"))
                    }
                ],
                default: Box::new((spn(i, 55, 56), Expression::Num("9")))
            }
        )
    }
}
