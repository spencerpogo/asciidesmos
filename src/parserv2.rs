use crate::types::{AssertionError, Expression, LocatedExpression};
use pest_consume;
use pest_consume::{match_nodes, Error, Node as PestNode, Parser as PestConsumeParser};

// pest + result = pesult ;)
type Pesult<T> = std::result::Result<T, Error<Rule>>;
type Node<'i> = PestNode<'i, Rule, ()>;

#[derive(PestConsumeParser)]
#[grammar = "grammar.pest"] // relative to src
pub struct DesmosParser;

#[pest_consume::parser]
impl DesmosParser {
    fn EOI(_input: Node) -> Pesult<()> {
        Ok(())
    }

    fn ExpressionNoList(input: Node) -> Pesult<LocatedExpression> {
        DesmosParser::Expression(input)
    }

    fn Expression(input: Node) -> Pesult<LocatedExpression> {
        Ok(match_nodes!(
            input.into_children();
            [List(n)] => n,
            [UnaryExpression(n)] => n
        ))
    }

    fn Term(input: Node) -> Pesult<LocatedExpression> {
        Ok(match_nodes!(
            input.into_children();
            // TODO: Call
            // TODO: Variable
            // TODO: Number
            [Expression(e)] => e
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

    fn Number(input: Node) -> Pesult<LocatedExpression> {
        let s = input.as_span();
        Ok((
            s,
            Expression::Num {
                val: input.as_str(),
            },
        ))
    }

    fn List(input: Node) -> Pesult<LocatedExpression> {
        let s = input.as_span();
        Ok(match_nodes!(
            input.into_children();
            [] => (s, Expression::List(vec![])),
            [ExpressionNoList(items)..] => (s, Expression::List(items.map(|i| Box::new(i)).collect())),
        ))
    }

    fn Program(input: Node) -> Pesult<LocatedExpression> {
        Ok(match_nodes!(
            input.into_children();
            [Expression(n), EOI(_)] => n,
        ))
    }
}

pub fn parse(i: &str) -> Pesult<LocatedExpression> {
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
}
