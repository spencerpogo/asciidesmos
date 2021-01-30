use super::{
    ast::{Operation, AST},
    parser::parse_call,
    simple::{parse_ident_ast, parse_num, parse_space, parse_space_newline},
    ParseResult,
};
use nom::{
    branch::alt,
    character::complete::char as nom_char,
    multi::fold_many0,
    sequence::{delimited, tuple},
};

pub fn parse_paren_exp(i: &str) -> ParseResult<AST> {
    let (res, n) = delimited(nom_char('('), parse_exp, nom_char(')'))(i)?;
    return Ok((res, n));
}

macro_rules! binop_parser {
    ($name:ident, $c:expr, $op:expr) => {
        pub fn $name(i: &str) -> ParseResult<(Operation, AST)> {
            let (
                rest,
                (
                    _, // space
                    _, // operator
                    _, // space
                    right,
                ),
            ) = tuple((parse_space, nom_char($c), parse_space, parse_term))(i)?;
            Ok((rest, ($op, right)))
        }
    };
}

binop_parser!(parse_mul_exp, '*', Operation::Mul);
binop_parser!(parse_div_exp, '/', Operation::Div);
binop_parser!(parse_add_exp, '+', Operation::Add);
binop_parser!(parse_sub_exp, '-', Operation::Sub);

pub fn parse_factorial_exp(i: &str) -> ParseResult<(Operation, AST)> {
    let (rest, _) = tuple((parse_space, nom_char('!')))(i)?;
    Ok((rest, (Operation::Factorial, AST::FactorialLeft)))
}

#[allow(dead_code)]
pub fn parse_term(i: &str) -> ParseResult<AST> {
    let (rest, n) = alt((parse_paren_exp, parse_num, parse_call, parse_ident_ast))(i)?;
    return Ok((rest, n));
}

pub fn parse_binop(i: &str) -> ParseResult<(Operation, AST)> {
    alt((
        parse_factorial_exp,
        parse_mul_exp,
        parse_div_exp,
        parse_add_exp,
        parse_sub_exp,
    ))(i)
}

pub fn parse_exp(i: &str) -> ParseResult<AST> {
    // Require a single term, e.g. the "1" in "1+1" or "1"
    let (inp, (_, first)) = tuple((parse_space_newline, parse_term))(i)?;
    // Fold in any binop operations from the remaining input
    let (rest, n) = fold_many0(
        parse_binop,
        Vec::new(),
        |mut l: Vec<(Operation, Box<AST>)>, item| {
            let (op, r) = item;
            l.push((op, Box::new(r)));
            l
        },
    )(inp)?;

    if n.len() > 0 {
        return Ok((rest, AST::BinOp(Box::new(first), n)));
    } else {
        return Ok((rest, first));
    }
}

pub fn parse_exp_boxed(i: &str) -> ParseResult<Box<AST>> {
    let (rest, ast) = parse_exp(i)?;
    return Ok((rest, Box::new(ast)));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_exp() {
        // It parses numbers
        assert_eq!(parse_exp(" 1 "), Ok((" ", AST::Num("1"))));
    }

    #[test]
    fn test_parse_paren_exp() {
        assert_eq!(parse_exp("(1)"), Ok(("", AST::Num("1"))));
    }

    #[test]
    fn test_parse_add_exp() {
        assert_eq!(
            parse_exp("1 + 2"),
            Ok((
                "",
                AST::BinOp(
                    Box::new(AST::Num("1")),
                    vec![(Operation::Add, Box::new(AST::Num("2"))),]
                )
            ))
        );
        assert_eq!(
            parse_exp("1 + 2 + 3"),
            Ok((
                "",
                AST::BinOp(
                    Box::new(AST::Num("1")),
                    vec![
                        (Operation::Add, Box::new(AST::Num("2"))),
                        (Operation::Add, Box::new(AST::Num("3")))
                    ]
                )
            ))
        );
        assert_eq!(parse_exp("1 + "), Ok((" + ", AST::Num("1"))));
        assert_eq!(
            parse_exp("1 + (2 + 3)"),
            Ok((
                "",
                AST::BinOp(
                    Box::new(AST::Num("1")),
                    vec![(
                        Operation::Add,
                        Box::new(AST::BinOp(
                            Box::new(AST::Num("2")),
                            vec![(Operation::Add, Box::new(AST::Num("3")))]
                        ))
                    ),]
                )
            ))
        );
    }

    #[test]
    fn test_mul_exp() {
        assert_eq!(
            parse_exp("1 * 2"),
            Ok((
                "",
                AST::BinOp(
                    Box::new(AST::Num("1")),
                    vec![(Operation::Mul, Box::new(AST::Num("2")))]
                )
            ))
        );
        assert_eq!(
            parse_exp("1 + 2 * 3"),
            Ok((
                "",
                AST::BinOp(
                    Box::new(AST::Num("1")),
                    vec![
                        (Operation::Add, Box::new(AST::Num("2"))),
                        (Operation::Mul, Box::new(AST::Num("3")))
                    ]
                )
            ))
        );
    }

    #[test]
    fn test_factorial_exp() {
        assert_eq!(
            parse_exp(" 3 ! + 2 "),
            Ok((
                " ",
                AST::BinOp(
                    Box::new(AST::Num("3")),
                    vec![
                        (Operation::Factorial, Box::new(AST::FactorialLeft)),
                        (Operation::Add, Box::new(AST::Num("2")))
                    ]
                )
            ))
        )
    }

    #[test]
    fn test_call_term() {
        assert_eq!(
            parse_exp("a(1) + b(2)"),
            Ok((
                "",
                AST::BinOp(
                    Box::new(AST::Call("a", vec![Box::new(AST::Num("1"))])),
                    vec![(
                        Operation::Add,
                        Box::new(AST::Call("b", vec![Box::new(AST::Num("2"))]))
                    )]
                )
            ))
        )
    }

    #[test]
    fn test_ident_term() {
        assert_eq!(
            parse_exp("a + b "),
            Ok((
                " ",
                AST::BinOp(
                    Box::new(AST::Ident("a")),
                    vec![(Operation::Add, Box::new(AST::Ident("b")))]
                )
            ))
        )
    }
}
