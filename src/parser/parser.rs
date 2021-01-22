use super::{
    ast::{EquationType, Operation, AST},
    chars,
};
use nom;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while, take_while1, take_while_m_n},
    character::complete::char as nom_char,
    combinator::{opt, recognize},
    multi::{fold_many0, separated_list0},
    sequence::{delimited, pair, tuple},
    IResult,
};

type ParseResult<'a, T> = IResult<&'a str, T>;

pub fn parse_ident(i: &str) -> ParseResult<&str> {
    // Recognize returns everything consumed by the child parser,
    //  combining the two subparsers without re-allocation (I think)
    let (input, output) = recognize(pair(
        take_while_m_n(1, 1, chars::is_ident_leading_char),
        take_while(chars::is_ident_char),
    ))(i)?;
    return Ok((input, output));
}

pub fn parse_ident_ast(i: &str) -> ParseResult<AST> {
    let (rest, ident) = parse_ident(i)?;
    return Ok((rest, AST::Ident(ident)));
}

#[allow(dead_code)]
pub fn parse_space(i: &str) -> ParseResult<&str> {
    let (inp, out) = take_while(chars::is_space_char)(i)?;
    return Ok((inp, out));
}

#[allow(dead_code)]
pub fn parse_space_newline(i: &str) -> ParseResult<&str> {
    let (inp, out) = take_while(chars::is_space_newline_char)(i)?;
    return Ok((inp, out));
}

#[allow(dead_code)]
pub fn parse_call(i: &str) -> ParseResult<AST> {
    let (
        inp,
        (
            func,
            _, // space
            _, // Open paren
            _, // space
            args,
            _, // space
            _, // Close paren
        ),
    ) = tuple((
        parse_ident,
        parse_space,
        nom_char('('),
        parse_space_newline,
        // is_arg_sep accepts commas, spaces, and newlines
        separated_list0(take_while1(chars::is_arg_sep), parse_exp_boxed),
        parse_space_newline,
        nom_char(')'),
    ))(i)?;
    return Ok((inp, AST::Call(func, args)));
}

#[allow(dead_code)]
pub fn parse_num(i: &str) -> ParseResult<AST> {
    let (rest, n) = recognize(tuple((
        take_while_m_n(0, 1, |c| c == '+' || c == '-'),
        take_while1(chars::is_digit_char),
        opt(pair(tag("."), take_while(chars::is_digit_char))),
    )))(i)?;
    return Ok((rest, AST::Num(n)));
}

#[allow(dead_code)]
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

#[allow(dead_code)]
pub fn parse_binop(i: &str) -> ParseResult<(Operation, AST)> {
    alt((
        parse_factorial_exp,
        parse_mul_exp,
        parse_div_exp,
        parse_add_exp,
        parse_sub_exp,
    ))(i)
}

#[allow(dead_code)]
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

#[allow(dead_code)]
pub fn parse_exp_boxed(i: &str) -> ParseResult<Box<AST>> {
    let (rest, ast) = parse_exp(i)?;
    return Ok((rest, Box::new(ast)));
}

macro_rules! eq_sep_parser {
    ($name:ident, $c:expr, $val:expr) => {
        pub fn $name(i: &str) -> ParseResult<EquationType> {
            let (rest, _) = tag($c)(i)?;
            Ok((rest, $val))
        }
    };
}

eq_sep_parser!(parse_eq_equal, "=", EquationType::Equal);
eq_sep_parser!(parse_eq_gt, ">", EquationType::GreaterThan);
eq_sep_parser!(parse_eq_gte, ">=", EquationType::GreaterThanEqualTo);
eq_sep_parser!(parse_eq_lt, "<", EquationType::LessThan);
eq_sep_parser!(parse_eq_lte, "<=", EquationType::LessThanEqualTo);

#[allow(dead_code)]
pub fn parse_equation_sep(i: &str) -> ParseResult<EquationType> {
    alt((
        parse_eq_gte,
        parse_eq_lte,
        parse_eq_gt,
        parse_eq_lt,
        parse_eq_equal,
    ))(i)
}

#[allow(dead_code)]
pub fn parse_equation(i: &str) -> ParseResult<AST> {
    let (
        rest,
        (
            _, // space
            ident,
            _, // space
            eq_type,
            expr,
        ),
    ) = tuple((
        parse_space,
        parse_ident,
        parse_space,
        parse_equation_sep,
        parse_exp,
    ))(i)?;
    return Ok((rest, AST::Equation(ident, eq_type, Box::new(expr))));
}

#[allow(dead_code)]
pub fn parse_comment(i: &str) -> ParseResult<()> {
    let (rest, _) = tuple((tag("//"), take_while(|c| c != '\n'), nom_char('\n')))(i)?;
    return Ok((rest, ()));
}

#[cfg(test)]
mod tests {
    use super::*;
    use nom::error;

    #[test]
    fn test_parse_ident() {
        assert_eq!(parse_ident("a1b_c3d"), Ok(("", "a1b_c3d")));
        assert_eq!(parse_ident("_as2df()"), Ok(("()", "_as2df")));
        // umlaut
        assert_eq!(parse_ident("a\u{00fc}b"), Ok(("", "a\u{00fc}b")));
        // No leading numbers
        assert_eq!(
            parse_ident("3abc"),
            Err(nom::Err::Error(error::Error {
                input: "3abc",
                code: error::ErrorKind::TakeWhileMN
            }))
        );
    }

    #[test]
    fn test_parse_space() {
        assert_eq!(parse_space(" \t a"), Ok(("a", " \t ")));
        assert_eq!(parse_space("a"), Ok(("a", "")));
    }

    #[test]
    fn test_parse_space_newline() {
        assert_eq!(parse_space_newline("\n \t\n a"), Ok(("a", "\n \t\n ")))
    }

    #[test]
    fn test_parse_call() {
        assert_eq!(
            parse_call("a \t(\n  \t1\n\t ) \na"),
            Ok((" \na", AST::Call("a", vec![Box::new(AST::Num("1"))])))
        );
        assert_eq!(
            parse_call("_3b ( a72n, \n\t 123 , 45,67 )\n"),
            Ok((
                "\n",
                AST::Call(
                    "_3b",
                    vec![
                        Box::new(AST::Ident("a72n")),
                        Box::new(AST::Num("123")),
                        Box::new(AST::Num("45")),
                        Box::new(AST::Num("67"))
                    ]
                )
            ))
        )
    }

    #[test]
    fn test_parse_num() {
        assert_eq!(parse_num("0"), Ok(("", AST::Num("0"))));
        assert_eq!(parse_num("0aaa"), Ok(("aaa", AST::Num("0"))));
        assert_eq!(parse_num("0.1234"), Ok(("", AST::Num("0.1234"))));
        assert_eq!(parse_num("12345."), Ok(("", AST::Num("12345."))));
        assert_eq!(
            parse_num(""),
            Err(nom::Err::Error(error::Error {
                input: "",
                code: error::ErrorKind::TakeWhile1
            }))
        );
    }

    #[test]
    fn test_parse_num_signed() {
        assert_eq!(parse_num("-1"), Ok(("", AST::Num("-1"))));
        assert_eq!(parse_num("+1"), Ok(("", AST::Num("+1"))));
        assert_eq!(
            parse_num("-+0"),
            Err(nom::Err::Error(error::Error {
                input: "+0",
                code: error::ErrorKind::TakeWhile1
            }))
        );
        assert_eq!(parse_num("-123.456"), Ok(("", AST::Num("-123.456"))));
    }

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

    #[test]
    fn test_parse_equation() {
        macro_rules! asert_eq_parse {
            ($sep:expr, $t:expr) => {
                assert_eq!(
                    parse_equation(concat!(" a ", $sep, " 1 ")),
                    Ok((" ", AST::Equation("a", $t, Box::new(AST::Num("1")))))
                )
            };
        }

        asert_eq_parse!("=", EquationType::Equal);
        asert_eq_parse!(">", EquationType::GreaterThan);
        asert_eq_parse!(">=", EquationType::GreaterThanEqualTo);
        asert_eq_parse!("<", EquationType::LessThan);
        asert_eq_parse!("<=", EquationType::LessThanEqualTo);
    }

    #[test]
    fn test_parse_comment() {
        assert_eq!(parse_comment("// a\n "), Ok((" ", ())));
    }
}

//pub fn parse_call() -> Parser {
//  take_while!(p)
//}

pub fn parse(inp: &str) -> ParseResult<AST> {
    return parse_exp(inp);
}
