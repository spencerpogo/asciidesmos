use super::{
    ast::{EquationType, AST},
    binop::{parse_exp, parse_exp_boxed},
    chars,
    simple::{parse_ident, parse_space, parse_space_newline},
    ParseResult,
};
use nom;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    character::complete::char as nom_char,
    multi::separated_list0,
    sequence::tuple,
};

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

    #[test]
    fn test_parse_call() {
        assert_eq!(
            parse_call("a \t(\n  \t1\n\t ) \na"),
            Ok((" \na", AST::Call("a", vec![Box::new(AST::Num("1"))])))
        );
        assert_eq!(
            parse_call("b3 ( a72n, \n\t 123 , 45,67 )\n"),
            Ok((
                "\n",
                AST::Call(
                    "b3",
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
