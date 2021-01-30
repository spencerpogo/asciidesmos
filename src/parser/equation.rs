use super::{
    ast::{EquationType, AST},
    binop::parse_exp,
    simple::{parse_ident, parse_space},
    ParseResult,
};
use nom::{branch::alt, bytes::complete::tag, sequence::tuple};

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

#[cfg(test)]
mod tests {
    use super::*;

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
}
