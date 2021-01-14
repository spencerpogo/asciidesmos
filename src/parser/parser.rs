use super::{ast::AST, chars};
use nom;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while, take_while1, take_while_m_n},
    character::complete::char as nom_char,
    combinator::{opt, recognize},
    multi::separated_list0,
    sequence::{delimited, pair, tuple},
    IResult,
};

pub(self) mod parsers {
    use super::*;

    pub fn parse_ident(i: &str) -> IResult<&str, &str> {
        // Recognize returns everything consumed by the child parser,
        //  combining the two subparsers without re-allocation (I think)
        let (input, output) = recognize(pair(
            take_while_m_n(1, 1, chars::is_ident_leading_char),
            take_while(chars::is_ident_char),
        ))(i)?;
        return Ok((input, output));
    }

    #[allow(dead_code)]
    pub fn parse_space(i: &str) -> IResult<&str, &str> {
        let (inp, out) = take_while(chars::is_space_char)(i)?;
        return Ok((inp, out));
    }

    #[allow(dead_code)]
    pub fn parse_space_newline(i: &str) -> IResult<&str, &str> {
        let (inp, out) = take_while(chars::is_space_newline_char)(i)?;
        return Ok((inp, out));
    }

    #[allow(dead_code)]
    pub fn parse_call(i: &str) -> IResult<&str, AST> {
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
            parse_space_newline,
            nom_char('('),
            parse_space_newline,
            separated_list0(
                take_while1(chars::is_arg_sep),
                take_while1(chars::is_arg_char),
            ),
            parse_space_newline,
            nom_char(')'),
        ))(i)?;
        return Ok((inp, AST::Call(func, args)));
    }

    #[allow(dead_code)]
    pub fn parse_num(i: &str) -> IResult<&str, AST> {
        let (rest, n) = recognize(tuple((
            take_while_m_n(0, 1, |c| c == '+' || c == '-'),
            take_while1(chars::is_digit_char),
            opt(pair(tag("."), take_while(chars::is_digit_char))),
        )))(i)?;
        return Ok((rest, AST::Num(n)));
    }

    #[allow(dead_code)]
    pub fn parse_paren_exp(i: &str) -> IResult<&str, AST> {
        let (res, n) = delimited(nom_char('('), parse_exp, nom_char(')'))(i)?;
        return Ok((res, n));
    }

    #[allow(dead_code)]
    pub fn parse_exp(i: &str) -> IResult<&str, AST> {
        let (rest, n) = alt((parse_num, parse_paren_exp))(i)?;
        return Ok((rest, n));
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
                parse_call("a \n\t(\n  \t1\n\t )"),
                Ok(("", AST::Call("a", vec!["1"])))
            );
            assert_eq!(
                parse_call("_3b \n ( a72n, \n\t 123 , 45,67 )\n"),
                Ok(("\n", AST::Call("_3b", vec!["a72n", "123", "45", "67"])))
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
            assert_eq!(parse_exp("1"), Ok(("", AST::Num("1"))));
        }

        #[test]
        fn test_parse_paren_exp() {
            assert_eq!(parse_paren_exp("(1)"), Ok(("", AST::Num("1"))));
            assert_eq!(parse_exp("(1)"), Ok(("", AST::Num("1"))));
        }
    }
}

//pub fn parse_call() -> Parser {
//  take_while!(p)
//}

pub fn parse(inp: &str) -> IResult<&str, &str> {
    return parsers::parse_ident(inp);
}
