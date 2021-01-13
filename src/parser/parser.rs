use super::{ast::AST, chars};
use nom;
use nom::{
    bytes::complete::{tag, take_while, take_while1, take_while_m_n},
    combinator::recognize,
    multi::separated_list0,
    sequence::{pair, tuple},
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

    pub fn parse_space(i: &str) -> IResult<&str, &str> {
        let (inp, out) = take_while(chars::is_space_char)(i)?;
        return Ok((inp, out));
    }

    pub fn parse_space_newline(i: &str) -> IResult<&str, &str> {
        let (inp, out) = take_while(chars::is_space_newline_char)(i)?;
        return Ok((inp, out));
    }

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
            tag("("),
            parse_space_newline,
            separated_list0(
                take_while1(chars::is_arg_sep),
                take_while1(chars::is_arg_char),
            ),
            parse_space_newline,
            tag(")"),
        ))(i)?;
        return Ok((inp, AST::Call(func, args)));
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
    }
}

//pub fn parse_call() -> Parser {
//  take_while!(p)
//}

pub fn parse(inp: &str) -> IResult<&str, &str> {
    return parsers::parse_ident(inp);
}
