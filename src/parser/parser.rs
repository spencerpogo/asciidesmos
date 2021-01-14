use super::{ast::AST, chars, Span};
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
use nom_recursive::RecursiveInfo;

fn s(i: &str) -> Span {
    Span::new_extra(i, RecursiveInfo::new())
}

pub(self) mod parsers {
    use super::*;

    pub fn parse_ident(i: Span) -> IResult<Span, Span> {
        // Recognize returns everything consumed by the child parser,
        //  combining the two subparsers without re-allocation (I think)
        let (input, output) = recognize(pair(
            take_while_m_n(1, 1, chars::is_ident_leading_char),
            take_while(chars::is_ident_char),
        ))(i)?;
        return Ok((input, output));
    }

    #[allow(dead_code)]
    pub fn parse_space(i: Span) -> IResult<Span, Span> {
        let (inp, out) = take_while(chars::is_space_char)(i)?;
        return Ok((inp, out));
    }

    #[allow(dead_code)]
    pub fn parse_space_newline(i: Span) -> IResult<Span, Span> {
        let (inp, out) = take_while(chars::is_space_newline_char)(i)?;
        return Ok((inp, out));
    }

    #[allow(dead_code)]
    pub fn parse_call(i: Span) -> IResult<Span, AST> {
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
    pub fn parse_num(i: Span) -> IResult<Span, AST> {
        let (rest, n) = recognize(tuple((
            take_while_m_n(0, 1, |c| c == '+' || c == '-'),
            take_while1(chars::is_digit_char),
            opt(pair(tag("."), take_while(chars::is_digit_char))),
        )))(i)?;
        return Ok((rest, AST::Num(n)));
    }

    #[allow(dead_code)]
    pub fn parse_paren_exp(i: Span) -> IResult<Span, AST> {
        let (res, n) = delimited(nom_char('('), parse_exp, nom_char(')'))(i)?;
        return Ok((res, n));
    }

    #[allow(dead_code)]
    pub fn parse_add_exp(i: Span) -> IResult<Span, AST> {
        let (
            rest,
            (
                left,
                _, // space
                _, // operator
                _, // space
                right,
            ),
        ) = tuple((
            parse_exp,
            parse_space,
            nom_char('+'),
            parse_space,
            parse_exp,
        ))(i)?;
        return Ok((rest, AST::Add(Box::new(left), Box::new(right))));
    }

    #[allow(dead_code)]
    pub fn parse_exp(i: Span) -> IResult<Span, AST> {
        println!("parse_exp {:#?}", i);
        let (rest, n) = alt((parse_paren_exp, parse_add_exp, parse_num))(i)?;
        return Ok((rest, n));
    }

    #[cfg(test)]
    mod tests {
        use super::*;
        use nom::error;

        fn test_parse(l: IResult<Span, Span>, r: (&str, &str)) {
            let (la, lb) = l.unwrap();
            let (ra, rb) = r;
            assert_eq!(la.fragment(), &ra);
            assert_eq!(lb.fragment(), &rb);
        }

        #[test]
        fn test_parse_ident() {
            test_parse(parse_ident(s("a1b_c3d")), ("", "a1b_c3d"));
            test_parse(parse_ident(s("_as2df()")), ("()", "_as2df"));
            // umlaut
            test_parse(parse_ident(s("a\u{00fc}b")), ("", "a\u{00fc}b"));
            // No leading numbers
            assert_eq!(
                parse_ident(s("3abc")),
                Err(nom::Err::Error(error::Error {
                    input: s("3abc"),
                    code: error::ErrorKind::TakeWhileMN
                }))
            );
        }

        #[test]
        fn test_parse_space() {
            test_parse(parse_space(s(" \t a")), ("a", " \t "));
            test_parse(parse_space(s("a")), ("a", ""));
        }

        #[test]
        fn test_parse_space_newline() {
            test_parse(parse_space_newline(s("\n \t\n a")), ("a", "\n \t\n "));
        }

        fn assert_call<'a>(
            result: IResult<Span<'a>, AST<'a>>,
            rest: &'a str,
            fname: &'a str,
        ) -> Vec<Span<'a>> {
            let (a, c) = result.unwrap();
            assert_eq!(a.fragment(), &rest);
            match c {
                AST::Call(lf, la) => {
                    assert_eq!(lf.fragment(), &fname);
                    return la;
                }
                _ => unreachable!(),
            }
        }

        #[test]
        fn test_parse_call() {
            let args = assert_call(parse_call(s("a \n\t(\n  \t1\n\t )")), "", "a");
            assert_eq!(args.len(), 1);
            assert_eq!(args[0].fragment(), &"1");

            let args = assert_call(
                parse_call(s("_3b \n ( a72n, \n\t 123 , 45,67 )\n")),
                "\n",
                "_3b",
            );
            assert_eq!(args.len(), 4);
            assert_eq!(args[0].fragment(), &"a72n");
            assert_eq!(args[1].fragment(), &"123");
            assert_eq!(args[2].fragment(), &"45");
            assert_eq!(args[3].fragment(), &"67");
        }

        fn assert_num<'a>(n: &'a str, rest: &'a str, expect: &'a str) {
            let (r, ast) = parse_num(s(n)).unwrap();
            assert_eq!(r.fragment(), &rest);
            match ast {
                AST::Num(n) => {
                    assert_eq!(n.fragment(), &expect);
                }
                _ => unreachable!(),
            }
        }

        #[test]
        fn test_parse_num() {
            assert_num("0aaa", "aaa", "0");
            assert_num("0.1234", "", "0.1234");
            assert_num("1234.", "", "1234.");
            assert_eq!(
                parse_ident(s("")),
                Err(nom::Err::Error(error::Error {
                    input: s(""),
                    code: error::ErrorKind::TakeWhileMN
                }))
            );
        }

        #[test]
        fn test_parse_num_signed() {
            assert_num("-1", "", "-1");
            assert_num("+1", "", "+1");
            assert_eq!(
                parse_num(s("-+0")),
                Err(nom::Err::Error(error::Error {
                    input: unsafe { Span::new_from_raw_offset(1, 1, "+0", RecursiveInfo::new()) },
                    code: error::ErrorKind::TakeWhile1
                }))
            );
        }

        /*#[test]
        fn test_parse_exp() {
            // It parses numbers
            assert_eq!(parse_exp("1"), Ok(("", AST::Num("1"))));
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
                    AST::Add(Box::new(AST::Num(s("1"))), Box::new(AST::Num(s("2"))))
                ))
            );
        }*/
    }
}

//pub fn parse_call() -> Parser {
//  take_while!(p)
//}

pub fn parse(inp: &str) -> IResult<Span, AST> {
    let (s, a) = parsers::parse_exp(s(inp))?;
    return Ok((s, a));
}
