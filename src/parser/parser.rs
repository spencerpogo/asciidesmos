use super::{
    ast::AST,
    binop::{parse_exp, parse_exp_boxed},
    chars,
    simple::{parse_ident, parse_space, parse_space_newline},
    ParseResult,
};
use nom;
use nom::{
    bytes::complete::take_while1, character::complete::char as nom_char, multi::separated_list0,
    sequence::tuple,
};

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
}

//pub fn parse_call() -> Parser {
//  take_while!(p)
//}

pub fn parse(inp: &str) -> ParseResult<AST> {
    return parse_exp(inp);
}
