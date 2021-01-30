use super::{ast::AST, chars, ParseResult};
use nom::{
  bytes::complete::{take_while, take_while_m_n},
  combinator::recognize,
  sequence::pair,
};

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

#[cfg(test)]
mod tests {
  use super::*;
  use nom::error;

  #[test]
  fn test_parse_ident() {
    assert_eq!(parse_ident("a1bc3d"), Ok(("", "a1bc3d")));
    assert_eq!(parse_ident("as2df()"), Ok(("()", "as2df")));
    // umlaut
    assert_eq!(parse_ident("a\u{00fc}b"), Ok(("\u{00fc}b", "a")));
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
}
