use crate::parser::ast::AST;

pub fn ident_to_latex(v: &str) -> String {
  let mut chars = v.chars();
  let first = chars.next();
  let rest = chars.as_str();
  if rest.len() == 0 {
    return match first {
      None => "".to_string(),
      Some(c) => c.to_string(),
    };
  } else {
    // if there 2 or more chars, there will always be a first
    return format!("{}_{{{}}}", first.unwrap(), rest);
  }
}

pub fn ast_to_latex(ast: &AST) -> String {
  match ast {
    AST::Ident(x) => ident_to_latex(x),
    AST::Num(x) => x.to_string(),
    AST::Call(f, args) => format!(
      "{}\\left({}\\right)",
      ident_to_latex(f),
      args.iter().fold(String::new(), |mut s, a| {
        s.push_str(&ast_to_latex(&**a));
        s
      })
    ),
    _ => unimplemented!(),
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_empty_ident() {
    assert_eq!(ast_to_latex(&AST::Ident("")), "".to_string());
  }

  #[test]
  fn test_short_ident() {
    assert_eq!(ast_to_latex(&AST::Ident("a")), "a".to_string());
    // umlaut
    assert_eq!(
      ast_to_latex(&AST::Ident("\u{00fc}")),
      "\u{00fc}".to_string()
    );
  }

  #[test]
  fn test_long_ident() {
    assert_eq!(ast_to_latex(&AST::Ident("abc")), "a_{bc}".to_string());
  }

  #[test]
  fn test_num() {
    assert_eq!(ast_to_latex(&AST::Num("1")), "1".to_string());
  }

  #[test]
  fn test_call() {
    assert_eq!(
      ast_to_latex(&AST::Call("a", vec![Box::new(AST::Num("1"))])),
      "a\\left(1\\right)".to_string(),
    );
  }
}
