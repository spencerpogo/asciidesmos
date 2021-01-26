use crate::parser::ast::{EquationType, Operation, AST};

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

pub fn operation_to_latex(left: &String, op: &Operation, right: &String) -> String {
  match op {
    Operation::Mul => format!("{}\\cdot2{}", left, right),
    Operation::Div => format!("\\frac{{{}}}{{{}}}", left, right),
    Operation::Add => format!("{}+{}", left, right),
    Operation::Sub => format!("{}-{}", left, right),
    Operation::Factorial => format!("{}!", left),
  }
}

// Whats the point of even testing this?
#[cfg(not(tarpaulin_include))]
pub fn equationtype_to_latex(eqt: &EquationType) -> &str {
  match eqt {
    EquationType::Equal => "=",
    EquationType::GreaterThan => ">",
    EquationType::GreaterThanEqualTo => "\\ge",
    EquationType::LessThan => "<",
    EquationType::LessThanEqualTo => "\\le",
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
    AST::BinOp(first, terms) => {
      let (r, _) = terms.iter().fold(
        (String::new(), ast_to_latex(&*first)),
        |(mut s, left), (op, rbox)| {
          let rstr = match &**rbox {
            AST::FactorialLeft => String::from(""),
            r => ast_to_latex(r),
          };
          s.push_str(&operation_to_latex(&left, op, &rstr));
          (s, rstr)
        },
      );
      r
    }
    AST::Equation(v, eqt, expr) => {
      format!("{}{}{}", v, equationtype_to_latex(eqt), ast_to_latex(expr))
    }
    AST::FactorialLeft => unreachable!(),
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

  #[test]
  fn test_binop_add() {
    assert_eq!(
      ast_to_latex(&AST::BinOp(
        Box::new(AST::Num("1")),
        vec![(Operation::Add, Box::new(AST::Ident("abc")))]
      )),
      "1+a_{bc}".to_string()
    );
  }

  #[test]
  fn test_binop_sub() {
    assert_eq!(
      ast_to_latex(&AST::BinOp(
        Box::new(AST::Num("1")),
        vec![(Operation::Sub, Box::new(AST::Ident("abc")))]
      )),
      "1-a_{bc}".to_string()
    );
  }

  #[test]
  fn test_binop_mul() {
    let call = AST::Call("f", vec![Box::new(AST::Num("1"))]);
    let callstr = ast_to_latex(&call);

    assert_eq!(
      ast_to_latex(&AST::BinOp(
        Box::new(call),
        vec![(Operation::Mul, Box::new(AST::Ident("abc")))]
      )),
      format!("{}\\cdot2a_{{bc}}", callstr)
    );
  }

  #[test]
  fn test_binop_div() {
    assert_eq!(
      ast_to_latex(&AST::BinOp(
        Box::new(AST::Ident("abc")),
        vec![(Operation::Div, Box::new(AST::Ident("def")))]
      )),
      "\\frac{a_{bc}}{d_{ef}}".to_string()
    );
  }

  #[test]
  fn test_binop_factorial() {
    assert_eq!(
      ast_to_latex(&AST::BinOp(
        Box::new(AST::Num("3")),
        vec![
          (Operation::Factorial, Box::new(AST::FactorialLeft)),
          (Operation::Mul, Box::new(AST::Ident("abc")))
        ]
      )),
      "3!\\cdot2a_{bc}".to_string()
    );
  }

  #[test]
  fn test_equation() {
    assert_eq!(
      ast_to_latex(&AST::Equation(
        "a",
        EquationType::Equal,
        Box::new(AST::Num("1"))
      )),
      "a=1".to_string(),
    )
  }
}
