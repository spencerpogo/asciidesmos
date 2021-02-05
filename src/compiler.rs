use crate::{
    builtins,
    types::{CompileError, Expression, Function},
};
use std::convert::TryFrom;
use std::fmt::Write;

pub struct Context {
    // TODO
}

pub fn compile_identifier(v: &str) -> String {
    // Don't care about UTF-8 since identifiers are guaranteed to be ASCII
    let mut chars = v.chars();

    match chars.next() {
        Some(c) => {
            let rest: String = chars.collect();
            if rest.len() == 0 {
                c.to_string()
            } else {
                format!("{}_{{{}}}", c, rest)
            }
        }
        None => "".to_string(),
    }
}

pub fn resolve_function<'a>(_ctx: &mut Context, func: &str) -> Option<&'a Function> {
    builtins::BUILTIN_FUNCTIONS.get(func)
}

pub fn resolve_func<'a>(
    ctx: &mut Context,
    fname: &'a str,
    args: &Vec<Box<Expression<'a>>>,
) -> Result<(), CompileError<'a>> {
    match resolve_function(ctx, fname) {
        None => Err(CompileError::UnknownFunction(fname)),
        Some(func) => {
            // Validate arg count
            let got = args.len();
            let expect = func.args.len();

            if got != expect {
                Err(CompileError::WrongArgCount {
                    got: got,
                    expected: expect,
                })
            } else {
                // TODO: Validate types
                Ok(())
            }
        }
    }
}

pub fn compile_call<'a>(
    ctx: &mut Context,
    fname: &str,
    args: Vec<Box<Expression<'a>>>,
) -> Result<String, CompileError<'a>> {
    Ok(format!(
        "{}\\left({}\\right)",
        compile_identifier(fname),
        args.into_iter().try_fold(String::new(), |mut s, i| {
            // Writing the string should never fail
            write!(s, "{}", compile_expr(ctx, *i)?).unwrap();
            Ok(s)
        })?
    ))
}

pub fn compile_expr<'a>(
    ctx: &mut Context,
    expr: Expression<'a>,
) -> Result<String, CompileError<'a>> {
    match expr {
        Expression::Num { val } => Ok(val.to_string()),
        Expression::Variable { val } => Ok(compile_identifier(val)),
        Expression::BinaryExpr {
            left,
            operator,
            right,
        } => Ok(format!(
            "{}{}{}",
            compile_expr(ctx, *left)?,
            operator,
            compile_expr(ctx, *right)?
        )),
        Expression::UnaryExpr {
            val: v,
            operator: op,
        } => Ok(format!("{}{}", compile_expr(ctx, *v)?, op)),
        Expression::Call { func, args } => {
            resolve_func(ctx, func, &args)?;
            compile_call(ctx, func, args)
        }
        _ => unimplemented!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn new_ctx() -> Context {
        Context {}
    }

    fn compile(exp: Expression) -> Result<String, CompileError> {
        compile_expr(&mut new_ctx(), exp)
    }

    fn check(exp: Expression, r: &str) {
        assert_eq!(compile(exp).unwrap(), r.to_string());
    }

    #[test]
    fn num() {
        check(Expression::Num { val: "5" }, "5");
        check(Expression::Num { val: "2.3" }, "2.3");
    }

    #[test]
    fn variable() {
        check(Expression::Variable { val: "" }, "");
        check(Expression::Variable { val: "a" }, "a");
        check(Expression::Variable { val: "abc" }, "a_{bc}");
    }

    #[test]
    fn binary_expr() {
        check(
            Expression::BinaryExpr {
                left: Box::new(Expression::Num { val: "1" }),
                operator: "+",
                right: Box::new(Expression::Num { val: "2" }),
            },
            "1+2",
        )
    }

    #[test]
    fn unary_expression() {
        check(
            Expression::UnaryExpr {
                val: Box::new(Expression::Num { val: "2" }),
                operator: "!",
            },
            "2!",
        );
    }

    #[test]
    fn call() {
        assert_eq!(
            compile_call(
                &mut new_ctx(),
                "abc",
                vec![Box::new(Expression::Num { val: "1" })]
            )
            .unwrap(),
            "a_{bc}\\left(1\\right)",
        );
    }

    #[test]
    fn func_resolution() {
        check(
            Expression::Call {
                func: "sin",
                args: vec![Box::new(Expression::Num { val: "1" })],
            },
            // TODO: Should start with "\\sin"
            "s_{in}\\left(1\\right)",
        );
        assert_eq!(
            compile(Expression::Call {
                func: "abc",
                args: vec![],
            }),
            Err(CompileError::UnknownFunction("abc"))
        );
    }

    #[test]
    fn argc_validation() {
        assert_eq!(
            compile(Expression::Call {
                func: "sin",
                args: vec![],
            }),
            Err(CompileError::WrongArgCount {
                got: 0,
                expected: 1
            })
        );
        assert_eq!(
            compile(Expression::Call {
                func: "sin",
                args: vec![
                    Box::new(Expression::Num { val: "1" }),
                    Box::new(Expression::Num { val: "2" })
                ]
            }),
            Err(CompileError::WrongArgCount {
                got: 2,
                expected: 1,
            })
        );
    }
}
