use crate::{
    builtins,
    types::{CompileError, Expression, Function, ValType},
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

pub fn resolve_function<'a>(_ctx: &mut Context, func: &str) -> Option<&'a Function<'a>> {
    builtins::BUILTIN_FUNCTIONS.get(func)
}

pub fn compile_call<'a>(
    ctx: &mut Context,
    fname: &'a str,
    args: Vec<Box<Expression<'a>>>,
) -> Result<(String, ValType), CompileError<'a>> {
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
                let mut r = compile_identifier(fname);
                r.push_str("\\left(");

                let mut aiter = args.into_iter();
                for expect_type in (*func.args).iter() {
                    // Already checked that they are the same length, so unwrap is safe
                    let a = aiter.next().unwrap();

                    let (arg_latex, got_type) = compile_expr(ctx, *a)?;
                    if got_type != **expect_type {
                        return Err(CompileError::TypeMismatch {
                            got: got_type,
                            expected: **expect_type,
                        });
                    }

                    write!(r, "{}", arg_latex).unwrap();
                }

                r.push_str("\\right)");
                Ok((r, *func.ret))
            }
        }
    }
}

pub fn check_type<'a>(got: ValType, expect: ValType) -> Result<(), CompileError<'a>> {
    if got != expect {
        Err(CompileError::TypeMismatch {
            got: got,
            expected: expect,
        })
    } else {
        Ok(())
    }
}

// Combination of compile_expr and check_type
pub fn compile_expect<'a>(
    ctx: &mut Context,
    expr: Expression<'a>,
    expect: ValType,
) -> Result<String, CompileError<'a>> {
    let (s, t) = compile_expr(ctx, expr)?;
    check_type(t, expect)?;
    Ok(s)
}

pub fn compile_expr<'a>(
    ctx: &mut Context,
    expr: Expression<'a>,
) -> Result<(String, ValType), CompileError<'a>> {
    match expr {
        Expression::Num { val } => Ok((val.to_string(), ValType::Number)),
        // TODO: Resolve type of variable
        Expression::Variable { val } => Ok((compile_identifier(val), ValType::Number)),
        Expression::BinaryExpr {
            left,
            operator,
            right,
        } => Ok((
            format!(
                "{}{}{}",
                // Expect number because cannot do math on lists
                compile_expect(ctx, *left, ValType::Number)?,
                operator,
                compile_expect(ctx, *right, ValType::Number)?
            ),
            ValType::Number,
        )),
        Expression::UnaryExpr {
            val: v,
            operator: op,
        } => Ok((
            format!("{}{}", compile_expect(ctx, *v, ValType::Number)?, op),
            ValType::Number,
        )),
        Expression::Call { func, args } => compile_call(ctx, func, args),
        // TODO: Stringify it
        Expression::List(_) => Ok((String::new(), ValType::List)),
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
        Ok(compile_expr(&mut new_ctx(), exp)?.0)
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
    fn call_resolution() {
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

    #[test]
    fn call_arg_checking() {
        assert_eq!(
            compile(Expression::Call {
                func: "sin",
                args: vec![Box::new(Expression::List(vec![Box::new(
                    Expression::Num { val: "1" }
                )]))]
            }),
            Err(CompileError::TypeMismatch {
                got: ValType::List,
                expected: ValType::Number
            })
        );
    }

    #[test]
    fn binexp_typecheck() {
        assert_eq!(
            compile(Expression::BinaryExpr {
                left: Box::new(Expression::List(vec![Box::new(Expression::Num {
                    val: "1"
                })])),
                operator: "+",
                right: Box::new(Expression::Num { val: "2" })
            }),
            Err(CompileError::TypeMismatch {
                got: ValType::List,
                expected: ValType::Number
            })
        );
    }

    #[test]
    fn unary_typecheck() {
        assert_eq!(
            compile(Expression::UnaryExpr {
                val: Box::new(Expression::List(vec![Box::new(Expression::Num {
                    val: "1"
                })])),
                operator: "+",
            }),
            Err(CompileError::TypeMismatch {
                got: ValType::List,
                expected: ValType::Number
            })
        );
    }
}
