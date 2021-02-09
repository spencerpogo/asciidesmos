use crate::{
    builtins,
    types::{CompileError, CompileErrorKind, Expression, Function, LocatedExpression, ValType},
};
use pest::Span;
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
    span: Span<'a>,
    fname: &'a str,
    args: Vec<Box<LocatedExpression<'a>>>,
) -> Result<(String, ValType), CompileError<'a>> {
    match resolve_function(ctx, fname) {
        None => Err(CompileError {
            kind: CompileErrorKind::UnknownFunction(fname),
            span: span,
        }),
        Some(func) => {
            // Validate arg count
            let got = args.len();
            let expect = func.args.len();

            if got != expect {
                Err(CompileError {
                    kind: CompileErrorKind::WrongArgCount {
                        got: got,
                        expected: expect,
                    },
                    span: span,
                })
            } else {
                let mut r = compile_identifier(fname);
                r.push_str("\\left(");

                let mut aiter = args.into_iter();
                for expect_type in (*func.args).iter() {
                    // Already checked that they are the same length, so unwrap is safe
                    let a = aiter.next().unwrap();
                    let b = a.clone(); // TODO: maybe avoid cloning here?

                    let (arg_latex, got_type) = compile_expr(ctx, *a)?;
                    if got_type != **expect_type {
                        return Err(CompileError {
                            kind: CompileErrorKind::TypeMismatch {
                                got: got_type,
                                expected: **expect_type,
                            },
                            span: b.0,
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

pub fn check_type<'a>(
    span: Span<'a>,
    got: ValType,
    expect: ValType,
) -> Result<(), CompileError<'a>> {
    if got != expect {
        Err(CompileError {
            kind: CompileErrorKind::TypeMismatch {
                got: got,
                expected: expect,
            },
            span: span,
        })
    } else {
        Ok(())
    }
}

// Combination of compile_expr and check_type
pub fn compile_expect<'a>(
    ctx: &mut Context,
    span: Span<'a>,
    expr: LocatedExpression<'a>,
    expect: ValType,
) -> Result<String, CompileError<'a>> {
    let (s, t) = compile_expr(ctx, expr)?;
    check_type(span, t, expect)?;
    Ok(s)
}

pub fn compile_expr<'a>(
    ctx: &mut Context,
    expr: LocatedExpression<'a>,
) -> Result<(String, ValType), CompileError<'a>> {
    let span = expr.0;

    match expr.1 {
        Expression::Num { val } => Ok((val.to_string(), ValType::Number)),
        // TODO: Resolve type of variable
        Expression::Variable { val } => Ok((compile_identifier(val), ValType::Number)),
        Expression::BinaryExpr {
            left,
            operator,
            right,
        } => {
            let span2 = span.clone();
            Ok((
                format!(
                    "{}{}{}",
                    // Expect number because cannot do math on lists
                    compile_expect(ctx, span, *left, ValType::Number)?,
                    operator,
                    compile_expect(ctx, span2, *right, ValType::Number)?
                ),
                ValType::Number,
            ))
        }
        Expression::UnaryExpr {
            val: v,
            operator: op,
        } => Ok((
            format!("{}{}", compile_expect(ctx, span, *v, ValType::Number)?, op),
            ValType::Number,
        )),
        Expression::Call { func, args } => compile_call(ctx, span, func, args),
        // TODO: Stringify it
        Expression::List(values) => {
            let mut s = String::new();
            let mut first = true;

            for v in values.iter() {
                let v1 = v.clone();
                let v2 = v.clone();

                let val_str = compile_expect(ctx, v1.0, *v2, ValType::Number)?;
                if first {
                    write!(s, "{}", val_str).unwrap();
                    first = false;
                } else {
                    write!(s, ",{}", val_str).unwrap();
                }
            }

            Ok((format!("\\left[{}\\right]", s), ValType::List))
        }
        _ => unimplemented!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pest::Span;

    fn new_ctx() -> Context {
        Context {}
    }

    fn compile(exp: Expression) -> Result<String, CompileError> {
        Ok(compile_expr(&mut new_ctx(), (spn(), exp))?.0)
    }

    fn check(exp: Expression, r: &str) {
        assert_eq!(compile(exp).unwrap(), r.to_string());
    }

    #[inline]
    fn spn<'a>() -> Span<'a> {
        Span::new("", 0, 0).unwrap()
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
        let i = "1+2";
        check(
            Expression::BinaryExpr {
                left: Box::new((spn(), Expression::Num { val: "1" })),
                operator: "+",
                right: Box::new((spn(), Expression::Num { val: "2" })),
            },
            i,
        )
    }

    #[test]
    fn unary_expression() {
        let i = "2!";
        check(
            Expression::UnaryExpr {
                val: Box::new((spn(), Expression::Num { val: "2" })),
                operator: "!",
            },
            i,
        );
    }

    #[test]
    fn call_resolution() {
        let a = "s_{in}\\left(1\\right)";
        check(
            Expression::Call {
                func: "sin",
                args: vec![Box::new((spn(), Expression::Num { val: "1" }))],
            },
            // TODO: Should start with "\\sin"
            a,
        );
        assert_eq!(
            compile(Expression::Call {
                func: "abc",
                args: vec![],
            })
            .unwrap_err()
            .kind,
            CompileErrorKind::UnknownFunction("abc")
        );
    }

    #[test]
    fn argc_validation() {
        assert_eq!(
            compile(Expression::Call {
                func: "sin",
                args: vec![],
            })
            .unwrap_err()
            .kind,
            CompileErrorKind::WrongArgCount {
                got: 0,
                expected: 1
            }
        );
        assert_eq!(
            compile(Expression::Call {
                func: "sin",
                args: vec![
                    Box::new((spn(), Expression::Num { val: "1" })),
                    Box::new((spn(), Expression::Num { val: "2" }))
                ]
            })
            .unwrap_err()
            .kind,
            CompileErrorKind::WrongArgCount {
                got: 2,
                expected: 1,
            }
        );
    }

    #[test]
    fn call_arg_checking() {
        assert_eq!(
            compile(Expression::Call {
                func: "sin",
                args: vec![Box::new((
                    spn(),
                    Expression::List(vec![Box::new((spn(), Expression::Num { val: "1" }))])
                ))]
            })
            .unwrap_err()
            .kind,
            CompileErrorKind::TypeMismatch {
                got: ValType::List,
                expected: ValType::Number
            }
        );
    }

    #[test]
    fn binexp_typecheck() {
        assert_eq!(
            compile(Expression::BinaryExpr {
                left: Box::new((
                    spn(),
                    Expression::List(vec![Box::new((spn(), Expression::Num { val: "1" }))])
                )),
                operator: "+",
                right: Box::new((spn(), Expression::Num { val: "2" }))
            })
            .unwrap_err()
            .kind,
            CompileErrorKind::TypeMismatch {
                got: ValType::List,
                expected: ValType::Number
            }
        );
    }

    #[test]
    fn unary_typecheck() {
        assert_eq!(
            compile(Expression::UnaryExpr {
                val: Box::new((
                    spn(),
                    Expression::List(vec![Box::new((spn(), Expression::Num { val: "1" }))])
                )),
                operator: "+",
            })
            .unwrap_err()
            .kind,
            CompileErrorKind::TypeMismatch {
                got: ValType::List,
                expected: ValType::Number
            }
        );
    }

    #[test]
    fn list() {
        check(
            Expression::List(vec![Box::new((spn(), Expression::Num { val: "1" }))]),
            "\\left[1\\right]",
        );
        check(
            Expression::List(vec![
                Box::new((spn(), Expression::Num { val: "1" })),
                Box::new((spn(), Expression::Num { val: "2" })),
            ]),
            "\\left[1,2\\right]",
        );
    }

    #[test]
    fn list_typecheck() {
        assert_eq!(
            compile(Expression::List(vec![Box::new((
                spn(),
                Expression::List(vec![Box::new((spn(), Expression::Num { val: "1" }))])
            ))]))
            .unwrap_err()
            .kind,
            CompileErrorKind::TypeMismatch {
                got: ValType::List,
                expected: ValType::Number
            }
        );
    }
}
