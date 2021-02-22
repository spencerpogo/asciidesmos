use crate::{
    builtins,
    types::{CompileError, CompileErrorKind, Expression, Function, LocatedExpression, ValType},
};
use pest::Span;
use std::collections::HashMap;
use std::fmt::Write;

pub struct Context<'a> {
    pub variables: HashMap<&'a str, ValType>,
}

impl Context<'_> {
    pub fn new() -> Self {
        Context {
            variables: HashMap::new(),
        }
    }
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

// Returns function and whether it is builtin
pub fn resolve_function<'a>(_ctx: &mut Context, func: &str) -> Option<(&'a Function<'a>, bool)> {
    match builtins::BUILTIN_FUNCTIONS.get(func) {
        None => None,
        Some(f) => Some((f, true)),
    }
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
        Some((func, is_builtin)) => {
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
                // Builtins are prefixed with a backslash and are not in the identifier
                //  form
                let mut r = if is_builtin {
                    format!("\\{}", fname)
                } else {
                    compile_identifier(fname)
                };
                r.push_str("\\left(");

                let mut aiter = args.into_iter();
                for expect_type in (*func.args).iter() {
                    // Already checked that they are the same length, so unwrap is safe
                    let a = aiter.next().unwrap();
                    let b = a.clone(); // TODO: maybe avoid cloning here?

                    let (arg_latex, got_type) = compile_expr(ctx, *a)?;
                    if got_type != *expect_type {
                        return Err(CompileError {
                            kind: CompileErrorKind::TypeMismatch {
                                got: got_type,
                                expected: *expect_type,
                            },
                            span: b.0,
                        });
                    }

                    write!(r, "{}", arg_latex).unwrap();
                }

                r.push_str("\\right)");
                Ok((r, func.ret))
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

// Ideally this would be functional and ctx would not need to be mutable, but rust
//  support for immutable hashmaps isn't built in and mutation is much simpler.
pub fn compile_expr<'a>(
    ctx: &mut Context,
    expr: LocatedExpression<'a>,
) -> Result<(String, ValType), CompileError<'a>> {
    let span = expr.0;

    match expr.1 {
        Expression::Num { val } => Ok((val.to_string(), ValType::Number)),
        Expression::Variable { val } => match ctx.variables.get(val) {
            Some(var_type) => Ok((compile_identifier(val), *var_type)),
            None => Err(CompileError {
                kind: CompileErrorKind::UndefinedVariable(val),
                span: span,
            }),
        },
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
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pest::Span;

    fn new_ctx<'a>() -> Context<'a> {
        Context::new()
    }

    fn compile(exp: Expression) -> Result<String, CompileError> {
        compile_with_ctx(&mut new_ctx(), exp)
    }

    fn compile_with_ctx<'a>(
        ctx: &mut Context,
        exp: Expression<'a>,
    ) -> Result<String, CompileError<'a>> {
        Ok(compile_expr(ctx, (spn(), exp))?.0)
    }

    fn check(exp: Expression, r: &str) {
        assert_eq!(compile(exp).unwrap(), r.to_string());
    }

    fn comp_with_var<'a>(
        v: &str,
        vtype: ValType,
        exp: Expression<'a>,
    ) -> Result<String, CompileError<'a>> {
        let mut ctx = new_ctx();
        ctx.variables.insert(v, vtype);
        compile_with_ctx(&mut ctx, exp)
    }

    fn check_with_var<'a>(v: &str, vtype: ValType, exp: Expression<'a>, r: &'a str) {
        assert_eq!(comp_with_var(v, vtype, exp).unwrap(), r);
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
        check_with_var("a", ValType::Number, Expression::Variable { val: "a" }, "a");
        check_with_var(
            "abc",
            ValType::Number,
            Expression::Variable { val: "abc" },
            "a_{bc}",
        );
    }

    #[test]
    fn variable_resolution() {
        assert_eq!(
            compile(Expression::Variable { val: "" }).unwrap_err().kind,
            CompileErrorKind::UndefinedVariable("")
        );
        assert_eq!(
            compile(Expression::Variable { val: "abc" })
                .unwrap_err()
                .kind,
            CompileErrorKind::UndefinedVariable("abc")
        );
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
        check(
            Expression::Call {
                func: "sin",
                args: vec![Box::new((spn(), Expression::Num { val: "1" }))],
            },
            // TODO: Should start with "\\sin"
            "\\sin\\left(1\\right)",
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
