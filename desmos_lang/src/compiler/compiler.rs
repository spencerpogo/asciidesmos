use super::{
    builtins,
    error::{CompileError, CompileErrorKind},
};
use crate::core::{
    ast::{Expression, LocatedExpression, LocatedStatement, Statement},
    latex::{BinaryOperator, Latex},
    runtime::ValType,
};
use pest::Span;
use std::collections::HashMap;
use std::rc::Rc;

pub struct FunctionSignature {
    pub args: Vec<ValType>,
    pub ret: ValType,
}

pub struct Context<'a> {
    pub variables: HashMap<&'a str, ValType>,
    pub locals: HashMap<&'a str, ValType>,
    pub defined_functions: HashMap<&'a str, Rc<FunctionSignature>>,
    pub inside_map_macro: bool,
}

impl Context<'_> {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            locals: HashMap::new(),
            defined_functions: HashMap::new(),
            inside_map_macro: false,
        }
    }
}

impl Default for Context<'_> {
    fn default() -> Self {
        Self::new()
    }
}

// Returns function and whether it is builtin
pub fn resolve_function<'a>(
    ctx: &'a mut Context,
    func: &str,
) -> Option<(Rc<FunctionSignature>, bool)> {
    match ctx.defined_functions.get(func) {
        None => match builtins::BUILTIN_FUNCTIONS.get(func) {
            None => None,
            Some(f) => Some((
                Rc::new(FunctionSignature {
                    args: f.args.to_vec(),
                    ret: f.ret,
                }),
                true,
            )),
        },
        Some(f) => Some((f.clone(), false)),
    }
}

pub fn resolve_variable<'a>(ctx: &'a mut Context, var: &str) -> Option<&'a ValType> {
    match ctx.variables.get(var) {
        Some(r) => Some(r),
        None => ctx.locals.get(var),
    }
}

pub fn compile_call<'a>(
    ctx: &mut Context,
    span: Span<'a>,
    fname: &'a str,
    args: Vec<(Span<'a>, Latex, ValType)>,
) -> Result<(Latex, ValType), CompileError<'a>> {
    match resolve_function(ctx, fname) {
        None => Err(CompileError {
            kind: CompileErrorKind::UnknownFunction(fname),
            span,
        }),
        Some((func, is_builtin)) => {
            // Validate arg count
            let got = args.len();
            let expect = func.args.len();

            if got != expect {
                Err(CompileError {
                    kind: CompileErrorKind::WrongArgCount {
                        got,
                        expected: expect,
                    },
                    span,
                })
            } else {
                let mut aiter = args.into_iter();
                let args_latex = func
                    .args
                    .iter()
                    .map(|expect_type| -> Result<Latex, _> {
                        // Already checked that they are the same length, so unwrap is safe
                        let (aspan, arg_latex, got_type) = aiter.next().unwrap();
                        let type_errors_ok = ctx.inside_map_macro
                            && got_type == ValType::List
                            && *expect_type == ValType::Number;
                        if !type_errors_ok && got_type != *expect_type {
                            return Err(CompileError {
                                kind: CompileErrorKind::TypeMismatch {
                                    got: got_type,
                                    expected: *expect_type,
                                },
                                span: aspan,
                            });
                        }
                        Ok(arg_latex)
                    })
                    .collect::<Result<Vec<Latex>, _>>()?;

                Ok((
                    Latex::Call {
                        func: fname.to_string(),
                        is_builtin,
                        args: args_latex,
                    },
                    func.ret,
                ))
            }
        }
    }
}

pub fn check_type(span: Span, got: ValType, expect: ValType) -> Result<(), CompileError> {
    if got != expect {
        Err(CompileError {
            kind: CompileErrorKind::TypeMismatch {
                got,
                expected: expect,
            },
            span,
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
) -> Result<Latex, CompileError<'a>> {
    let (s, t) = compile_expr(ctx, expr)?;
    check_type(span, t, expect)?;
    Ok(s)
}

pub fn handle_map_macro<'a>(
    ctx: &mut Context,
    span: Span<'a>,
    args: Vec<LocatedExpression<'a>>,
) -> Result<(Latex, ValType), CompileError<'a>> {
    if args.len() < 2 {
        return Err(CompileError {
            span,
            kind: CompileErrorKind::BadMapMacro,
        });
    }

    let mut argsiter = args.into_iter();
    let (fspan, fexpr) = argsiter.next().unwrap();
    match fexpr {
        Expression::Variable(fname) => {
            let call_args = argsiter
                .map(
                    |(aspan, aexpr)| -> Result<(Span, Latex, ValType), CompileError> {
                        let (latex, t) = compile_expr(ctx, (aspan.clone(), aexpr))?;
                        Ok((aspan, latex, t))
                    },
                )
                .collect::<Result<Vec<(Span, Latex, ValType)>, CompileError>>()?;
            //compile_expect(ctx, lspan.clone(), (lspan, lexpr), ValType::List)?;
            // There should be no situtation in which ctx.inside_map_macro is currently
            //  true, but save it's old state anyway.
            let was_inside_map_macro = ctx.inside_map_macro;
            ctx.inside_map_macro = true;
            let r = compile_call(ctx, span, fname, call_args);
            ctx.inside_map_macro = was_inside_map_macro;
            r
        }
        _ => Err(CompileError {
            span: fspan,
            kind: CompileErrorKind::ExpectedFunction,
        }),
    }
}

pub fn handle_macro<'a>(
    ctx: &mut Context,
    span: Span<'a>,
    name: &'a str,
    args: Vec<LocatedExpression<'a>>,
) -> Result<(Latex, ValType), CompileError<'a>> {
    match name {
        "map" => handle_map_macro(ctx, span, args),
        _ => Err(CompileError {
            span,
            kind: CompileErrorKind::UndefinedMacro(name),
        }),
    }
}

// Ideally this would be functional and ctx would not need to be mutable, but rust
//  support for immutable hashmaps isn't built in and mutation is much simpler.
pub fn compile_expr<'a>(
    ctx: &mut Context,
    expr: LocatedExpression<'a>,
) -> Result<(Latex, ValType), CompileError<'a>> {
    let span = expr.0;

    match expr.1 {
        Expression::Num(val) => Ok((Latex::Num(val.to_string()), ValType::Number)),
        Expression::Variable(val) => match resolve_variable(ctx, val) {
            Some(var_type) => Ok((Latex::Variable(val.to_string()), *var_type)),
            None => Err(CompileError {
                kind: CompileErrorKind::UndefinedVariable(val),
                span,
            }),
        },
        Expression::BinaryExpr {
            left,
            operator,
            right,
        } => {
            let span2 = span.clone();
            Ok((
                Latex::BinaryExpression {
                    left: Box::new(compile_expect(ctx, span, *left, ValType::Number)?),
                    // TODO: Do this in parser
                    operator: match operator {
                        "+" => BinaryOperator::Add,
                        "-" => BinaryOperator::Subtract,
                        "*" => BinaryOperator::Multiply,
                        "/" => BinaryOperator::Divide,
                        _ => unreachable!(),
                    },
                    right: Box::new(compile_expect(ctx, span2, *right, ValType::Number)?),
                },
                ValType::Number,
            ))
        }
        Expression::UnaryExpr {
            val: v,
            operator: op,
        } => Ok((
            Latex::UnaryExpression {
                left: Box::new(compile_expect(ctx, span, *v, ValType::Number)?),
                operator: op.to_string(),
            },
            ValType::Number,
        )),
        Expression::Call { func, args } => {
            let compiled_args = args
                .into_iter()
                .map(|(s, e)| -> Result<(Span, Latex, ValType), CompileError> {
                    let (latex, t) = compile_expr(ctx, (s.clone(), e))?;
                    Ok((s, latex, t))
                })
                .collect::<Result<Vec<(Span, Latex, ValType)>, CompileError>>()?;
            compile_call(ctx, span, func, compiled_args)
        }
        Expression::List(values) => {
            let items = values
                .into_iter()
                .map(|(s, e)| -> Result<Latex, CompileError> {
                    let (latex, vtype) = compile_expr(ctx, (s.clone(), e))?;
                    if vtype != ValType::Number {
                        Err(CompileError {
                            span: s,
                            kind: CompileErrorKind::NoNestedList,
                        })
                    } else {
                        Ok(latex)
                    }
                })
                .collect::<Result<Vec<Latex>, CompileError>>()?;

            Ok((Latex::List(items), ValType::List))
        }
        Expression::MacroCall { name, args } => handle_macro(ctx, span, name, args),
    }
}

pub fn compile_stmt<'a>(
    ctx: &mut Context<'a>,
    expr: LocatedStatement<'a>,
) -> Result<Latex, CompileError<'a>> {
    let s = expr.0;

    match expr.1 {
        Statement::Expression(e) => Ok(compile_expr(ctx, (s, e))?.0),
        Statement::FuncDef(fdef, e) => {
            // Clone a copy we can restore later
            let old_locals = ctx.locals.clone();
            // Add args into locals
            for (aname, atype) in fdef.args.iter() {
                ctx.locals.insert(aname, *atype);
            }
            let span = e.0.clone();
            // Evaluate the body with the new ctx
            let (body, ret) = compile_expr(ctx, e)?;
            // Validate the return type annotation
            if let Some(retann) = fdef.ret_annotation {
                check_type(span, ret, retann)?;
            }
            // restore old locals
            ctx.locals = old_locals;

            // Add function to context
            ctx.defined_functions.insert(
                fdef.name,
                Rc::new(FunctionSignature {
                    args: fdef.args.iter().map(|a| a.1).collect(),
                    ret,
                }),
            );

            Ok(Latex::FuncDef {
                name: fdef.name.to_string(),
                args: fdef.args.iter().map(|a| a.0.to_string()).collect(),
                body: Box::new(body),
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::ast::FunctionDefinition;
    use pest::Span;

    fn new_ctx<'a>() -> Context<'a> {
        Context::new()
    }

    fn compile(exp: Expression) -> Result<Latex, CompileError> {
        compile_with_ctx(&mut new_ctx(), exp)
    }

    fn compile_with_ctx<'a>(
        ctx: &mut Context,
        exp: Expression<'a>,
    ) -> Result<Latex, CompileError<'a>> {
        Ok(compile_expr(ctx, (spn(), exp))?.0)
    }

    fn compile_stmt(stmt: Statement) -> Result<Latex, CompileError> {
        compile_stmt_with_ctx(&mut new_ctx(), stmt)
    }

    fn compile_stmt_with_ctx<'a>(
        ctx: &mut Context<'a>,
        stmt: Statement<'a>,
    ) -> Result<Latex, CompileError<'a>> {
        super::compile_stmt(ctx, (spn(), stmt))
    }

    fn check_stmt(stmt: Statement, r: Latex) {
        assert_eq!(compile_stmt(stmt).unwrap(), r);
    }

    fn check(exp: Expression, r: Latex) {
        assert_eq!(compile(exp).unwrap(), r);
    }

    fn comp_with_var<'a>(
        v: &str,
        vtype: ValType,
        exp: Expression<'a>,
    ) -> Result<Latex, CompileError<'a>> {
        let mut ctx = new_ctx();
        ctx.variables.insert(v, vtype);
        compile_with_ctx(&mut ctx, exp)
    }

    fn check_with_var<'a>(v: &str, vtype: ValType, exp: Expression<'a>, r: Latex) {
        assert_eq!(comp_with_var(v, vtype, exp), Ok(r));
    }

    #[inline]
    fn spn<'a>() -> Span<'a> {
        Span::new("", 0, 0).unwrap()
    }

    #[test]
    fn num() {
        check(Expression::Num("5"), Latex::Num("5".to_string()));
        check(Expression::Num("2.3"), Latex::Num("2.3".to_string()));
    }

    #[test]
    fn variable() {
        check_with_var(
            "a",
            ValType::Number,
            Expression::Variable("a"),
            Latex::Variable("a".to_string()),
        );
        check_with_var(
            "abc",
            ValType::Number,
            Expression::Variable("abc"),
            Latex::Variable("abc".to_string()),
        );
    }

    #[test]
    fn variable_resolution() {
        assert_eq!(
            compile(Expression::Variable("")).unwrap_err().kind,
            CompileErrorKind::UndefinedVariable("")
        );
        assert_eq!(
            compile(Expression::Variable("abc")).unwrap_err().kind,
            CompileErrorKind::UndefinedVariable("abc")
        );
    }

    #[test]
    fn binary_expr() {
        check(
            Expression::BinaryExpr {
                left: Box::new((spn(), Expression::Num("1"))),
                operator: "+",
                right: Box::new((spn(), Expression::Num("2"))),
            },
            Latex::BinaryExpression {
                left: Box::new(Latex::Num("1".to_string())),
                operator: BinaryOperator::Add,
                right: Box::new(Latex::Num("2".to_string())),
            },
        )
    }

    #[test]
    fn unary_expression() {
        check(
            Expression::UnaryExpr {
                val: Box::new((spn(), Expression::Num("2"))),
                operator: "!",
            },
            Latex::UnaryExpression {
                left: Box::new(Latex::Num("2".to_string())),
                operator: "!".to_string(),
            },
        );
    }

    #[test]
    fn call_resolution() {
        check(
            Expression::Call {
                func: "sin",
                args: vec![(spn(), Expression::Num("1"))],
            },
            Latex::Call {
                func: "sin".to_string(),
                is_builtin: true,
                args: vec![Latex::Num("1".to_string())],
            },
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
                args: vec![(spn(), Expression::Num("1")), (spn(), Expression::Num("2"))]
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
                args: vec![(spn(), Expression::List(vec![(spn(), Expression::Num("1"))]))]
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
                left: Box::new((spn(), Expression::List(vec![(spn(), Expression::Num("1"))]))),
                operator: "+",
                right: Box::new((spn(), Expression::Num("2")))
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
                val: Box::new((spn(), Expression::List(vec![(spn(), Expression::Num("1"))]))),
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
            Expression::List(vec![(spn(), Expression::Num("1"))]),
            Latex::List(vec![Latex::Num("1".to_string())]),
        );
        check(
            Expression::List(vec![
                (spn(), Expression::Num("1")),
                (spn(), Expression::Num("2")),
            ]),
            Latex::List(vec![
                Latex::Num("1".to_string()),
                Latex::Num("2".to_string()),
            ]),
        );
    }

    #[test]
    fn list_typecheck() {
        assert_eq!(
            compile(Expression::List(vec![(
                spn(),
                Expression::List(vec![(spn(), Expression::Num("1"))])
            )])),
            Err(CompileError {
                span: spn(),
                kind: CompileErrorKind::NoNestedList
            })
        );
    }

    #[test]
    fn expression_stmt() {
        check_stmt(
            Statement::Expression(Expression::Num("1")),
            Latex::Num("1".to_string()),
        );
    }

    #[test]
    fn funcdef_single_arg() {
        check_stmt(
            Statement::FuncDef(
                FunctionDefinition {
                    name: "abc",
                    args: vec![("def", ValType::Number)],
                    ret_annotation: None,
                },
                (spn(), Expression::Num("1")),
            ),
            Latex::FuncDef {
                name: "abc".to_string(),
                args: vec!["def".to_string()],
                body: Box::new(Latex::Num("1".to_string())),
            },
        );
    }

    #[test]
    fn funcdef_many_args() {
        check_stmt(
            Statement::FuncDef(
                FunctionDefinition {
                    name: "f",
                    args: vec![("abc", ValType::List), ("def", ValType::Number)],
                    ret_annotation: None,
                },
                (spn(), Expression::Num("1")),
            ),
            Latex::FuncDef {
                name: "f".to_string(),
                args: vec!["abc".to_string(), "def".to_string()],
                body: Box::new(Latex::Num("1".to_string())),
            },
        );
    }

    #[test]
    fn funcdef_can_use_args() {
        let mut ctx = new_ctx();
        assert_eq!(
            compile_stmt_with_ctx(
                &mut ctx,
                Statement::FuncDef(
                    FunctionDefinition {
                        name: "f",
                        args: vec![("a", ValType::Number)],
                        ret_annotation: None,
                    },
                    (spn(), Expression::Variable("a")),
                )
            ),
            Ok(Latex::FuncDef {
                name: "f".to_string(),
                args: vec!["a".to_string()],
                body: Box::new(Latex::Variable("a".to_string())),
            },)
        );
        // Check that the variable is no longer in scope
        assert_eq!(
            compile_with_ctx(&mut ctx, Expression::Variable("a")),
            Err(CompileError {
                span: spn(),
                kind: CompileErrorKind::UndefinedVariable("a")
            })
        )
    }

    #[test]
    fn funcdef_ret_annotation_checked() {
        assert_eq!(
            compile_stmt(Statement::FuncDef(
                FunctionDefinition {
                    name: "f",
                    args: vec![("a", ValType::Number)],
                    ret_annotation: Some(ValType::List),
                },
                (spn(), Expression::Num("1")),
            ))
            .unwrap_err(),
            CompileError {
                kind: CompileErrorKind::TypeMismatch {
                    got: ValType::Number,
                    expected: ValType::List
                },
                span: spn()
            },
        );
    }

    #[test]
    fn funcdef_arg_leave_scope() {
        let mut ctx = new_ctx();
        compile_stmt_with_ctx(
            &mut ctx,
            Statement::FuncDef(
                FunctionDefinition {
                    name: "f",
                    args: vec![("a", ValType::Number)],
                    ret_annotation: None,
                },
                (spn(), Expression::Variable("a")),
            ),
        )
        .unwrap();
        assert_eq!(
            compile_stmt_with_ctx(&mut ctx, Statement::Expression(Expression::Variable("a")))
                .unwrap_err(),
            CompileError {
                kind: CompileErrorKind::UndefinedVariable("a"),
                span: spn()
            }
        );
    }

    #[test]
    fn funcdef_func_callable() {
        let mut ctx = new_ctx();
        compile_stmt_with_ctx(
            &mut ctx,
            Statement::FuncDef(
                FunctionDefinition {
                    name: "f",
                    args: vec![("a", ValType::Number)],
                    ret_annotation: None,
                },
                (spn(), Expression::Variable("a")),
            ),
        )
        .unwrap();
        compile_stmt_with_ctx(
            &mut ctx,
            Statement::Expression(Expression::Call {
                func: "f",
                args: vec![(spn(), Expression::Num("1"))],
            }),
        )
        .unwrap();
    }

    #[test]
    fn funcdef_func_argslen() {
        let mut ctx = new_ctx();
        compile_stmt_with_ctx(
            &mut ctx,
            Statement::FuncDef(
                FunctionDefinition {
                    name: "f",
                    args: vec![],
                    ret_annotation: None,
                },
                (spn(), Expression::Num("1")),
            ),
        )
        .unwrap();
        assert_eq!(
            compile_stmt_with_ctx(
                &mut ctx,
                Statement::Expression(Expression::Call {
                    func: "f",
                    args: vec![(spn(), Expression::Num("1"))],
                }),
            )
            .unwrap_err(),
            CompileError {
                span: spn(),
                kind: CompileErrorKind::WrongArgCount {
                    got: 1,
                    expected: 0,
                }
            }
        );
    }

    #[test]
    fn funcdef_args_typecheck() {
        let mut ctx = new_ctx();
        compile_stmt_with_ctx(
            &mut ctx,
            Statement::FuncDef(
                FunctionDefinition {
                    name: "f",
                    args: vec![("a", ValType::Number)],
                    ret_annotation: None,
                },
                (spn(), Expression::Num("1")),
            ),
        )
        .unwrap();
        assert_eq!(
            compile_stmt_with_ctx(
                &mut ctx,
                Statement::Expression(Expression::Call {
                    func: "f",
                    args: vec![(spn(), Expression::List(vec![]))],
                }),
            )
            .unwrap_err(),
            CompileError {
                span: spn(),
                kind: CompileErrorKind::TypeMismatch {
                    expected: ValType::Number,
                    got: ValType::List
                }
            }
        );
    }

    #[test]
    fn unknown_macro_errors() {
        let name = "does_not_exist";
        assert_eq!(
            compile(Expression::MacroCall { name, args: vec![] }),
            Err(CompileError {
                span: spn(),
                kind: CompileErrorKind::UndefinedMacro(name)
            })
        );
    }

    #[test]
    fn map_macro_works() {
        check(
            Expression::MacroCall {
                name: "map",
                args: vec![
                    (spn(), Expression::Variable("sin")),
                    (
                        spn(),
                        Expression::List(vec![
                            (spn(), Expression::Num("1")),
                            (spn(), Expression::Num("2")),
                        ]),
                    ),
                ],
            },
            Latex::Call {
                func: "sin".to_string(),
                is_builtin: true,
                args: vec![Latex::List(vec![
                    Latex::Num("1".to_string()),
                    Latex::Num("2".to_string()),
                ])],
            },
        );
    }

    #[test]
    fn map_macro_multi_args() {
        let mut ctx = new_ctx();
        ctx.defined_functions.insert(
            "multi_arg_test",
            Rc::new(FunctionSignature {
                args: vec![ValType::Number, ValType::Number, ValType::Number],
                ret: ValType::Number,
            }),
        );
        assert_eq!(
            compile_with_ctx(
                &mut ctx,
                Expression::MacroCall {
                    name: "map",
                    args: vec![
                        (spn(), Expression::Variable("multi_arg_test")),
                        (
                            spn(),
                            Expression::List(vec![
                                (spn(), Expression::Num("1")),
                                (spn(), Expression::Num("2")),
                                (spn(), Expression::Num("3"))
                            ])
                        ),
                        (spn(), Expression::Num("4")),
                        (
                            spn(),
                            Expression::List(vec![
                                (spn(), Expression::Num("5")),
                                (spn(), Expression::Num("6")),
                                (spn(), Expression::Num("7"))
                            ])
                        ),
                    ]
                }
            ),
            Ok(Latex::Call {
                func: "multi_arg_test".to_string(),
                is_builtin: false,
                args: vec![
                    Latex::List(vec![
                        Latex::Num("1".to_string()),
                        Latex::Num("2".to_string()),
                        Latex::Num("3".to_string())
                    ]),
                    Latex::Num("4".to_string()),
                    Latex::List(vec![
                        Latex::Num("5".to_string()),
                        Latex::Num("6".to_string()),
                        Latex::Num("7".to_string())
                    ])
                ]
            })
        )
    }

    #[test]
    fn map_macro_handles_errors() {
        assert_eq!(
            compile(Expression::MacroCall {
                name: "map",
                args: vec![],
            }),
            Err(CompileError {
                span: spn(),
                kind: CompileErrorKind::BadMapMacro
            })
        )
    }

    #[test]
    fn map_macro_checks_fn_type() {
        assert_eq!(
            compile(Expression::MacroCall {
                name: "map",
                args: vec![
                    (spn(), Expression::Num("1")),
                    (spn(), Expression::List(vec![]))
                ]
            }),
            Err(CompileError {
                span: spn(),
                kind: CompileErrorKind::ExpectedFunction
            })
        );
    }
}
