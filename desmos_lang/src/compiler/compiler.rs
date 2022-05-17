use super::{
    error::{CompileError, CompileErrorKind},
    types::{Context, FunctionArgs, FunctionSignature},
};
use ast::{
    BinaryOperator, Branch, Expression, LocatedExpression, LocatedStatement, Statement,
    UnaryOperator,
};
use latex::{
    self, BinaryOperator as LatexBinaryOperator, Cond, Latex, UnaryOperator as LatexUnaryOperator,
};
use std::rc::Rc;
use types::ValType;

pub fn resolve_variable<'a>(ctx: &'a mut Context, var: String) -> Option<&'a ValType> {
    match ctx.variables.get(&*var) {
        Some(r) => Some(r),
        None => ctx.locals.get(&*var),
    }
}

pub fn check_type(span: types::Span, got: ValType, expect: ValType) -> Result<(), CompileError> {
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
    span: types::Span,
    expr: LocatedExpression,
    expect: ValType,
) -> Result<Latex, CompileError> {
    let (s, t) = compile_expr(ctx, expr)?;
    check_type(span, t, expect)?;
    Ok(s)
}

pub fn binop_to_latex(op: BinaryOperator) -> LatexBinaryOperator {
    match op {
        BinaryOperator::Add => LatexBinaryOperator::Add,
        BinaryOperator::Subtract => LatexBinaryOperator::Subtract,
        BinaryOperator::Multiply => LatexBinaryOperator::Multiply,
        BinaryOperator::Divide => LatexBinaryOperator::Divide,
        BinaryOperator::Mod => unreachable!(),
    }
}

pub fn unop_to_latex(op: UnaryOperator) -> LatexUnaryOperator {
    match op {
        UnaryOperator::Negate => LatexUnaryOperator::Negate,
        UnaryOperator::Factorial => LatexUnaryOperator::Factorial,
    }
}

pub fn branch_to_cond<'a>(ctx: &mut Context, branch: Branch) -> Result<Cond, CompileError> {
    let leftcondspan = branch.cond_left.0.clone();
    Ok(Cond {
        left: compile_expect(ctx, leftcondspan, branch.cond_left, ValType::Number)?,
        op: branch.cond,
        right: compile_expr(ctx, branch.cond_right)?.0,
        result: compile_expr(ctx, branch.val)?.0,
    })
}

// Ideally this would be functional and ctx would not need to be mutable, but rust
//  support for immutable hashmaps isn't built in and mutation is much simpler.
pub fn compile_expr<'a>(
    ctx: &mut Context,
    expr: LocatedExpression,
) -> Result<(Latex, ValType), CompileError> {
    let span = expr.0;

    match expr.1 {
        Expression::Num(val) => Ok((Latex::Num(val.to_string()), ValType::Number)),
        Expression::Variable(val) => match resolve_variable(ctx, val.clone()) {
            Some(var_type) => Ok((Latex::Variable(val), *var_type)),
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
            let lv = compile_expect(ctx, span, *left, ValType::Number)?;
            let rv = compile_expect(ctx, span2, *right, ValType::Number)?;
            Ok((
                match operator {
                    BinaryOperator::Mod => Latex::Call {
                        func: latex::Function::Normal {
                            name: "mod".to_string(),
                        },
                        is_builtin: true,
                        args: vec![lv, rv],
                    },
                    _ => Latex::BinaryExpression {
                        left: Box::new(lv),
                        operator: binop_to_latex(operator),
                        right: Box::new(rv),
                    },
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
                operator: unop_to_latex(op),
            },
            ValType::Number,
        )),
        Expression::Call {
            modifier,
            func,
            args,
        } => {
            let compiled_args = args
                .into_iter()
                .map(
                    |(s, e)| -> Result<(types::Span, Latex, ValType), CompileError> {
                        let (latex, t) = compile_expr(ctx, (s.clone(), e))?;
                        Ok((s, latex, t))
                    },
                )
                .collect::<Result<Vec<(types::Span, Latex, ValType)>, CompileError>>()?;
            super::call::compile_call(ctx, span, func, modifier, compiled_args)
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
        Expression::Piecewise {
            first,
            rest,
            default,
        } => {
            let def = *default;
            let dspan = def.0.clone();
            Ok((
                Latex::Piecewise {
                    first: Box::new(branch_to_cond(ctx, *first)?),
                    rest: rest
                        .into_iter()
                        .map(|b| branch_to_cond(ctx, b))
                        .collect::<Result<Vec<_>, _>>()?,
                    default: Box::new(compile_expect(ctx, dspan, def, ValType::Number)?),
                },
                ValType::Number,
            ))
        }
    }
}

pub fn compile_stmt<'a>(
    ctx: &mut Context<'a>,
    expr: LocatedStatement,
) -> Result<Latex, CompileError> {
    let s = expr.0;

    match expr.1 {
        Statement::Expression(e) => Ok(compile_expr(ctx, (s, e))?.0),
        Statement::FuncDef(fdef, e) => {
            // Clone a copy we can restore later
            let old_locals = ctx.locals.clone();
            // Add args into locals
            for (aname, atype) in fdef.args.iter() {
                ctx.locals.insert(aname.clone(), *atype);
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
                fdef.name.clone(),
                Rc::new(FunctionSignature {
                    args: FunctionArgs::Static(fdef.args.iter().map(|a| a.1).collect()),
                    ret,
                }),
            );

            Ok(Latex::FuncDef {
                name: fdef.name,
                args: fdef.args.iter().map(|a| a.0.to_string()).collect(),
                body: Box::new(body),
            })
        }
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use ast::{self, FunctionDefinition};
    use types::CompareOperator;

    pub fn new_ctx<'a>() -> Context<'a> {
        Context::new()
    }

    pub fn compile(exp: Expression) -> Result<Latex, CompileError> {
        compile_with_ctx(&mut new_ctx(), exp)
    }

    pub fn compile_with_ctx<'a>(ctx: &mut Context, exp: Expression) -> Result<Latex, CompileError> {
        Ok(compile_expr(ctx, (spn(), exp))?.0)
    }

    pub fn compile_stmt(stmt: Statement) -> Result<Latex, CompileError> {
        compile_stmt_with_ctx(&mut new_ctx(), stmt)
    }

    pub fn compile_stmt_with_ctx<'a>(
        ctx: &mut Context<'a>,
        stmt: Statement,
    ) -> Result<Latex, CompileError> {
        super::compile_stmt(ctx, (spn(), stmt))
    }

    pub fn check_stmt(stmt: Statement, r: Latex) {
        assert_eq!(compile_stmt(stmt).unwrap(), r);
    }

    pub fn check(exp: Expression, r: Latex) {
        assert_eq!(compile(exp).unwrap(), r);
    }

    pub fn comp_with_var<'a>(
        v: &str,
        vtype: ValType,
        exp: Expression,
    ) -> Result<Latex, CompileError> {
        let mut ctx = new_ctx();
        ctx.variables.insert(v, vtype);
        compile_with_ctx(&mut ctx, exp)
    }

    pub fn check_with_var<'a>(v: &str, vtype: ValType, exp: Expression, r: Latex) {
        assert_eq!(comp_with_var(v, vtype, exp), Ok(r));
    }

    #[inline]
    pub fn spn<'a>() -> types::Span {
        types::Span::new(1234, 0..0)
    }

    #[test]
    fn num() {
        check(
            Expression::Num("5".to_string()),
            Latex::Num("5".to_string()),
        );
        check(
            Expression::Num("2.3".to_string()),
            Latex::Num("2.3".to_string()),
        );
    }

    #[test]
    fn variable() {
        check_with_var(
            "a",
            ValType::Number,
            Expression::Variable("a".to_string()),
            Latex::Variable("a".to_string()),
        );
        check_with_var(
            "abc",
            ValType::Number,
            Expression::Variable("abc".to_string()),
            Latex::Variable("abc".to_string()),
        );
    }

    #[test]
    fn variable_resolution() {
        assert_eq!(
            compile(Expression::Variable("".to_string()))
                .unwrap_err()
                .kind,
            CompileErrorKind::UndefinedVariable("".to_string())
        );
        assert_eq!(
            compile(Expression::Variable("abc".to_string()))
                .unwrap_err()
                .kind,
            CompileErrorKind::UndefinedVariable("abc".to_string())
        );
    }

    #[test]
    fn binary_expr() {
        check(
            Expression::BinaryExpr {
                left: Box::new((spn(), Expression::Num("1".to_string()))),
                operator: BinaryOperator::Add,
                right: Box::new((spn(), Expression::Num("2".to_string()))),
            },
            Latex::BinaryExpression {
                left: Box::new(Latex::Num("1".to_string())),
                operator: LatexBinaryOperator::Add,
                right: Box::new(Latex::Num("2".to_string())),
            },
        )
    }

    #[test]
    fn test_mod() {
        check(
            Expression::BinaryExpr {
                left: Box::new((spn(), Expression::Num("1".to_string()))),
                operator: BinaryOperator::Mod,
                right: Box::new((spn(), Expression::Num("2".to_string()))),
            },
            Latex::Call {
                func: latex::Function::Normal {
                    name: "mod".to_string(),
                },
                is_builtin: true,
                args: vec![Latex::Num("1".to_string()), Latex::Num("2".to_string())],
            },
        );
    }

    #[test]
    fn unary_expression() {
        check(
            Expression::UnaryExpr {
                val: Box::new((spn(), Expression::Num("2".to_string()))),
                operator: UnaryOperator::Factorial,
            },
            Latex::UnaryExpression {
                left: Box::new(Latex::Num("2".to_string())),
                operator: LatexUnaryOperator::Factorial,
            },
        );
    }

    #[test]
    fn binexp_typecheck() {
        assert_eq!(
            compile(Expression::BinaryExpr {
                left: Box::new((
                    spn(),
                    Expression::List(vec![(spn(), Expression::Num("1".to_string()))])
                )),
                operator: BinaryOperator::Add,
                right: Box::new((spn(), Expression::Num("2".to_string())))
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
                    Expression::List(vec![(spn(), Expression::Num("1".to_string()))])
                )),
                operator: UnaryOperator::Factorial,
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
            Expression::List(vec![(spn(), Expression::Num("1".to_string()))]),
            Latex::List(vec![Latex::Num("1".to_string())]),
        );
        check(
            Expression::List(vec![
                (spn(), Expression::Num("1".to_string())),
                (spn(), Expression::Num("2".to_string())),
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
                Expression::List(vec![(spn(), Expression::Num("1".to_string()))])
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
            Statement::Expression(Expression::Num("1".to_string())),
            Latex::Num("1".to_string()),
        );
    }

    #[test]
    fn funcdef_single_arg() {
        check_stmt(
            Statement::FuncDef(
                FunctionDefinition {
                    name: "abc".to_string(),
                    args: vec![("def".to_string(), ValType::Number)],
                    ret_annotation: None,
                },
                (spn(), Expression::Num("1".to_string())),
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
                    name: "f".to_string(),
                    args: vec![
                        ("abc".to_string(), ValType::List),
                        ("def".to_string(), ValType::Number),
                    ],
                    ret_annotation: None,
                },
                (spn(), Expression::Num("1".to_string())),
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
                        name: "f".to_string(),
                        args: vec![("a".to_string(), ValType::Number)],
                        ret_annotation: None,
                    },
                    (spn(), Expression::Variable("a".to_string())),
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
            compile_with_ctx(&mut ctx, Expression::Variable("a".to_string())),
            Err(CompileError {
                span: spn(),
                kind: CompileErrorKind::UndefinedVariable("a".to_string())
            })
        )
    }

    #[test]
    fn funcdef_ret_annotation_checked() {
        assert_eq!(
            compile_stmt(Statement::FuncDef(
                FunctionDefinition {
                    name: "f".to_string(),
                    args: vec![("a".to_string(), ValType::Number)],
                    ret_annotation: Some(ValType::List),
                },
                (spn(), Expression::Num("1".to_string())),
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
                    name: "f".to_string(),
                    args: vec![("a".to_string(), ValType::Number)],
                    ret_annotation: None,
                },
                (spn(), Expression::Variable("a".to_string())),
            ),
        )
        .unwrap();
        assert_eq!(
            compile_stmt_with_ctx(
                &mut ctx,
                Statement::Expression(Expression::Variable("a".to_string()))
            )
            .unwrap_err(),
            CompileError {
                kind: CompileErrorKind::UndefinedVariable("a".to_string()),
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
                    name: "f".to_string(),
                    args: vec![("a".to_string(), ValType::Number)],
                    ret_annotation: None,
                },
                (spn(), Expression::Variable("a".to_string())),
            ),
        )
        .unwrap();
        compile_stmt_with_ctx(
            &mut ctx,
            Statement::Expression(Expression::Call {
                modifier: ast::CallModifier::NormalCall,
                func: ast::Function::Normal {
                    name: "f".to_string(),
                },
                args: vec![(spn(), Expression::Num("1".to_string()))],
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
                    name: "f".to_string(),
                    args: vec![],
                    ret_annotation: None,
                },
                (spn(), Expression::Num("1".to_string())),
            ),
        )
        .unwrap();
        assert_eq!(
            compile_stmt_with_ctx(
                &mut ctx,
                Statement::Expression(Expression::Call {
                    modifier: ast::CallModifier::NormalCall,
                    func: ast::Function::Normal {
                        name: "f".to_string()
                    },
                    args: vec![(spn(), Expression::Num("1".to_string()))],
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
                    name: "f".to_string(),
                    args: vec![("a".to_string(), ValType::Number)],
                    ret_annotation: None,
                },
                (spn(), Expression::Num("1".to_string())),
            ),
        )
        .unwrap();
        assert_eq!(
            compile_stmt_with_ctx(
                &mut ctx,
                Statement::Expression(Expression::Call {
                    modifier: ast::CallModifier::NormalCall,
                    func: ast::Function::Normal {
                        name: "f".to_string()
                    },
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
    fn piecewise_single() {
        let mut ctx = new_ctx();
        ctx.variables.insert("a", ValType::Number);
        // input taken from parser test output
        assert_eq!(
            compile_with_ctx(
                &mut ctx,
                Expression::Piecewise {
                    first: Box::new(Branch {
                        cond_left: (spn(), Expression::Variable("a".to_string())),
                        cond: CompareOperator::Equal,
                        cond_right: (spn(), Expression::Num("1".to_string())),
                        val: (spn(), Expression::Num("2".to_string())),
                    }),
                    rest: vec![],
                    default: Box::new((spn(), Expression::Num("3".to_string()))),
                },
            ),
            Ok(Latex::Piecewise {
                first: Box::new(Cond {
                    left: Latex::Variable("a".to_string()),
                    op: CompareOperator::Equal,
                    right: Latex::Num("1".to_string()),
                    result: Latex::Num("2".to_string())
                }),
                rest: vec![],
                default: Box::new(Latex::Num("3".to_string()))
            })
        );
    }

    #[test]
    fn piecewise_multi() {
        let mut ctx = new_ctx();
        ctx.variables.insert("a", ValType::Number);
        // input taken from parser test output
        assert_eq!(
            compile_with_ctx(
                &mut ctx,
                Expression::Piecewise {
                    first: Box::new(Branch {
                        cond_left: (spn(), Expression::Variable("a".to_string())),
                        cond: CompareOperator::GreaterThanEqual,
                        cond_right: (spn(), Expression::Num("1".to_string())),
                        val: (spn(), Expression::Num("2".to_string()))
                    }),
                    rest: vec![
                        Branch {
                            cond_left: (spn(), Expression::Variable("a".to_string())),
                            cond: CompareOperator::LessThanEqual,
                            cond_right: (spn(), Expression::Num("3".to_string())),
                            val: (spn(), Expression::Num("4".to_string()))
                        },
                        Branch {
                            cond_left: (spn(), Expression::Variable("a".to_string())),
                            cond: CompareOperator::LessThan,
                            cond_right: (spn(), Expression::Num("5".to_string())),
                            val: (spn(), Expression::Num("6".to_string()))
                        },
                        Branch {
                            cond_left: (spn(), Expression::Variable("a".to_string())),
                            cond: CompareOperator::GreaterThan,
                            cond_right: (spn(), Expression::Num("7".to_string())),
                            val: (spn(), Expression::Num("8".to_string()))
                        }
                    ],
                    default: Box::new((spn(), Expression::Num("9".to_string())))
                }
            ),
            Ok(Latex::Piecewise {
                first: Box::new(Cond {
                    left: Latex::Variable("a".to_string()),
                    op: CompareOperator::GreaterThanEqual,
                    right: Latex::Num("1".to_string()),
                    result: Latex::Num("2".to_string())
                }),
                rest: vec![
                    Cond {
                        left: Latex::Variable("a".to_string()),
                        op: CompareOperator::LessThanEqual,
                        right: Latex::Num("3".to_string()),
                        result: Latex::Num("4".to_string())
                    },
                    Cond {
                        left: Latex::Variable("a".to_string()),
                        op: CompareOperator::LessThan,
                        right: Latex::Num("5".to_string()),
                        result: Latex::Num("6".to_string())
                    },
                    Cond {
                        left: Latex::Variable("a".to_string()),
                        op: CompareOperator::GreaterThan,
                        right: Latex::Num("7".to_string()),
                        result: Latex::Num("8".to_string())
                    }
                ],
                default: Box::new(Latex::Num("9".to_string()))
            }),
        );
    }
}
