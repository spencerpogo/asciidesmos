use super::{
    error::{CompileError, CompileErrorKind},
    types::{Context, FunctionArgs, FunctionSignature, InlineFunction},
};
use ast::{
    BinaryOperator, Expression, LocatedExpression, LocatedStatement, Statement, UnaryOperator,
};
use latex::{
    self, BinaryOperator as LatexBinaryOperator, Cond, Latex, LatexStatement,
    UnaryOperator as LatexUnaryOperator,
};
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

pub fn binop_to_latex(lv: Latex, operator: BinaryOperator, rv: Latex) -> Latex {
    Latex::BinaryExpression {
        operator: match operator {
            BinaryOperator::Add => LatexBinaryOperator::Add,
            BinaryOperator::Subtract => LatexBinaryOperator::Subtract,
            BinaryOperator::Multiply => LatexBinaryOperator::Multiply,
            BinaryOperator::Divide => LatexBinaryOperator::Divide,
            BinaryOperator::Exponent => LatexBinaryOperator::Exponent,
            BinaryOperator::Mod => {
                return Latex::Call {
                    func: latex::Function::Normal {
                        name: "mod".to_string(),
                    },
                    is_builtin: true,
                    args: vec![lv, rv],
                }
            }
        },
        left: Box::new(lv),
        right: Box::new(rv),
    }
}

pub fn unop_to_latex(op: UnaryOperator) -> LatexUnaryOperator {
    match op {
        UnaryOperator::Negate => LatexUnaryOperator::Negate,
        UnaryOperator::Factorial => LatexUnaryOperator::Factorial,
    }
}

pub fn branch_to_cond<'a>(
    ctx: &mut Context,
    (_spn, branch): ast::Spanned<ast::Branch>,
) -> Result<Cond, CompileError> {
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
        Expression::Error => unimplemented!(),
        Expression::Num(val) => Ok((Latex::Num(val.to_string()), ValType::Number)),
        Expression::Variable(name) => match ctx.inline_vals.get(&name) {
            Some((t, v)) => Ok((v.clone(), *t)),
            None => match resolve_variable(ctx, name.clone()) {
                Some(var_type) => Ok((Latex::Variable(name), *var_type)),
                None => Err(CompileError {
                    kind: CompileErrorKind::UndefinedVariable(name),
                    span,
                }),
            },
        },
        Expression::BinaryExpr {
            left,
            operator,
            right,
        } => {
            let span2 = span.clone();
            let lv = compile_expect(ctx, span, *left, ValType::Number)?;
            let rv = compile_expect(ctx, span2, *right, ValType::Number)?;
            Ok((binop_to_latex(lv, operator, rv), ValType::Number))
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

pub fn compile_stmt(
    ctx: &mut Context,
    expr: LocatedStatement,
) -> Result<Option<LatexStatement>, CompileError> {
    let s = expr.0;

    match expr.1 {
        Statement::Expression(e) => Ok(Some(LatexStatement::Expression(
            compile_expr(ctx, (s, e))?.0,
        ))),
        Statement::FuncDef(fdef, e) => {
            // Add args into locals
            for (aspan, aname, atype) in fdef.args.iter() {
                if ctx.variables.contains_key(aname) || ctx.locals.contains_key(aname) {
                    return Err(CompileError {
                        kind: CompileErrorKind::DuplicateVariable(aname.clone()),
                        span: aspan.clone(),
                    });
                }
                ctx.locals.insert(aname.clone(), *atype);
            }
            // Evaluate the body with the new ctx
            let (body, ret) = compile_expr(ctx, e)?;
            // Validate the return type annotation
            if let Some(retann) = fdef.ret_annotation {
                check_type(s, ret, retann)?;
            }
            // remove args from scope
            for (_span, aname, _atyp) in fdef.args.iter() {
                ctx.locals.remove(aname);
            }

            let sig = FunctionSignature {
                args: FunctionArgs::Static(
                    fdef.args
                        .iter()
                        .map(|(_span, _name, typ)| typ.clone())
                        .collect(),
                ),
                ret,
            };
            if fdef.inline {
                ctx.inline_fns.insert(
                    fdef.name.clone(),
                    std::rc::Rc::new(InlineFunction {
                        args: fdef
                            .args
                            .into_iter()
                            .map(|(_span, name, typ)| (name, typ))
                            .collect(),
                        ret,
                        body,
                    }),
                );
                return Ok(None);
            }
            ctx.defined_functions
                .insert(fdef.name.clone(), std::rc::Rc::new(sig));

            Ok(Some(LatexStatement::FuncDef {
                name: fdef.name,
                args: fdef
                    .args
                    .into_iter()
                    .map(|(_span, name, _typ)| name)
                    .collect(),
                body: Box::new(body),
            }))
        }
        Statement::VarDef { name, val, inline } => {
            if ctx.variables.contains_key(name.as_str())
                || ctx.inline_vals.contains_key(name.as_str())
            {
                return Err(CompileError {
                    kind: CompileErrorKind::DuplicateVariable(name),
                    span: s,
                });
            };
            let (val_latex, t) = compile_expr(ctx, val)?;
            match inline {
                true => {
                    ctx.inline_vals.insert(name.clone(), (t, val_latex));
                    Ok(None)
                }
                false => {
                    ctx.variables.insert(name.clone(), t);
                    Ok(Some(LatexStatement::Assignment(
                        Box::new(Latex::Variable(name)),
                        Box::new(val_latex),
                    )))
                }
            }
        }
    }
}

pub fn stmts_to_graph(
    ctx: &mut Context,
    stmts: Vec<ast::Spanned<ast::Statement>>,
) -> Result<graph::CalcState, CompileError> {
    let latex_exprs = stmts
        .into_iter()
        .map(|s| compile_stmt(ctx, s))
        .collect::<Result<Vec<_>, CompileError>>()?;
    Ok(graph::CalcState {
        expressions: graph::Expressions::from_latex_strings(
            latex_exprs
                .into_iter()
                .filter(Option::is_some)
                .map(|l| latex::latex_stmt_to_str(l.unwrap()))
                .collect(),
        ),
        ..Default::default()
    })
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use ast::{self, Branch, FunctionDefinition};
    use types::CompareOperator;

    pub fn new_ctx() -> Context {
        Context::new()
    }

    pub fn compile(exp: Expression) -> Result<Latex, CompileError> {
        compile_with_ctx(&mut new_ctx(), exp)
    }

    pub fn compile_with_ctx(ctx: &mut Context, exp: Expression) -> Result<Latex, CompileError> {
        Ok(compile_expr(ctx, (spn(), exp))?.0)
    }

    pub fn compile_stmt(stmt: Statement) -> Result<Option<LatexStatement>, CompileError> {
        compile_stmt_with_ctx(&mut new_ctx(), stmt)
    }

    pub fn compile_stmt_with_ctx(
        ctx: &mut Context,
        stmt: Statement,
    ) -> Result<Option<LatexStatement>, CompileError> {
        super::compile_stmt(ctx, (spn(), stmt))
    }

    pub fn check_stmt(stmt: Statement, r: LatexStatement) {
        assert_eq!(compile_stmt(stmt).unwrap(), Some(r));
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
        ctx.variables.insert(v.to_string(), vtype);
        compile_with_ctx(&mut ctx, exp)
    }

    pub fn check_with_var<'a>(v: &str, vtype: ValType, exp: Expression, r: Latex) {
        assert_eq!(comp_with_var(v, vtype, exp), Ok(r));
    }

    // generates a fake span to satisfy the type-checker as proper spans aren't
    //  important for most tests
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
            LatexStatement::Expression(Latex::Num("1".to_string())),
        );
    }

    #[test]
    fn funcdef_single_arg() {
        check_stmt(
            Statement::FuncDef(
                FunctionDefinition {
                    name: "abc".to_string(),
                    args: vec![(spn(), "def".to_string(), ValType::Number)],
                    ret_annotation: None,
                    inline: false,
                },
                (spn(), Expression::Num("1".to_string())),
            ),
            LatexStatement::FuncDef {
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
                        (spn(), "abc".to_string(), ValType::List),
                        (spn(), "def".to_string(), ValType::Number),
                    ],
                    ret_annotation: None,
                    inline: false,
                },
                (spn(), Expression::Num("1".to_string())),
            ),
            LatexStatement::FuncDef {
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
                        args: vec![(spn(), "a".to_string(), ValType::Number)],
                        ret_annotation: None,
                        inline: false,
                    },
                    (spn(), Expression::Variable("a".to_string())),
                )
            ),
            Ok(Some(LatexStatement::FuncDef {
                name: "f".to_string(),
                args: vec!["a".to_string()],
                body: Box::new(Latex::Variable("a".to_string())),
            }),)
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
                    args: vec![(spn(), "a".to_string(), ValType::Number)],
                    ret_annotation: Some(ValType::List),
                    inline: false,
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
                    args: vec![(spn(), "a".to_string(), ValType::Number)],
                    ret_annotation: None,
                    inline: false,
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
                    args: vec![(spn(), "a".to_string(), ValType::Number)],
                    ret_annotation: None,
                    inline: false,
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
                    inline: false,
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
                    args: vec![(spn(), "a".to_string(), ValType::Number)],
                    ret_annotation: None,
                    inline: false,
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
    fn funcdef_catch_shadow() {
        let mut ctx = new_ctx();
        ctx.variables.insert("a".to_string(), ValType::Number);
        assert_eq!(
            compile_stmt_with_ctx(
                &mut ctx,
                Statement::FuncDef(
                    FunctionDefinition {
                        args: vec![(spn(), "a".to_string(), ValType::Number)],
                        name: "f".to_string(),
                        ret_annotation: None,
                        inline: false,
                    },
                    (spn(), Expression::Num("1".to_string())),
                )
            ),
            Err(CompileError {
                span: spn(),
                kind: CompileErrorKind::DuplicateVariable("a".to_string()),
            }),
        );
    }

    #[test]
    fn piecewise_single() {
        let mut ctx = new_ctx();
        ctx.variables.insert("a".to_string(), ValType::Number);
        // input taken from parser test output
        assert_eq!(
            compile_with_ctx(
                &mut ctx,
                Expression::Piecewise {
                    first: Box::new((
                        spn(),
                        Branch {
                            cond_left: (spn(), Expression::Variable("a".to_string())),
                            cond: CompareOperator::Equal,
                            cond_right: (spn(), Expression::Num("1".to_string())),
                            val: (spn(), Expression::Num("2".to_string())),
                        }
                    )),
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
        ctx.variables.insert("a".to_string(), ValType::Number);
        // input taken from parser test output
        assert_eq!(
            compile_with_ctx(
                &mut ctx,
                Expression::Piecewise {
                    first: Box::new((
                        spn(),
                        Branch {
                            cond_left: (spn(), Expression::Variable("a".to_string())),
                            cond: CompareOperator::GreaterThanEqual,
                            cond_right: (spn(), Expression::Num("1".to_string())),
                            val: (spn(), Expression::Num("2".to_string()))
                        }
                    )),
                    rest: vec![
                        (
                            spn(),
                            Branch {
                                cond_left: (spn(), Expression::Variable("a".to_string())),
                                cond: CompareOperator::LessThanEqual,
                                cond_right: (spn(), Expression::Num("3".to_string())),
                                val: (spn(), Expression::Num("4".to_string()))
                            }
                        ),
                        (
                            spn(),
                            Branch {
                                cond_left: (spn(), Expression::Variable("a".to_string())),
                                cond: CompareOperator::LessThan,
                                cond_right: (spn(), Expression::Num("5".to_string())),
                                val: (spn(), Expression::Num("6".to_string()))
                            }
                        ),
                        (
                            spn(),
                            Branch {
                                cond_left: (spn(), Expression::Variable("a".to_string())),
                                cond: CompareOperator::GreaterThan,
                                cond_right: (spn(), Expression::Num("7".to_string())),
                                val: (spn(), Expression::Num("8".to_string()))
                            }
                        )
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

    #[test]
    fn var_assign() {
        let mut ctx = new_ctx();
        assert_eq!(
            compile_with_ctx(&mut ctx, ast::Expression::Variable("test".to_string())),
            Err(CompileError {
                kind: CompileErrorKind::UndefinedVariable("test".to_string()),
                span: spn(),
            })
        );
        assert_eq!(
            compile_stmt_with_ctx(
                &mut ctx,
                ast::Statement::VarDef {
                    name: "test".to_string(),
                    val: (spn(), ast::Expression::Num("1".to_string())),
                    inline: false,
                }
            ),
            Ok(Some(LatexStatement::Assignment(
                Box::new(Latex::Variable("test".to_string())),
                Box::new(Latex::Num("1".to_string()))
            )))
        );
        assert_eq!(
            compile_with_ctx(&mut ctx, ast::Expression::Variable("test".to_string())),
            Ok(Latex::Variable("test".to_string()))
        );
    }
}
