use crate::types::{combine_types, reduce_types, Cesult, Literal, Typ, TypInfo};

use super::{
    error::{CompileError, CompileErrorKind},
    types::{Context, FunctionArgs, FunctionSignature, InlineFunction},
};
use ast::{
    BinaryOperator, Expression, LocatedExpression, LocatedStatement, Statement, UnaryOperator,
};
use latex::{
    BinaryOperator as LatexBinaryOperator, Cond, Latex, LatexStatement,
    UnaryOperator as LatexUnaryOperator,
};
use types::ValType;

pub fn resolve_variable(ctx: &Context, var: String) -> Option<(ValType, TypInfo)> {
    if let Some(r) = ctx.variables.get::<str>(var.as_ref()) {
        return Some(r.clone());
    }
    if let Some(r) = ctx.locals.get::<str>(var.as_ref()) {
        return Some(r.clone());
    }
    None
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

pub fn comp_expect<F>(
    ctx: &mut Context,
    expr: LocatedExpression,
    check: F,
    kind: CompileErrorKind,
) -> Cesult<(Latex, Typ, TypInfo)>
where
    F: Fn(Typ) -> bool,
{
    let span = expr.0.clone();
    let (v, t, ti) = compile_expr(ctx, expr)?;
    if !check(t) {
        return Err(CompileError { kind, span });
    }
    Ok((v, t, ti))
}

pub fn comp_expect_num_strict(
    ctx: &mut Context,
    expr: LocatedExpression,
    kind: CompileErrorKind,
) -> Cesult<(Latex, Typ, TypInfo)> {
    comp_expect(ctx, expr, |t| t == Typ::Num, kind)
}

pub fn comp_expect_num(
    ctx: &mut Context,
    expr: LocatedExpression,
    kind: CompileErrorKind,
) -> Cesult<(Latex, Typ, TypInfo)> {
    comp_expect(ctx, expr, |t| t.is_num_weak(), kind)
}

pub fn comp_expect_list_strict(
    ctx: &mut Context,
    expr: LocatedExpression,
    kind: CompileErrorKind,
) -> Cesult<(Latex, Typ, TypInfo)> {
    comp_expect(ctx, expr, |t| t == Typ::List, kind)
}

pub fn comp_binop(
    ctx: &mut Context,
    left: LocatedExpression,
    right: LocatedExpression,
) -> Cesult<(Latex, Latex, Typ, TypInfo)> {
    let ls = left.0.clone();
    let rs = left.0.clone();
    let (lv, lt, li) = compile_expr(ctx, left)?;
    let (rv, rt, ri) = compile_expr(ctx, right)?;
    if !lt.eq_weak(rt) {
        return Err(CompileError {
            kind: CompileErrorKind::ExpectedSameTypes {
                left: (lt, li),
                right: (rt, ri),
            },
            span: ls.with_end_of(&rs).unwrap_or(ls),
        });
    }
    let (_s, t, i) = combine_types((ls, lt, li), (rs, rt, ri));
    Ok((lv, rv, t, i))
}

pub fn branch_to_cond(
    ctx: &mut Context,
    (spn, branch): ast::Spanned<ast::Branch>,
) -> Cesult<(Cond, (types::Span, Typ, TypInfo))> {
    let (left, right, t, i) = comp_binop(ctx, branch.cond_left, branch.cond_right)?;
    Ok((
        Cond {
            left: left,
            op: branch.cond,
            right: right,
            result: compile_expr(ctx, branch.val)?.0,
        },
        (spn, t, i),
    ))
}

pub fn compile_variable_ref(
    ctx: &Context,
    span: types::Span,
    name: String,
) -> Cesult<(Latex, Typ, TypInfo)> {
    match ctx.inline_vals.get(&name) {
        Some((t, v, i)) => Ok((v.clone(), *t, i.clone())),
        None => match resolve_variable(ctx, name.clone()) {
            Some((var_type, info)) => Ok((Latex::Variable(name), var_type.into(), info)),
            None => Err(CompileError {
                kind: CompileErrorKind::UndefinedVariable(name),
                span,
            }),
        },
    }
}

// Ideally this would be functional and ctx would not need to be mutable, but rust
//  support for immutable hashmaps isn't built in and mutation is much simpler.
pub fn compile_expr(ctx: &mut Context, expr: LocatedExpression) -> Cesult<(Latex, Typ, TypInfo)> {
    let span = expr.0;

    match expr.1 {
        Expression::Error => unimplemented!(),
        Expression::Num(val) => Ok((
            Latex::Num(val.to_string()),
            Typ::Num,
            TypInfo::Literal(Literal::Numeric, span),
        )),
        Expression::Variable(name) => compile_variable_ref(ctx, span, name),
        Expression::FullyQualifiedVariable { path, item } => {
            if path.len() != 1 {
                return Err(CompileError {
                    kind: CompileErrorKind::UnresolvedNamespace(path),
                    span,
                });
            }
            match ctx.modules.get(path.first().unwrap()) {
                Some(module) => compile_variable_ref(module, span, item),
                None => Err(CompileError {
                    kind: CompileErrorKind::UnresolvedNamespace(path),
                    span,
                }),
            }
        }
        Expression::BinaryExpr {
            left,
            operator,
            right,
        } => {
            let (left, right, t, i) = comp_binop(ctx, *left, *right)?;
            Ok((binop_to_latex(left, operator, right), t, i))
        }
        Expression::UnaryExpr {
            val: v,
            operator: op,
        } => {
            let (l, t, i) = compile_expr(ctx, *v)?;
            if !t.is_num_weak() {
                return Err(CompileError {
                    kind: match op {
                        UnaryOperator::Negate => CompileErrorKind::NegateList,
                        UnaryOperator::Factorial => CompileErrorKind::FactorialList,
                    },
                    span,
                });
            }
            Ok((
                Latex::UnaryExpression {
                    left: Box::new(l),
                    operator: match op {
                        UnaryOperator::Negate => LatexUnaryOperator::Negate,
                        UnaryOperator::Factorial => LatexUnaryOperator::Factorial,
                    },
                },
                t,
                i,
            ))
        }
        Expression::Map(val) => {
            let (v, t, _) = compile_expr(ctx, *val)?;
            if t != Typ::List {
                return Err(CompileError {
                    kind: CompileErrorKind::MapNonList,
                    span,
                });
            }
            Ok((v, Typ::MappedList, TypInfo::Map(span)))
        }
        Expression::Call { func, args } => {
            let compiled_args = args
                .into_iter()
                .map(|(s, e)| -> Cesult<(types::Span, Latex, Typ, TypInfo)> {
                    let (latex, t, i) = compile_expr(ctx, (s.clone(), e))?;
                    Ok((s, latex, t, i))
                })
                .collect::<Cesult<Vec<_>>>()?;
            super::call::compile_call(ctx, span, func, compiled_args)
        }
        Expression::List(values) => {
            let items = values
                .into_iter()
                .map(|e| -> Cesult<Latex> {
                    Ok(comp_expect_num_strict(ctx, e, CompileErrorKind::NoNestedList)?.0)
                })
                .collect::<Cesult<Vec<Latex>>>()?;

            Ok((
                Latex::List(items),
                Typ::List,
                TypInfo::Literal(Literal::List, span),
            ))
        }
        Expression::Range { first, second, end } => {
            let range = Latex::Range {
                first: Box::new(
                    comp_expect_num_strict(ctx, *first, CompileErrorKind::RangeExpectNumber)?.0,
                ),
                second: match second {
                    Some(second) => Some(Box::new(
                        comp_expect_num_strict(ctx, *second, CompileErrorKind::RangeExpectNumber)?
                            .0,
                    )),
                    None => None,
                },
                end: Box::new(
                    comp_expect_num_strict(ctx, *end, CompileErrorKind::RangeExpectNumber)?.0,
                ),
            };
            Ok((range, Typ::List, TypInfo::Literal(Literal::Range, span)))
        }
        Expression::Piecewise {
            first,
            rest,
            default,
        } => {
            let (first, ft) = branch_to_cond(ctx, *first)?;
            let (rest, rest_types): (Vec<_>, Vec<_>) = rest
                .into_iter()
                .map(|b| branch_to_cond(ctx, b))
                .collect::<Result<Vec<_>, _>>()?
                .into_iter()
                .unzip();
            let dspan = default.0.clone();
            let (default, dt, di) = compile_expr(ctx, *default)?;
            let (t, ti) = reduce_types(
                std::iter::once(ft)
                    .chain(rest_types)
                    .chain(std::iter::once((dspan, dt, di))),
            )
            .unwrap();
            Ok((
                Latex::Piecewise {
                    first: Box::new(first),
                    rest,
                    default: Box::new(default),
                },
                t,
                ti,
            ))
        }
        Expression::RawLatex(ty, l) => Ok((Latex::Raw(l), ty.into(), TypInfo::RawLatex(span))),
        Expression::Index { val, ind } => {
            let (r, rt, ri) = comp_expect_num(ctx, *ind, CompileErrorKind::IndexWithNonNumber)?;
            Ok((
                Latex::BinaryExpression {
                    left: Box::new(
                        comp_expect_list_strict(ctx, *val, CompileErrorKind::IndexNonList)?.0,
                    ),
                    operator: LatexBinaryOperator::Index,
                    right: Box::new(r),
                },
                rt,
                ri,
            ))
        }
    }
}

pub type CompileResult = Cesult<Vec<LatexStatement>>;

pub fn compile_stmt(ctx: &mut Context, expr: LocatedStatement) -> CompileResult {
    let s = expr.0;

    match expr.1 {
        Statement::Expression(e) => Ok(vec![LatexStatement::Expression(
            compile_expr(ctx, (s, e))?.0,
        )]),
        Statement::FuncDef(fdef, e) => {
            unimplemented!();
            /*// Add args into locals
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
                if ret != retann {
                    return Err(CompileError {
                        kind: CompileErrorKind::RetAnnMismatch {
                            got: ret,
                            expected: retann,
                        },
                        span: s,
                    });
                };
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
                return Ok(vec![]);
            }
            ctx.defined_functions
                .insert(fdef.name.clone(), std::rc::Rc::new(sig));

            Ok(vec![LatexStatement::FuncDef {
                name: fdef.name,
                args: fdef
                    .args
                    .into_iter()
                    .map(|(_span, name, _typ)| name)
                    .collect(),
                body: Box::new(body),
            }])*/
        }
        Statement::VarDef { name, val, inline } => {
            unimplemented!();
            /*if ctx.variables.contains_key(name.as_str())
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
                    Ok(vec![])
                }
                false => {
                    ctx.variables.insert(name.clone(), t);
                    Ok(vec![LatexStatement::Assignment(
                        Box::new(Latex::Variable(name)),
                        Box::new(val_latex),
                    )])
                }
            }*/
        }
        Statement::Import(import) => super::import::handle_import(ctx, s, import),
    }
}

pub fn compile_stmts(
    ctx: &mut Context,
    ast: Vec<ast::Spanned<ast::Statement>>,
) -> Cesult<Vec<LatexStatement>> {
    Ok(ast
        .into_iter()
        .map(|s| compile_stmt(ctx, s))
        .collect::<Result<Vec<_>, _>>()?
        .into_iter()
        .flatten()
        .collect())
}

pub fn stmts_to_graph(
    ctx: &mut Context,
    stmts: Vec<ast::Spanned<ast::Statement>>,
) -> Cesult<graph::CalcState> {
    let latex_exprs = stmts
        .into_iter()
        .map(|s| compile_stmt(ctx, s))
        .collect::<Cesult<Vec<_>>>()?;
    Ok(graph::CalcState {
        expressions: graph::Expressions::from_latex_strings(
            latex_exprs
                .into_iter()
                .flatten()
                .map(|l| latex::latex_stmt_to_str(l))
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

    pub fn compile(exp: Expression) -> Cesult<Latex> {
        compile_with_ctx(&mut new_ctx(), exp)
    }

    pub fn compile_with_ctx(ctx: &mut Context, exp: Expression) -> Cesult<Latex> {
        Ok(compile_expr(ctx, (spn(), exp))?.0)
    }

    pub fn compile_stmt(stmt: Statement) -> Cesult<Vec<LatexStatement>> {
        compile_stmt_with_ctx(&mut new_ctx(), stmt)
    }

    pub fn compile_stmt_with_ctx(
        ctx: &mut Context,
        stmt: Statement,
    ) -> Cesult<Vec<LatexStatement>> {
        super::compile_stmt(ctx, (spn(), stmt))
    }

    pub fn check_stmt(stmt: Statement, r: LatexStatement) {
        assert_eq!(compile_stmt(stmt).unwrap(), vec![r]);
    }

    pub fn check(exp: Expression, r: Latex) {
        assert_eq!(compile(exp).unwrap(), r);
    }

    pub fn tinfo() -> TypInfo {
        TypInfo::Literal(Literal::Numeric, spn())
    }

    pub fn comp_with_var(v: &str, vtype: ValType, exp: Expression) -> Cesult<Latex> {
        let mut ctx = new_ctx();
        ctx.variables.insert(v.to_string(), (vtype, tinfo()));
        compile_with_ctx(&mut ctx, exp)
    }

    pub fn check_with_var(v: &str, vtype: ValType, exp: Expression, r: Latex) {
        assert_eq!(comp_with_var(v, vtype, exp), Ok(r));
    }

    // generates a fake span to satisfy the type-checker
    // accurate spans are relevant to testing the parser but not for the compiler
    #[inline]
    pub fn spn() -> types::Span {
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
            CompileErrorKind::ExpectedSameTypes {
                left: (Typ::List, todo!()),
                right: (Typ::Num, todo!())
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
            CompileErrorKind::FactorialList
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
            Ok(vec![LatexStatement::FuncDef {
                name: "f".to_string(),
                args: vec!["a".to_string()],
                body: Box::new(Latex::Variable("a".to_string())),
            }])
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
                kind: CompileErrorKind::RetAnnMismatch {
                    got: Typ::Num,
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
                    expected: crate::error::ExpectedArgCount::Exact(0),
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
                    func: ast::Function::Normal {
                        name: "f".to_string()
                    },
                    args: vec![(spn(), Expression::List(vec![]))],
                }),
            )
            .unwrap_err(),
            CompileError {
                span: spn(),
                kind: CompileErrorKind::ArgTypeMismatch {
                    expected: ValType::Number,
                    got: (Typ::List, todo!())
                }
            }
        );
    }

    #[test]
    fn funcdef_catch_shadow() {
        let mut ctx = new_ctx();
        ctx.variables
            .insert("a".to_string(), (ValType::Number, tinfo()));
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
        ctx.variables
            .insert("a".to_string(), (ValType::Number, tinfo()));
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
        ctx.variables
            .insert("a".to_string(), (ValType::Number, tinfo()));
        let firstbranch = Branch {
            cond_left: (spn(), Expression::Variable("a".to_string())),
            cond: CompareOperator::GreaterThanEqual,
            cond_right: (spn(), Expression::Num("1".to_string())),
            val: (spn(), Expression::Num("2".to_string())),
        };
        let ast = Expression::Piecewise {
            first: Box::new((spn(), firstbranch.clone())),
            rest: vec![
                (
                    spn(),
                    Branch {
                        cond_left: (spn(), Expression::Variable("a".to_string())),
                        cond: CompareOperator::LessThanEqual,
                        cond_right: (spn(), Expression::Num("3".to_string())),
                        val: (spn(), Expression::Num("4".to_string())),
                    },
                ),
                (
                    spn(),
                    Branch {
                        cond_left: (spn(), Expression::Variable("a".to_string())),
                        cond: CompareOperator::LessThan,
                        cond_right: (spn(), Expression::Num("5".to_string())),
                        val: (spn(), Expression::Num("6".to_string())),
                    },
                ),
                (
                    spn(),
                    Branch {
                        cond_left: (spn(), Expression::Variable("a".to_string())),
                        cond: CompareOperator::GreaterThan,
                        cond_right: (spn(), Expression::Num("7".to_string())),
                        val: (spn(), Expression::Num("8".to_string())),
                    },
                ),
            ],
            default: Box::new((spn(), Expression::Num("9".to_string()))),
        };
        // input taken from parser test output
        assert_eq!(
            compile_with_ctx(&mut ctx, ast.clone()),
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
            Ok(vec![LatexStatement::Assignment(
                Box::new(Latex::Variable("test".to_string())),
                Box::new(Latex::Num("1".to_string()))
            )])
        );
        assert_eq!(
            compile_with_ctx(&mut ctx, ast::Expression::Variable("test".to_string())),
            Ok(Latex::Variable("test".to_string()))
        );
    }

    #[test]
    fn module_reference() {
        let mut submodule = new_ctx();
        submodule
            .variables
            .insert("a".to_string(), (ValType::Number, tinfo()));
        submodule.inline_vals.insert(
            "b".to_string(),
            (Typ::Num, Latex::Num("1".to_string()), tinfo()),
        );
        let mut ctx = new_ctx();
        ctx.modules.insert("lib".to_owned(), submodule);
        let comp_var = |v| {
            compile_with_ctx(
                &mut ctx.clone(),
                ast::Expression::FullyQualifiedVariable {
                    path: vec!["lib".to_owned()],
                    item: v,
                },
            )
        };
        assert_eq!(
            comp_var("a".to_owned()),
            Ok(Latex::Variable("a".to_owned()))
        );
        assert_eq!(comp_var("b".to_owned()), Ok(Latex::Num("1".to_owned())));
    }
}
