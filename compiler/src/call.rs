use ast;
use latex::{self, Latex};
use std::{collections::HashMap, rc::Rc};
use types::ValType;

use crate::{
    builtins,
    error::{CompileError, CompileErrorKind, ExpectedArgCount},
    types::{
        combine_types, Context, FunctionArgs, FunctionSignature, ResolvedFunction, Typ, TypInfo,
    },
};

pub fn func_to_latex(func: ast::Function) -> latex::Function {
    match func {
        ast::Function::Normal { name } => latex::Function::Normal { name },
        ast::Function::Log { base } => latex::Function::Log { base },
    }
}

// Returns function and whether it is builtin
pub fn resolve_function<'a>(
    ctx: &'a mut Context,
    span: types::Span,
    func: ast::Function,
) -> Option<ResolvedFunction> {
    let name = match func {
        ast::Function::Log { base: _ } => {
            return Some(ResolvedFunction::Normal {
                func: Rc::new(FunctionSignature {
                    args: FunctionArgs::Static(vec![types::ValType::Number]),
                    ret: (ValType::Number, TypInfo::Builtin(span, func)),
                }),
                is_builtin: true,
            });
        }
        ast::Function::Normal { name } => name,
    };
    if let Some(f) = ctx.inline_fns.get::<str>(name.as_ref()) {
        return Some(ResolvedFunction::Inline(f.clone()));
    }
    if let Some(f) = ctx.defined_functions.get::<str>(name.as_ref()) {
        return Some(ResolvedFunction::Normal {
            func: f.clone(),
            is_builtin: false,
        });
    }
    if let Some(f) = builtins::BUILTIN_FUNCTIONS.get::<str>(name.as_ref()) {
        return Some(ResolvedFunction::Normal {
            func: Rc::new(FunctionSignature {
                args: match f.args {
                    types::Args::Static(args) => FunctionArgs::Static(args.to_vec()),
                    types::Args::Variadic => FunctionArgs::Variadic,
                },
                ret: (
                    f.ret.into(),
                    TypInfo::Builtin(span, ast::Function::Normal { name }),
                ),
            }),
            is_builtin: true,
        });
    }
    None
}

fn check_arg_types(
    args: Vec<(types::Span, latex::Latex, Typ, TypInfo)>,
    rargs: &Vec<types::ValType>,
    ret: ValType,
) -> Result<
    (
        Vec<latex::Latex>,
        Vec<(types::Span, Typ, TypInfo)>,
        Option<TypInfo>,
    ),
    CompileError,
> {
    let mut mapped_arg = None;
    let (args_latex, args_types) = args
        .into_iter()
        .zip(rargs.iter())
        .map(
            |(got_type, expect_type)| -> Result<(latex::Latex, (types::Span, Typ, TypInfo)), _> {
                let (aspan, arg_latex, gt, gi) = got_type;
                let et = (*expect_type).into();
                if !gt.eq_weak(et) {
                    return Err(CompileError {
                        kind: CompileErrorKind::ArgTypeMismatch {
                            got: (gt, gi),
                            expected: *expect_type,
                        },
                        span: aspan,
                    });
                }

                // Keep track of the first argument to cause the function to implicitly map
                if mapped_arg.is_none() && gt != et {
                    mapped_arg = Some(gi.clone());
                }
                Ok((arg_latex, (aspan, gt, gi)))
            },
        )
        .collect::<Result<Vec<_>, _>>()?
        .into_iter()
        .unzip();

    // Map does not occur if function returns a list anyway
    let mapped_arg = if ret == ValType::List {
        None
    } else {
        mapped_arg
    };
    Ok((args_latex, args_types, mapped_arg))
}

pub fn ret_type<I>(args_types: I, ret: ValType) -> Typ
where
    I: IntoIterator<Item = Typ>,
{
    let ret: Typ = ret.into();
    args_types
        .into_iter()
        .chain(std::iter::once(ret))
        .reduce(|l, r| combine_types(l, r))
        .unwrap_or(ret)
}

pub fn compile_static_call(
    span: types::Span,
    func: ast::Function,
    args: Vec<(types::Span, latex::Latex, Typ, TypInfo)>,
    rfunc: FunctionSignature,
    rargs: &Vec<types::ValType>,
    is_builtin: bool,
) -> Result<(latex::Latex, Typ, TypInfo), CompileError> {
    // Validate arg count
    {
        let got = args.len();
        let expect = rargs.len();

        if got != expect {
            return Err(CompileError {
                kind: CompileErrorKind::WrongArgCount {
                    got,
                    expected: ExpectedArgCount::Exact(expect),
                },
                span,
            });
        }
    }

    let (args_latex, args_types, mapped_arg) = check_arg_types(args, rargs, rfunc.ret.0)?;
    let rt = ret_type(args_types.into_iter().map(|i| i.1), rfunc.ret.0);
    let ri = match mapped_arg {
        Some(mapped_arg) => TypInfo::MappedCall {
            call_span: span,
            func: func.clone(),
            mapped_arg: Box::new(mapped_arg),
        },
        None => TypInfo::Call {
            call_span: span,
            ret: Box::new(rfunc.ret.1),
        },
    };

    Ok((
        latex::Latex::Call {
            func: func_to_latex(func),
            is_builtin,
            args: args_latex,
        },
        rt,
        ri,
    ))
}

fn variadic_call_types(
    args: Vec<(types::Span, latex::Latex, Typ, TypInfo)>,
    rfunc: FunctionSignature,
) -> Result<(Vec<Latex>, Typ, TypInfo), CompileError> {
    if args.len() == 1 {
        let first = args.first().unwrap();
        if first.2.is_list_weak() {
            let (rt, ri) = rfunc.ret;
            return Ok((vec![first.1.clone()], rt.into(), ri));
        }
    }
    todo!();
    /*let (args_latex, args_types) = args
        .into_iter()
        .map(
            |(span, latex, t, ti)| -> Result<(Latex, (types::Span, Typ, TypInfo)), CompileError> {
                if !t.is_num_weak() {
                    return Err(CompileError {
                        span,
                        kind: CompileErrorKind::VariadicList,
                    });
                }
                Ok((latex, (span, t, ti)))
            },
        )
        .collect::<Result<Vec<_>, CompileError>>()?
        .into_iter()
        .unzip();
    let (rt, ri) = resolve_func_type(args_types, rfunc.ret);
    Ok((args_latex, rt, ri))*/
}

pub fn compile_variadic_call(
    span: types::Span,
    func: ast::Function,
    args: Vec<(types::Span, latex::Latex, Typ, TypInfo)>,
    rfunc: FunctionSignature,
    is_builtin: bool,
) -> Result<(latex::Latex, Typ, TypInfo), CompileError> {
    if args.is_empty() {
        return Err(CompileError {
            kind: CompileErrorKind::WrongArgCount {
                got: 0,
                expected: ExpectedArgCount::NonZero,
            },
            span: span,
        });
    }
    // if variadic functions are passed a single list, they will return a single value
    // if passed multiple lists, normal automatic mapping applies
    let (args_latex, ret, ri) = variadic_call_types(args, rfunc)?;
    Ok((
        latex::Latex::Call {
            func: func_to_latex(func),
            is_builtin,
            args: args_latex,
        },
        ret,
        ri,
    ))
}

pub fn map_variables<F>(node: latex::Latex, replacer: &F) -> Latex
where
    F: Fn(String) -> Latex,
{
    let proc = |v| map_variables(v, replacer);
    let proc_vec = |v: Vec<Latex>| {
        v.into_iter()
            .map(|l| map_variables(l, replacer))
            .collect::<Vec<_>>()
    };
    let proc_cond = |c: latex::Cond| latex::Cond {
        left: proc(c.left),
        op: c.op,
        right: proc(c.right),
        result: proc(c.result),
    };
    match node {
        Latex::Variable(name) => replacer(name),
        Latex::Num(n) => Latex::Num(n),
        Latex::Call {
            func,
            is_builtin,
            args,
        } => Latex::Call {
            func,
            is_builtin,
            args: proc_vec(args),
        },
        Latex::BinaryExpression {
            left,
            operator,
            right,
        } => Latex::BinaryExpression {
            left: Box::new(proc(*left)),
            operator,
            right: Box::new(proc(*right)),
        },
        Latex::UnaryExpression { left, operator } => Latex::UnaryExpression {
            left: Box::new(proc(*left)),
            operator,
        },
        Latex::List(inner) => Latex::List(proc_vec(inner)),
        Latex::Range { first, second, end } => Latex::Range {
            first: Box::new(proc(*first)),
            second: second.map(|v| Box::new(proc(*v))),
            end: Box::new(proc(*end)),
        },
        Latex::Piecewise {
            first,
            rest,
            default,
        } => Latex::Piecewise {
            first: Box::new(proc_cond(*first)),
            rest: rest.into_iter().map(proc_cond).collect::<Vec<_>>(),
            default: Box::new(proc(*default)),
        },
        Latex::Raw(l) => Latex::Raw(l),
    }
}

pub fn replace_variables(node: Latex, vars: &HashMap<String, Latex>) -> Latex {
    map_variables(node, &|name| match vars.get(&name) {
        Some(replacement) => replacement.clone(),
        None => Latex::Variable(name),
    })
}

pub fn compile_call(
    ctx: &mut Context,
    span: types::Span,
    func: ast::Function,
    args: Vec<(types::Span, latex::Latex, Typ, TypInfo)>,
) -> Result<(latex::Latex, Typ, TypInfo), CompileError> {
    let rfunc = resolve_function(ctx, span.clone(), func.clone()).ok_or(CompileError {
        kind: CompileErrorKind::UnknownFunction(func.clone()),
        span: span.clone(),
    })?;
    match rfunc {
        ResolvedFunction::Inline(f_rc) => {
            let rfunc = (*f_rc).clone();
            {
                let got = args.len();
                let expected = rfunc.args.len();
                if got != expected {
                    return Err(CompileError {
                        kind: CompileErrorKind::WrongArgCount {
                            got,
                            expected: ExpectedArgCount::Exact(expected),
                        },
                        span,
                    });
                }
            }

            let (entries, args_types): (Vec<_>, Vec<_>) =
                args.into_iter()
                    .zip(rfunc.args.into_iter())
                    .map(
                        |((arg_span, arg_lat, got_typ, got_info), (name, typ))| -> Result<
                            ((String, Latex), (types::Span, Typ, TypInfo)),
                            CompileError,
                        > {
                            if !got_typ.eq_weak(typ.into()) {
                                return Err(CompileError {
                                    kind: CompileErrorKind::ArgTypeMismatch {
                                        got: (got_typ, got_info),
                                        expected: typ,
                                    },
                                    span: arg_span,
                                });
                            }
                            Ok(((name, arg_lat), (arg_span, got_typ, got_info)))
                        },
                    )
                    .collect::<Result<Vec<_>, CompileError>>()?
                    .into_iter()
                    .unzip();

            let vars: HashMap<String, Latex> = entries.into_iter().collect();
            todo!();
            //let (rt, ri) = resolve_func_type(args_types, rfunc.ret);
            //Ok((replace_variables(rfunc.body, &vars), rt, ri))
        }
        ResolvedFunction::Normal {
            func: rfunc,
            is_builtin,
        } => match &rfunc.args {
            FunctionArgs::Static(rargs) => {
                compile_static_call(span, func, args, (*rfunc).clone(), rargs, is_builtin)
            }
            FunctionArgs::Variadic => {
                compile_variadic_call(span, func, args, (*rfunc).clone(), is_builtin)
            }
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        compiler::tests::{check, compile, spn},
        types::Literal,
    };
    use ast::Expression;
    use builtins::BUILTIN_FUNCTIONS;
    use latex::Latex;
    use types::ValType;

    #[test]
    fn log() {
        check(
            Expression::Call {
                func: ast::Function::Log {
                    base: "".to_string(),
                },
                args: vec![(spn(), Expression::Num("10".to_string()))],
            },
            Latex::Call {
                func: latex::Function::Log {
                    base: "".to_string(),
                },
                args: vec![Latex::Num("10".to_string())],
                is_builtin: true,
            },
        );
    }

    #[test]
    fn log_base() {
        check(
            Expression::Call {
                func: ast::Function::Log {
                    base: "5".to_string(),
                },
                args: vec![(spn(), Expression::Num("25".to_string()))],
            },
            Latex::Call {
                func: latex::Function::Log {
                    base: "5".to_string(),
                },
                args: vec![Latex::Num("25".to_string())],
                is_builtin: true,
            },
        );
    }

    #[test]
    fn call_variadic() {
        assert_eq!(
            BUILTIN_FUNCTIONS.get("lcm").unwrap().args,
            types::Args::Variadic
        );
        assert_eq!(
            compile(Expression::Call {
                func: ast::Function::Normal {
                    name: "lcm".to_string()
                },
                args: vec![],
            }),
            Err(CompileError {
                kind: CompileErrorKind::WrongArgCount {
                    got: 0,
                    expected: ExpectedArgCount::NonZero
                },
                span: spn()
            })
        );
        check(
            Expression::Call {
                func: ast::Function::Normal {
                    name: "lcm".to_string(),
                },
                args: vec![
                    (spn(), Expression::Num("1".to_string())),
                    (spn(), Expression::Num("2".to_string())),
                    (spn(), Expression::Num("3".to_string())),
                ],
            },
            Latex::Call {
                func: latex::Function::Normal {
                    name: "lcm".to_string(),
                },
                args: vec![
                    Latex::Num("1".to_string()),
                    Latex::Num("2".to_string()),
                    Latex::Num("3".to_string()),
                ],
                is_builtin: true,
            },
        );
    }

    #[test]
    fn map_variadic() {
        assert_eq!(
            BUILTIN_FUNCTIONS.get("lcm").unwrap().args,
            types::Args::Variadic
        );
        let inp = Expression::Call {
            func: ast::Function::Normal {
                name: "lcm".to_string(),
            },
            args: vec![(
                spn(),
                Expression::List(vec![
                    (spn(), Expression::Num("1".to_string())),
                    (spn(), Expression::Num("2".to_string())),
                    (spn(), Expression::Num("3".to_string())),
                ]),
            )],
        };
        assert_eq!(
            compile(inp.clone()),
            Err(CompileError {
                kind: CompileErrorKind::ArgTypeMismatch {
                    got: (Typ::List, TypInfo::Literal(Literal::List, spn())),
                    expected: ValType::Number
                },
                span: spn()
            })
        );
        check(
            Expression::Call {
                func: ast::Function::Normal {
                    name: "lcm".to_string(),
                },
                args: vec![(
                    spn(),
                    Expression::List(vec![
                        (spn(), Expression::Num("1".to_string())),
                        (spn(), Expression::Num("2".to_string())),
                        (spn(), Expression::Num("3".to_string())),
                    ]),
                )],
            },
            Latex::Call {
                func: latex::Function::Normal {
                    name: "lcm".to_string(),
                },
                args: vec![Latex::List(vec![
                    Latex::Num("1".to_string()),
                    Latex::Num("2".to_string()),
                    Latex::Num("3".to_string()),
                ])],
                is_builtin: true,
            },
        );
    }

    #[test]
    fn call_resolution() {
        check(
            Expression::Call {
                func: ast::Function::Normal {
                    name: "sin".to_string(),
                },
                args: vec![(spn(), Expression::Num("1".to_string()))],
            },
            Latex::Call {
                func: latex::Function::Normal {
                    name: "sin".to_string(),
                },
                is_builtin: true,
                args: vec![Latex::Num("1".to_string())],
            },
        );
        assert_eq!(
            compile(Expression::Call {
                func: ast::Function::Normal {
                    name: "abc".to_string()
                },
                args: vec![],
            })
            .unwrap_err()
            .kind,
            CompileErrorKind::UnknownFunction(ast::Function::Normal {
                name: "abc".to_string()
            })
        );
    }

    #[test]
    fn argc_validation() {
        assert_eq!(
            compile(Expression::Call {
                func: ast::Function::Normal {
                    name: "sin".to_string()
                },
                args: vec![],
            })
            .unwrap_err()
            .kind,
            CompileErrorKind::WrongArgCount {
                got: 0,
                expected: ExpectedArgCount::Exact(1)
            }
        );
        assert_eq!(
            compile(Expression::Call {
                func: ast::Function::Normal {
                    name: "sin".to_string()
                },
                args: vec![
                    (spn(), Expression::Num("1".to_string())),
                    (spn(), Expression::Num("2".to_string()))
                ]
            })
            .unwrap_err()
            .kind,
            CompileErrorKind::WrongArgCount {
                got: 2,
                expected: ExpectedArgCount::Exact(1),
            }
        );
    }

    #[test]
    fn call_arg_checking() {
        assert_eq!(
            compile(Expression::Call {
                func: ast::Function::Normal {
                    name: "sin".to_string()
                },
                args: vec![(
                    spn(),
                    Expression::List(vec![(spn(), Expression::Num("1".to_string()))])
                )]
            })
            .unwrap_err()
            .kind,
            CompileErrorKind::ArgTypeMismatch {
                got: (Typ::List, TypInfo::Literal(Literal::List, spn())),
                expected: ValType::Number
            }
        );
    }

    #[test]
    fn mapcall_type() {
        check(
            Expression::Index {
                val: Box::new((
                    spn(),
                    Expression::Call {
                        func: ast::Function::Normal {
                            name: "sin".to_string(),
                        },
                        args: vec![(
                            spn(),
                            Expression::List(vec![(spn(), Expression::Num("1".to_string()))]),
                        )],
                    },
                )),
                ind: Box::new((spn(), Expression::Num("1".to_string()))),
            },
            Latex::BinaryExpression {
                left: Box::new(Latex::Call {
                    func: latex::Function::Normal {
                        name: "sin".to_string(),
                    },
                    is_builtin: true,
                    args: vec![Latex::List(vec![Latex::Num("1".to_string())])],
                }),
                operator: latex::BinaryOperator::Index,
                right: Box::new(Latex::Num("1".to_string())),
            },
        );
    }
}
