use ast;
use latex::{self, Latex};
use std::{collections::HashMap, rc::Rc};
use types::{self, ValType};

use crate::{
    builtins,
    error::{CompileError, CompileErrorKind, ExpectedArgCount},
    types::{Context, FunctionArgs, FunctionSignature, ResolvedFunction, Typ},
};

pub fn func_to_latex(func: ast::Function) -> latex::Function {
    match func {
        ast::Function::Normal { name } => latex::Function::Normal { name },
        ast::Function::Log { base } => latex::Function::Log { base },
    }
}

// Returns function and whether it is builtin
pub fn resolve_function<'a>(ctx: &'a mut Context, func: ast::Function) -> Option<ResolvedFunction> {
    match func {
        ast::Function::Log { base: _ } => Some(ResolvedFunction::Normal {
            func: Rc::new(FunctionSignature {
                args: FunctionArgs::Static(vec![types::ValType::Number]),
                ret: types::ValType::Number,
            }),
            is_builtin: true,
        }),
        ast::Function::Normal { name } => match ctx.defined_functions.get(&*name) {
            None => match ctx.inline_fns.get(&*name) {
                Some(f) => Some(ResolvedFunction::Inline(f.clone())),
                None => match builtins::BUILTIN_FUNCTIONS.get(&*name) {
                    None => None,
                    Some(f) => Some(ResolvedFunction::Normal {
                        func: Rc::new(FunctionSignature {
                            args: match f.args {
                                types::Args::Static(args) => FunctionArgs::Static(args.to_vec()),
                                types::Args::Variadic => FunctionArgs::Variadic,
                            },
                            ret: f.ret,
                        }),
                        is_builtin: true,
                    }),
                },
            },
            Some(f) => Some(ResolvedFunction::Normal {
                func: f.clone(),
                is_builtin: false,
            }),
        },
    }
}

pub fn compile_static_call(
    span: types::Span,
    func: ast::Function,
    modifier: ast::CallModifier,
    args: Vec<(types::Span, latex::Latex, Typ)>,
    rfunc: FunctionSignature,
    rargs: &Vec<types::ValType>,
    is_builtin: bool,
) -> Result<(latex::Latex, Typ), CompileError> {
    // Validate arg count
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
    let args_results = args
        .into_iter()
        .zip(rargs.iter())
        .map(
            |(got_type, expect_type)| -> Result<(latex::Latex, Typ), _> {
                let (aspan, arg_latex, got_type) = got_type;
                if got_type.eq_weak((*expect_type).into()) {
                    return Err(CompileError {
                        kind: CompileErrorKind::ArgTypeMismatch {
                            got: got_type,
                            expected: *expect_type,
                        },
                        span: aspan,
                    });
                }
                Ok((arg_latex, got_type))
            },
        )
        .collect::<Result<Vec<(latex::Latex, Typ)>, _>>()?;

    let (args_latex, args_types): (Vec<_>, Vec<_>) = args_results.into_iter().unzip();
    let ret_typ = args_types.into_iter().reduce(|l, r| l.binop_result(r));

    Ok((
        latex::Latex::Call {
            func: func_to_latex(func),
            is_builtin,
            args: args_latex,
        },
        match ret_typ {
            None => rfunc.ret.into(), // there were no args
            Some(ret_typ) => ret_typ.binop_result(ret_typ.into()),
        },
    ))
}

pub fn compile_variadic_call(
    span: types::Span,
    func: ast::Function,
    modifier: ast::CallModifier,
    args: Vec<(types::Span, latex::Latex, Typ)>,
    rfunc: FunctionSignature,
    is_builtin: bool,
) -> Result<(latex::Latex, types::ValType), CompileError> {
    if args.is_empty() {
        return Err(CompileError {
            kind: CompileErrorKind::WrongArgCount {
                got: 0,
                expected: ExpectedArgCount::NonZero,
            },
            span: span,
        });
    }
    unimplemented!();
    /*// if variadic functions are passed a single list, they will return a single value
    // if passed multiple lists, normal automatic mapping applies
    let (args_latex, ret) = if args.len() == 1 && first.2.is_list_weak() {
        (first.1, Typ::Num)
    } else {
        let args_results = args
            .into_iter()
            .try_reduce(|l, r| -> Result<Typ, CompileError> {
                if !l.2.eq_weak(r.2) {
                    return Err(CompileError {
                        kind: CompileErrorKind::ExpectedSameTypes {
                            left: l.2,
                            right: r.2,
                        },
                        span: r.0,
                    });
                }
                Ok(l.2.binop_result(r.2))
            })?
            .unwrap_or(rfunc.ret);
    };
    Ok((
        latex::Latex::Call {
            func: func_to_latex(func),
            is_builtin,
            args: args_latex,
        },
        ret,
    ))*/
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

//pub fn replace_variables
//vars: &HashMap<String, latex::Latex>
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
    modifier: ast::CallModifier,
    args: Vec<(types::Span, latex::Latex, Typ)>,
) -> Result<(latex::Latex, Typ), CompileError> {
    let rfunc = resolve_function(ctx, func.clone()).ok_or(CompileError {
        kind: CompileErrorKind::UnknownFunction(func.clone()),
        span: span.clone(),
    })?;
    match rfunc {
        ResolvedFunction::Inline(f_rc) => {
            let rfunc = (*f_rc).clone();
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

            unimplemented!();
            /*let mut vars = HashMap::new();
            for ((name, typ), (arg_span, arg_lat, got_typ)) in
                rfunc.args.into_iter().zip(args.into_iter())
            {
                if typ.into() != got_typ {
                    return Err(CompileError {
                        kind: CompileErrorKind::ArgTypeMismatch {
                            got: got_typ,
                            expected: typ,
                        },
                        span: arg_span,
                    });
                }
                vars.insert(name, arg_lat);
            }*/

            //Ok((replace_variables(rfunc.body, &vars), rfunc.ret))
        }
        ResolvedFunction::Normal {
            func: rfunc,
            is_builtin,
        } => match &rfunc.args {
            FunctionArgs::Static(rargs) => compile_static_call(
                span,
                func,
                modifier,
                args,
                (*rfunc).clone(),
                rargs,
                is_builtin,
            ),
            FunctionArgs::Variadic => {
                unimplemented!()
                //compile_variadic_call(span, func, modifier, args, (*rfunc).clone(), is_builtin)
            }
        },
    }
}

#[cfg(test)]
mod tests {
    use super::super::compiler::tests::{check, compile, spn};
    use super::*;
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
                modifier: ast::CallModifier::NormalCall,
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
                modifier: ast::CallModifier::NormalCall,
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
                modifier: ast::CallModifier::NormalCall,
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
                modifier: ast::CallModifier::NormalCall,
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
            modifier: ast::CallModifier::NormalCall,
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
                    got: Typ::List,
                    expected: ValType::Number
                },
                span: spn()
            })
        );
        check(
            Expression::Call {
                modifier: ast::CallModifier::MapCall,
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
                modifier: ast::CallModifier::NormalCall,
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
                modifier: ast::CallModifier::NormalCall,
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
                modifier: ast::CallModifier::NormalCall,
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
                modifier: ast::CallModifier::NormalCall,
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
                modifier: ast::CallModifier::NormalCall,
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
                got: Typ::List,
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
                        modifier: ast::CallModifier::MapCall,
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
