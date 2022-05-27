use ast;
use latex;
use std::rc::Rc;
use types;

use super::{
    builtins,
    error::{CompileError, CompileErrorKind},
    types::{Context, FunctionArgs, FunctionSignature, ResolvedFunction},
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
        ast::Function::Log { base: _ } => Some(ResolvedFunction {
            func: Rc::new(FunctionSignature {
                args: FunctionArgs::Static(vec![types::ValType::Number]),
                ret: types::ValType::Number,
            }),
            is_builtin: true,
        }),
        ast::Function::Normal { name } => match ctx.defined_functions.get(&*name) {
            None => match builtins::BUILTIN_FUNCTIONS.get(&*name) {
                None => None,
                Some(f) => Some(ResolvedFunction {
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
            Some(f) => Some(ResolvedFunction {
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
    args: Vec<(types::Span, latex::Latex, types::ValType)>,
    rfunc: ResolvedFunction,
    rargs: &Vec<types::ValType>,
) -> Result<(latex::Latex, types::ValType), CompileError> {
    // Validate arg count
    let got = args.len();
    let expect = rargs.len();

    if got != expect {
        return Err(CompileError {
            kind: CompileErrorKind::WrongArgCount {
                got,
                expected: expect,
            },
            span,
        });
    }
    let args_latex = args
        .into_iter()
        .zip(rargs.iter())
        .map(|(got_type, expect_type)| -> Result<latex::Latex, _> {
            let (aspan, arg_latex, got_type) = got_type;
            let is_valid_map = modifier == ast::CallModifier::MapCall
                && got_type == types::ValType::List
                && *expect_type == types::ValType::Number;
            if !is_valid_map && got_type != *expect_type {
                // TODO this error sucks and logic should be more strict for MapCalls
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
        .collect::<Result<Vec<latex::Latex>, _>>()?;

    Ok((
        latex::Latex::Call {
            func: func_to_latex(func),
            is_builtin: rfunc.is_builtin,
            args: args_latex,
        },
        rfunc.func.ret,
    ))
}

pub fn compile_variadic_call(
    span: types::Span,
    func: ast::Function,
    modifier: ast::CallModifier,
    args: Vec<(types::Span, latex::Latex, types::ValType)>,
    rfunc: ResolvedFunction,
) -> Result<(latex::Latex, types::ValType), CompileError> {
    if modifier == ast::CallModifier::MapCall {
        if args.len() != 1 {
            return Err(CompileError {
                kind: CompileErrorKind::WrongArgCount {
                    got: args.len(),
                    expected: 1,
                },
                span: span,
            });
        }
        let first = args.first().unwrap();
        if first.2 != types::ValType::List {
            return Err(CompileError {
                kind: CompileErrorKind::TypeMismatch {
                    got: first.2,
                    expected: types::ValType::List,
                },
                span: span,
            });
        }
        Ok((
            latex::Latex::Call {
                func: func_to_latex(func),
                is_builtin: rfunc.is_builtin,
                args: vec![first.1.clone()],
            },
            rfunc.func.ret,
        ))
    } else {
        if args.is_empty() {
            return Err(CompileError {
                kind: CompileErrorKind::WrongArgCount {
                    got: 0,
                    expected: 1,
                },
                span: span,
            });
        }
        let args_latex = args
            .into_iter()
            .map(|a| -> Result<latex::Latex, _> {
                if a.2 == types::ValType::Number {
                    Ok(a.1)
                } else {
                    Err(CompileError {
                        kind: CompileErrorKind::TypeMismatch {
                            got: a.2,
                            expected: types::ValType::Number,
                        },
                        span: a.0,
                    })
                }
            })
            .collect::<Result<Vec<latex::Latex>, _>>()?;
        Ok((
            latex::Latex::Call {
                func: func_to_latex(func),
                is_builtin: rfunc.is_builtin,
                args: args_latex,
            },
            rfunc.func.ret,
        ))
    }
}

pub fn compile_call(
    ctx: &mut Context,
    span: types::Span,
    func: ast::Function,
    modifier: ast::CallModifier,
    args: Vec<(types::Span, latex::Latex, types::ValType)>,
) -> Result<(latex::Latex, types::ValType), CompileError> {
    let rfunc = resolve_function(ctx, func.clone()).ok_or(CompileError {
        kind: CompileErrorKind::UnknownFunction(func.clone()),
        span: span.clone(),
    })?;
    match &rfunc.func.args {
        FunctionArgs::Static(rargs) => {
            compile_static_call(span, func, modifier, args, rfunc.clone(), rargs)
        }
        FunctionArgs::Variadic => compile_variadic_call(span, func, modifier, args, rfunc),
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
                    expected: 1
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
                kind: CompileErrorKind::TypeMismatch {
                    got: ValType::List,
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
                expected: 1
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
                expected: 1,
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
            CompileErrorKind::TypeMismatch {
                got: ValType::List,
                expected: ValType::Number
            }
        );
    }
}
