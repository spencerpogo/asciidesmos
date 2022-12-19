use std::fmt;
use types::{ArgCount, ValType};

use crate::types::{Typ, TypInfo};

#[derive(Clone, Debug, PartialEq)]
pub enum ExpectedArgCount {
    NonZero,
    Exact(ArgCount),
}

#[derive(Clone, Debug, PartialEq)]
pub enum CompileErrorKind {
    UnknownFunction(ast::Function),
    WrongArgCount {
        got: ArgCount,
        expected: ExpectedArgCount,
    },
    ArgTypeMismatch {
        got: (Typ, TypInfo),
        expected: ValType,
    },
    NegateList,
    FactorialList,
    RangeExpectNumber,
    IndexNonList(Typ, TypInfo),
    MapNonList,
    IndexWithNonNumber,
    RetAnnMismatch {
        got: (Typ, TypInfo),
        expected: ValType,
    },
    ExpectedSameTypes {
        left: (Typ, TypInfo),
        right: (Typ, TypInfo),
    },
    VariadicList,
    UndefinedVariable(String),
    DuplicateVariable(String),
    ExpectedFunction,
    NoNestedList,
    NoInlineVariadic,
    UnresolvedNamespace(Vec<String>),
    ModuleNotFound(String),
    MapAsVariable,
    ReturnMap,
}

#[derive(Clone, Debug, PartialEq)]
pub struct CompileError {
    pub kind: CompileErrorKind,
    pub span: types::Span,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash, Copy, PartialOrd, Ord)]
struct DummyRuleType {}

fn typinfo_labels(ti: TypInfo) -> Vec<(types::Span, String)> {
    match ti {
        TypInfo::Literal(l, s) => vec![(s, format!("{:#?} literal here", l))],
        TypInfo::BinOp(l, r) => vec![(l, format!("Left")), (r, "Right".to_owned())], // FIXME: This can be much better
        TypInfo::Map(s) => vec![(s, "Mapped list here".to_string())],
        TypInfo::Builtin(s, _) => vec![(s, "Call to builtin function".to_string())],
        TypInfo::RawLatex(s) => vec![(s, "Raw latex".to_string())],
        TypInfo::InlineFuncArg(s) => vec![(s, "Inline function argument".to_string().to_string())],
        TypInfo::Call { call_span, ret: _ } => vec![(call_span, "Call here".to_string())], // FIXME: Expose ret better
        TypInfo::MappedCall {
            call_span,
            mapped_arg: _,
        } => vec![(call_span, "In this call".to_string())],
    }
}

impl CompileErrorKind {
    pub fn as_msg(&self) -> String {
        match self {
            CompileErrorKind::UnknownFunction(func) => format!(
                "Unknown function '{}'",
                // TODO: move this formatting into the AST crate
                match func {
                    ast::Function::Normal { name } => name.to_string(),
                    ast::Function::Log { base } => format!("log{}", base),
                }
            ),
            CompileErrorKind::WrongArgCount { got, expected } => {
                let ex_fmt: Box<dyn std::fmt::Display> = match expected {
                    ExpectedArgCount::NonZero => Box::new("1 or more"),
                    ExpectedArgCount::Exact(n) => Box::new(n),
                };
                format!("Expected {} arguments but got {}", ex_fmt, got)
            }
            CompileErrorKind::ArgTypeMismatch { got, expected } => {
                format!("Expected type {:#?} but got {:#?}", expected, got)
            }
            CompileErrorKind::NegateList => {
                // TODO: there will be syntax to map list
                format!("Cannot negate a list")
            }
            CompileErrorKind::FactorialList => {
                format!("Cannot take the factorial of a list")
            }
            CompileErrorKind::RangeExpectNumber => {
                format!("Range argument must be numbers")
            }
            CompileErrorKind::IndexNonList(t, _) => {
                format!("Cannot index non-list type {}", t)
            }
            CompileErrorKind::MapNonList => {
                format!("Cannot map non-list")
            }
            CompileErrorKind::IndexWithNonNumber => {
                format!("Index must be a number")
            }
            CompileErrorKind::RetAnnMismatch { got, expected } => {
                format!(
                    "Expected type {:#?} due to return type annotation, but function returned {:#?}", 
                    expected,
                    got
                )
            }
            CompileErrorKind::ExpectedSameTypes { left, right } => {
                format!(
                    "Expected left type {:#?} to match right type {:#?}",
                    left, right
                )
            }
            CompileErrorKind::VariadicList => {
                "Variadic functions expect all numerical arguments".to_string()
            }
            CompileErrorKind::UndefinedVariable(var) => {
                format!("Undefined variable '{}'", var)
            }
            CompileErrorKind::DuplicateVariable(name) => {
                // todo: span of prev definition
                format!("Variable '{}' is already defined", name)
            }
            CompileErrorKind::ExpectedFunction => "Expected a function".to_string(),
            CompileErrorKind::NoNestedList => {
                "Storing lists inside of lists is not allowed.".to_string()
            }
            CompileErrorKind::NoInlineVariadic => {
                "Inline functions cannot have variadic arguments".to_string()
            }
            CompileErrorKind::UnresolvedNamespace(path) => {
                format!("Cannot resolve namespace '{}'", ast::fmt_namespace(&path))
            }
            CompileErrorKind::ModuleNotFound(path) => {
                format!("Module not found: '{}'", path)
            }
            CompileErrorKind::MapAsVariable => {
                format!("Cannot assign a mapped list to a variable: delete this map")
            }
            CompileErrorKind::ReturnMap => {
                format!("Cannot return a mapped list from a function: delete this map")
            }
        }
    }

    pub fn help(&self) -> Vec<String> {
        match &self {
            _ => vec![],
        }
    }

    pub fn typinfos(self) -> Vec<TypInfo> {
        match self {
            CompileErrorKind::UnknownFunction(_) => vec![],
            CompileErrorKind::WrongArgCount {
                got: _,
                expected: _,
            } => vec![],
            CompileErrorKind::ArgTypeMismatch {
                got: (_, ti),
                expected: _,
            } => vec![ti],
            CompileErrorKind::NegateList => vec![],
            CompileErrorKind::FactorialList => vec![],
            CompileErrorKind::RangeExpectNumber => vec![],
            CompileErrorKind::IndexNonList(_, ti) => vec![ti],
            CompileErrorKind::MapNonList => vec![],
            CompileErrorKind::IndexWithNonNumber => vec![],
            CompileErrorKind::RetAnnMismatch {
                got: (_, ti),
                expected: _,
            } => vec![ti],
            CompileErrorKind::ExpectedSameTypes {
                left: (_, lti),
                right: (_, rti),
            } => vec![lti, rti],
            CompileErrorKind::VariadicList => vec![],
            CompileErrorKind::UndefinedVariable(_) => vec![],
            CompileErrorKind::DuplicateVariable(_) => vec![],
            CompileErrorKind::ExpectedFunction => vec![],
            CompileErrorKind::NoNestedList => vec![],
            CompileErrorKind::NoInlineVariadic => vec![],
            CompileErrorKind::UnresolvedNamespace(_) => vec![],
            CompileErrorKind::ModuleNotFound(_) => vec![],
            CompileErrorKind::MapAsVariable => vec![],
            CompileErrorKind::ReturnMap => vec![],
        }
    }

    pub fn labels(self) -> Vec<(types::Span, String)> {
        self.typinfos()
            .into_iter()
            .flat_map(|ti| typinfo_labels(ti))
            .collect()
    }
}

impl fmt::Display for CompileErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_msg())
    }
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:#?} {}", self.span, self.kind)
    }
}
