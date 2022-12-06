use std::fmt;
use types::{ArgCount, ValType};

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
        got: ValType,
        expected: ValType,
    },
    NegateList,
    FactorialList,
    RangeExpectNumber,
    IndexNonList,
    MapNonList,
    IndexWithNonNumber,
    RetAnnMismatch {
        got: ValType,
        expected: ValType,
    },
    ExpectedSameTypes {
        left: ValType,
        right: ValType,
    },
    UndefinedVariable(String),
    DuplicateVariable(String),
    ExpectedFunction,
    NoNestedList,
    NoInlineVariadic,
    UnresolvedNamespace(Vec<String>),
    ModuleNotFound(String),
}

#[derive(Clone, Debug, PartialEq)]
pub struct CompileError {
    pub kind: CompileErrorKind,
    pub span: types::Span,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash, Copy, PartialOrd, Ord)]
struct DummyRuleType {}

impl CompileErrorKind {
    pub fn as_msg(&self) -> String {
        match &self {
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
            CompileErrorKind::IndexNonList => {
                format!("Cannot index non-list")
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
        }
    }

    pub fn help(&self) -> Option<String> {
        match &self {
            _ => None,
        }
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
