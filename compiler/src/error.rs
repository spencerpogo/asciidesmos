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
    TypeMismatch {
        got: ValType,
        expected: ValType,
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
            CompileErrorKind::TypeMismatch { got, expected } => {
                format!("Expected type {:#?} but got {:#?}", expected, got)
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
            CompileErrorKind::TypeMismatch { got, expected } => {
                if *got == ValType::List && *expected == ValType::Number {
                    Some("To map a function over a list use @, like `sin@([1])".to_string())
                } else {
                    None
                }
            }
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
