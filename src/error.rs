use crate::types::{ArgCount, Span, ValType};
use pest::error as pest_err;
use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub enum CompileErrorKind<'a> {
    UnknownFunction(&'a str),
    WrongArgCount { got: ArgCount, expected: ArgCount },
    TypeMismatch { got: ValType, expected: ValType },
    UndefinedVariable(&'a str),
    UndefinedMacro(&'a str),
    BadMapMacro,
    ExpectedFunction,
}

#[derive(Clone, Debug, PartialEq)]
pub struct CompileError<'a> {
    pub kind: CompileErrorKind<'a>,
    pub span: Span<'a>,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash, Copy, PartialOrd, Ord)]
struct DummyRuleType {}

impl CompileError<'_> {
    fn as_msg(&self) -> String {
        match self.kind {
            CompileErrorKind::UnknownFunction(func) => format!("Unknown function '{}'", func),
            CompileErrorKind::WrongArgCount { got, expected } => {
                format!("Expected {} arguments but got {}", expected, got)
            }
            CompileErrorKind::TypeMismatch { got, expected } => {
                format!("Expected type {:#?} but got {:#?}", expected, got)
            }
            CompileErrorKind::UndefinedVariable(var) => {
                format!("Undefined variable '{}'", var)
            }
            CompileErrorKind::UndefinedMacro(name) => format!("Undefined macro '{}'", name),
            CompileErrorKind::BadMapMacro => {
                "Usage: map!(function, list, [list2], ..., [listn])".to_string()
            }
            CompileErrorKind::ExpectedFunction => "Expected a function".to_string(),
        }
    }
}

impl fmt::Display for CompileError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s: pest_err::Error<DummyRuleType> = pest_err::Error::new_from_span(
            pest_err::ErrorVariant::CustomError {
                message: self.as_msg(),
            },
            self.span.clone(),
        );
        write!(f, "{}", s)
    }
}
