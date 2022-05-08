use pest::{error as pest_err, Span};
use std::fmt;
use types::{ArgCount, ValType};

#[derive(Clone, Debug, PartialEq)]
pub enum CompileErrorKind {
    UnknownFunction(ast::Function),
    WrongArgCount { got: ArgCount, expected: ArgCount },
    TypeMismatch { got: ValType, expected: ValType },
    UndefinedVariable(String),
    UndefinedMacro(String),
    BadMapMacro,
    ExpectedFunction,
    NoNestedList,
}

#[derive(Clone, Debug, PartialEq)]
pub struct CompileError<'a> {
    pub kind: CompileErrorKind,
    pub span: Span<'a>,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash, Copy, PartialOrd, Ord)]
struct DummyRuleType {}

impl CompileError<'_> {
    fn as_msg(&self) -> String {
        match &self.kind {
            CompileErrorKind::UnknownFunction(func) => format!(
                "Unknown function '{}'",
                match func {
                    ast::Function::Normal { name } => name.to_string(),
                    ast::Function::Log { base } => format!("log{}", base),
                }
            ),
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
                "The map! macro takes a function and then at least one list to pass\
                as an argument"
                    .to_string()
            }
            CompileErrorKind::ExpectedFunction => "Expected a function".to_string(),
            CompileErrorKind::NoNestedList => {
                "Storing lists inside of lists is not allowed.".to_string()
            }
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
