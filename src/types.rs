use pest::error as pest_err;
use pest::Span as PestSpan;
use std::fmt;

#[derive(Debug, Clone)]
pub struct AssertionError;

// Generation of an error is completely separate from how it is displayed.
// There's no need to be concerned about cluttering complex logic with the display style.
//
// Note that we don't store any extra info about the errors. This means we can't state
// which string failed to parse without modifying our types to carry that information.
impl fmt::Display for AssertionError {
    #[cfg(not(tarpaulin_include))]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "assertion error while processing tokens")
    }
}

// Expression is a component of a statement
#[derive(Clone, Debug, PartialEq)]
pub enum Expression<'a> {
    Num {
        val: &'a str,
    },
    Variable {
        val: &'a str,
    },
    BinaryExpr {
        left: Box<LocatedExpression<'a>>,
        // Should probably make an enum for this, but its not worth the work to encode
        //  it just to stringify it again later
        operator: &'a str,
        right: Box<LocatedExpression<'a>>,
    },
    UnaryExpr {
        val: Box<LocatedExpression<'a>>,
        operator: &'a str,
    },
    Call {
        func: &'a str,
        args: Vec<Box<LocatedExpression<'a>>>,
    },
    List(Vec<Box<LocatedExpression<'a>>>),
}

pub type Span<'a> = PestSpan<'a>;

pub type LocatedExpression<'a> = (Span<'a>, Expression<'a>);

// A statement is a part of a program
#[derive(Clone, Debug, PartialEq)]
pub enum Statement<'a> {
    FuncDef(FunctionDefinition<'a>, LocatedExpression<'a>),
    Expression(Expression<'a>),
}

pub type LocatedStatement<'a> = (Span<'a>, Statement<'a>);

pub type ArgCount = usize;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ValType {
    Number,
    List,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function<'a> {
    pub args: &'a [ValType],
    pub ret: ValType,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionDefinition<'a> {
    pub name: &'a str,
    pub args: Vec<(&'a str, Option<ValType>)>,
    pub ret_annotation: Option<ValType>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum CompileErrorKind<'a> {
    UnknownFunction(&'a str),
    WrongArgCount { got: ArgCount, expected: ArgCount },
    TypeMismatch { got: ValType, expected: ValType },
    UndefinedVariable(&'a str),
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
