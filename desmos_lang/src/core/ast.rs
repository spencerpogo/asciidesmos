use super::runtime::ValType;
use pest::Span;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Mod,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum UnaryOperator {
    Factorial,
}

// Expression is a component of a statement
#[derive(Clone, Debug, PartialEq)]
pub enum Expression<'a> {
    Num(&'a str),
    Variable(&'a str),
    BinaryExpr {
        left: Box<LocatedExpression<'a>>,
        // Should probably make an enum for this, but its not worth the work to encode
        //  it just to stringify it again later
        operator: BinaryOperator,
        right: Box<LocatedExpression<'a>>,
    },
    UnaryExpr {
        val: Box<LocatedExpression<'a>>,
        operator: UnaryOperator,
    },
    Call {
        func: &'a str,
        args: Vec<LocatedExpression<'a>>,
    },
    MacroCall {
        name: &'a str,
        args: Vec<LocatedExpression<'a>>,
    },
    List(Vec<LocatedExpression<'a>>),
}

pub type LocatedExpression<'a> = (Span<'a>, Expression<'a>);

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionDefinition<'a> {
    pub name: &'a str,
    pub args: Vec<(&'a str, ValType)>,
    pub ret_annotation: Option<ValType>,
}

// A statement is a part of a program
#[derive(Clone, Debug, PartialEq)]
pub enum Statement<'a> {
    FuncDef(FunctionDefinition<'a>, LocatedExpression<'a>),
    Expression(Expression<'a>),
}

pub type LocatedStatement<'a> = (Span<'a>, Statement<'a>);
