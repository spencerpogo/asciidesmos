use pest::Span as PestSpan;
use std::fmt;

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
