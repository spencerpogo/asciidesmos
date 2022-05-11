use latex;
use types::{Span, ValType};

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
    Negate,
    Factorial,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Branch {
    pub cond_left: LocatedExpression,
    pub cond: latex::CompareOperator,
    pub cond_right: LocatedExpression,
    pub val: LocatedExpression,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum CallModifier {
    MapCall,
    NormalCall,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Function {
    Normal { name: String },
    Log { base: String },
}

impl Function {
    pub fn to_latex(self) -> latex::Function {
        match self {
            Function::Normal { name } => latex::Function::Normal {
                name: name.to_string(),
            },
            Function::Log { base } => latex::Function::Log {
                base: base.to_string(),
            },
        }
    }
}

// Expression is a component of a statement
#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Num(String),
    Variable(String),
    BinaryExpr {
        left: Box<LocatedExpression>,
        // Should probably make an enum for this, but its not worth the work to encode
        //  it just to stringify it again later
        operator: BinaryOperator,
        right: Box<LocatedExpression>,
    },
    UnaryExpr {
        val: Box<LocatedExpression>,
        operator: UnaryOperator,
    },
    Call {
        modifier: CallModifier,
        func: Function,
        args: Vec<LocatedExpression>,
    },
    List(Vec<LocatedExpression>),
    Piecewise {
        first: Box<Branch>,
        rest: Vec<Branch>,
        default: Box<LocatedExpression>,
    },
    MapExpression(Box<LocatedExpression>),
}

pub type LocatedExpression = (Span, Expression);

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionDefinition {
    pub name: String,
    pub args: Vec<(String, ValType)>,
    pub ret_annotation: Option<ValType>,
}

// A statement is a part of a program
#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    FuncDef(FunctionDefinition, LocatedExpression),
    Expression(Expression),
}

pub type LocatedStatement = (Span, Statement);
