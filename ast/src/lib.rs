use types::{Span, ValType};

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Mod,
    Exponent,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum UnaryOperator {
    Negate,
    Factorial,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Branch {
    pub cond_left: LocatedExpression,
    pub cond: types::CompareOperator,
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

// Expression is a component of a statement
#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Error,
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
        first: Box<Spanned<Branch>>,
        rest: Vec<Spanned<Branch>>,
        default: Box<LocatedExpression>,
    },
}

pub type Spanned<T> = (Span, T);

pub type LocatedExpression = Spanned<Expression>;

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionDefinition {
    pub name: String,
    pub args: Vec<(String, ValType)>,
    pub ret_annotation: Option<ValType>,
    pub inline: bool,
}

// A statement is a part of a program
#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    VarDef {
        name: String,
        val: LocatedExpression,
        inline: bool,
    },
    FuncDef(FunctionDefinition, LocatedExpression),
    Expression(Expression),
}

pub type LocatedStatement = Spanned<Statement>;
