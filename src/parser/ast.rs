#[derive(Clone, Debug, PartialEq)]
pub enum Operation {
    Mul,
    Div,
    Add,
    Sub,
}

#[derive(Clone, Debug, PartialEq)]
pub enum AST<'a> {
    Ident(&'a str),
    Call(&'a str, Vec<Box<AST<'a>>>),
    Num(&'a str), // We don't care about the value of the int, desmos can figure that out
    BinOp(Box<AST<'a>>, Vec<(Operation, Box<AST<'a>>)>),
}
