use super::Span;

/*#[derive(Clone, Debug, PartialEq)]
pub enum Operator {
    Add,
}*/

#[derive(Clone, Debug, PartialEq)]
pub enum AST<'a> {
    Call(Span<'a>, Vec<Span<'a>>),
    Num(Span<'a>), // We don't care about the value of the int, desmos can figure that out
    Add(Box<AST<'a>>, Box<AST<'a>>),
}
