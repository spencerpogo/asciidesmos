#[derive(Clone, Debug, PartialEq)]
pub enum AST<'a> {
    Call(&'a str, Vec<&'a str>),
    Int(i64),
}
