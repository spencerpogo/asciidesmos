use pest::Span as PestSpan;

pub type Span<'a> = PestSpan<'a>;

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
