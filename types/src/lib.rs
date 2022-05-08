pub type ArgCount = usize;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ValType {
    Number,
    List,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Args<'a> {
    Static(&'a [ValType]),
    Variadic,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function<'a> {
    pub args: Args<'a>,
    pub ret: ValType,
}

pub type Span = std::ops::Range<usize>;
