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

#[derive(Clone, Debug, PartialEq)]
pub struct Span {
    // path to file
    pub source: String,
    pub range: std::ops::Range<usize>,
}

#[cfg(feature = "chumsky")]
impl chumsky::Span for Span {
    type Context = String;
    type Offset = usize;

    fn new(ctx: Self::Context, range: std::ops::Range<Self::Offset>) -> Self {
        Self { source: ctx, range }
    }

    fn context(self: &Self) -> Self::Context {
        // this might have performance impacts idk
        self.source.clone()
    }
    fn start(self: &Self) -> Self::Offset {
        self.range.start
    }
    fn end(self: &Self) -> Self::Offset {
        self.range.end
    }
}
