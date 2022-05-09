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

pub type FileID = usize;

#[derive(Clone, Debug, PartialEq)]
pub struct Span {
    pub file_id: FileID,
    pub range: std::ops::Range<usize>,
}

impl Span {
    pub fn new(file_id: FileID, range: std::ops::Range<usize>) -> Self {
        if file_id == 3 { println!("file_id 3 created"); }
        Self { file_id, range }
    }

    pub fn dummy() -> Self {
        Self::new(0, 0..0)
    }
}

#[cfg(feature = "chumsky")]
impl chumsky::Span for Span {
    type Context = FileID;
    type Offset = usize;

    fn new(ctx: Self::Context, range: std::ops::Range<Self::Offset>) -> Self {
        Self {
            file_id: ctx,
            range,
        }
    }

    fn context(self: &Self) -> Self::Context {
        self.file_id
    }

    fn start(self: &Self) -> Self::Offset {
        self.range.start
    }

    fn end(self: &Self) -> Self::Offset {
        self.range.end
    }
}
