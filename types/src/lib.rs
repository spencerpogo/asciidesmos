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
        Self { file_id, range }
    }

    pub fn dummy() -> Self {
        Self::new(0, 0..0)
    }

    pub fn with_end_of(&self, other: &Self) -> Option<Self> {
        if self.file_id == other.file_id {
            Some(Self::new(self.file_id, self.range.start..other.range.end))
        } else {
            None
        }
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
