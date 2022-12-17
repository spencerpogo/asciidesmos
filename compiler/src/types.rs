use ast::LStatements;
use std::{collections::HashMap, fmt::Debug, rc::Rc};
use types::ValType;

use crate::{error::CompileError, stdlib::StdlibLoader};

// ValType that supports list mapping
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Typ {
    Num,
    List,
    MappedList,
}

impl From<ValType> for Typ {
    fn from(v: ValType) -> Self {
        match v {
            ValType::Number => Self::Num,
            ValType::List => Self::List,
        }
    }
}

impl Typ {
    pub fn is_num_weak(self) -> bool {
        match self {
            Self::Num => true,
            Self::List => false,
            Self::MappedList => true,
        }
    }

    pub fn is_list_weak(self) -> bool {
        match self {
            Self::Num => false,
            Self::List => true,
            Self::MappedList => true,
        }
    }

    pub fn is_num_strict(self) -> bool {
        self == Self::Num
    }

    pub fn eq_weak(self, rhs: Self) -> bool {
        match self {
            Self::Num => rhs.is_num_weak(),
            Self::List => rhs == Self::List,
            // todo: reject redundant cmp of mappedlist to mappedlist?
            Self::MappedList => rhs.is_num_weak(),
        }
    }
}

pub fn combine_types(
    left: (types::Span, Typ, TypInfo),
    right: (types::Span, Typ, TypInfo),
) -> (types::Span, Typ, TypInfo) {
    let (ls, lt, li) = left;
    let (rs, rt, ri) = right;
    if lt.is_list_weak() {
        return (ls, Typ::List, li);
    }
    if rt.is_list_weak() {
        return (rs, Typ::List, ri);
    }
    // only possibilities left:
    debug_assert_eq!(lt, Typ::Num);
    debug_assert_eq!(rt, Typ::Num);
    (
        ls.with_end_of(&rs).expect("Parsing same file"),
        lt,
        TypInfo::BinOp(ls, rs),
    )
}

pub fn reduce_types<I>(types: I) -> Option<(Typ, TypInfo)>
where
    I: IntoIterator<Item = (types::Span, Typ, TypInfo)>,
{
    types
        .into_iter()
        .reduce(|left, right| combine_types(left, right))
        .map(|(_s, t, i)| (t, i))
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Literal {
    Numeric,
    List,
    Range,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypInfo {
    Literal(Literal, types::Span),
    BinOp(types::Span, types::Span),
    Map(types::Span),
    Builtin(ast::Function),
    RawLatex(types::Span),
    FuncArg(types::Span),
}

// heap version of core::runtime::Args
#[derive(Clone, Debug, PartialEq)]
pub enum FunctionArgs {
    Static(Vec<ValType>),
    Variadic,
}

// heap version of core::runtime::Function
#[derive(Clone, Debug, PartialEq)]
pub struct FunctionSignature {
    pub args: FunctionArgs,
    pub ret: (Typ, TypInfo),
}

#[derive(Clone, Debug, PartialEq)]
pub struct InlineFunction {
    pub args: Vec<(String, ValType)>,
    pub ret: (Typ, TypInfo),
    pub body: latex::Latex,
}

pub trait Loader: LoaderClone + Debug {
    fn load(&self, path: &str) -> Option<LStatements>;

    fn parse_source(&self, source: &str) -> Option<LStatements>;
}

// https://stackoverflow.com/a/30353928/9196137
pub trait LoaderClone {
    fn box_clone(&self) -> Box<dyn Loader>;
}

impl<T> LoaderClone for T
where
    T: 'static + Loader + Clone,
{
    fn box_clone(&self) -> Box<dyn Loader> {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn 'static + Loader> {
    fn clone(&self) -> Self {
        self.box_clone()
    }
}

#[derive(Clone, Debug)]
pub struct UnimplementedLoader;

impl Loader for UnimplementedLoader {
    fn load(&self, _path: &str) -> Option<LStatements> {
        unimplemented!()
    }

    fn parse_source(&self, _source: &str) -> Option<LStatements> {
        unimplemented!()
    }
}

#[derive(Clone, Debug)]
pub struct Context {
    pub variables: HashMap<String, (ValType, TypInfo)>,
    pub locals: HashMap<String, (ValType, TypInfo)>,
    pub defined_functions: HashMap<String, Rc<FunctionSignature>>,
    pub inline_vals: HashMap<String, (Typ, latex::Latex, TypInfo)>,
    pub inline_fns: HashMap<String, Rc<InlineFunction>>,
    // can't support submodules (yet)
    pub modules: HashMap<String, Context>,
    pub stdlib: StdlibLoader,
    pub loader: Box<dyn Loader>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            ..Default::default()
        }
    }

    pub fn new_with_loader(loader: Box<dyn Loader>) -> Self {
        Self {
            loader,
            ..Default::default()
        }
    }
}

impl Default for Context {
    fn default() -> Self {
        let loader = Box::new(UnimplementedLoader);
        Self {
            variables: HashMap::new(),
            locals: HashMap::new(),
            defined_functions: HashMap::new(),
            inline_vals: HashMap::new(),
            inline_fns: HashMap::new(),
            modules: HashMap::new(),
            stdlib: StdlibLoader::new(),
            loader,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ResolvedFunction {
    Normal {
        func: Rc<FunctionSignature>,
        is_builtin: bool,
    },
    Inline(Rc<InlineFunction>),
}

pub type Cesult<T> = Result<T, CompileError>;
