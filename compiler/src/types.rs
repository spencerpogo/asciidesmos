use ast::LStatements;
use std::{collections::HashMap, fmt::Debug, rc::Rc};
use types::ValType;

use crate::stdlib::StdlibLoader;

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

    // Given two types that are eq_weak, simulate the result of a desmos binop
    pub fn binop_result(self, rhs: Self) -> Self {
        if self.is_list_weak() || rhs.is_list_weak() {
            return Self::List;
        }
        // both sides are strictly Self::Num
        Self::Num
    }

    pub fn unop_result(self) -> Self {
        match self {
            Self::Num => Self::Num,
            Self::List => Self::List,
            Self::MappedList => Self::List,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypInfo {
    Expression(types::Span),
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
    pub ret: ValType,
}

#[derive(Clone, Debug, PartialEq)]
pub struct InlineFunction {
    pub args: Vec<(String, ValType)>,
    pub ret: ValType,
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
    pub variables: HashMap<String, (ValType, Option<TypInfo>)>,
    pub locals: HashMap<String, (ValType, Option<TypInfo>)>,
    pub defined_functions: HashMap<String, Rc<FunctionSignature>>,
    pub inline_vals: HashMap<String, (Typ, latex::Latex, Option<TypInfo>)>,
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
