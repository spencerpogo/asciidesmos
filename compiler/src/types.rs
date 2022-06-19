use ast::LStatements;
use std::{collections::HashMap, fmt::Debug, rc::Rc};
use types::ValType;

use crate::stdlib::StdlibLoader;

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
    fn load(&mut self, path: &str) -> Option<LStatements>;

    fn load_stdlib_file(&mut self, contents: &str) -> LStatements;
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
    fn load(&mut self, _path: &str) -> Option<LStatements> {
        unimplemented!()
    }

    fn load_stdlib_file(&mut self, _contents: &str) -> LStatements {
        unimplemented!()
    }
}

#[derive(Clone, Debug)]
pub struct Context {
    pub variables: HashMap<String, ValType>,
    pub locals: HashMap<String, ValType>,
    pub defined_functions: HashMap<String, Rc<FunctionSignature>>,
    pub inline_vals: HashMap<String, (ValType, latex::Latex)>,
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
            stdlib: StdlibLoader::new(loader.clone()),
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
