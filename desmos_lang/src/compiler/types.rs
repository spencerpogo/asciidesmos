use std::{collections::HashMap, rc::Rc};
use types::ValType;

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

#[derive(Clone, Debug, PartialEq)]
pub struct Context {
    pub variables: HashMap<String, ValType>,
    pub locals: HashMap<String, ValType>,
    pub defined_functions: HashMap<String, Rc<FunctionSignature>>,
    pub inline_vals: HashMap<String, (ValType, latex::Latex)>,
    pub inline_fns: HashMap<String, Rc<InlineFunction>>,
    // can't support submodules (yet)
    pub modules: HashMap<String, Context>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            ..Default::default()
        }
    }
}

impl Default for Context {
    fn default() -> Self {
        Self::new()
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
