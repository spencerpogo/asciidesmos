use std::{collections::HashMap, rc::Rc};
use types::ValType;

// heap version of core::runtime::Args
pub enum FunctionArgs {
    Static(Vec<ValType>),
    Variadic,
}

// heap version of core::runtime::Function
pub struct FunctionSignature {
    pub args: FunctionArgs,
    pub ret: ValType,
}

pub struct Context<'a> {
    pub variables: HashMap<&'a str, ValType>,
    pub locals: HashMap<String, ValType>,
    pub defined_functions: HashMap<String, Rc<FunctionSignature>>,
}

impl Context<'_> {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            locals: HashMap::new(),
            defined_functions: HashMap::new(),
        }
    }
}

impl Default for Context<'_> {
    fn default() -> Self {
        Self::new()
    }
}

pub struct ResolvedFunction {
    pub func: Rc<FunctionSignature>,
    pub is_builtin: bool,
}
