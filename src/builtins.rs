use crate::types::{Function, ValType as VT};
use phf::{phf_map, Map};

macro_rules! f {
    ($args:expr) => {
        Function { args: &$args }
    };
}

macro_rules! n {
    () => {
        f!([&VT::Number])
    };
}

pub static BUILTIN_FUNCTIONS: Map<&'static str, Function<'static>> = phf_map! {
    "sin" => n!(),
    // TODO: Add more functions here
};
