use crate::types::{ArgType as AT, Function};
use phf::{phf_map, Map};

macro_rules! f {
    ($args:expr) => {
        Function { args: &$args }
    };
}

macro_rules! n {
    () => {
        f!([&AT::Number])
    };
}

pub static BUILTIN_FUNCTIONS: Map<&'static str, Function<'static>> = phf_map! {
    "sin" => n!(),
    // TODO: Add more functions here
};
