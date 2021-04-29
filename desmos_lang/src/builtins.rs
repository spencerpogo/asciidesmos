use crate::types::{Function, ValType as VT};
use phf::{phf_map, Map};

macro_rules! f {
    ($args:expr, $ret:expr) => {
        Function {
            args: $args,
            ret: $ret,
        }
    };
}

macro_rules! n {
    () => {
        f!(&[VT::Number], VT::Number)
    };
}

pub static BUILTIN_FUNCTIONS: Map<&'static str, Function> = phf_map! {
    "sin" => n!(),
    // TODO: Add more functions here
};
