use crate::types::Function;
use phf::{phf_map, Map};

macro_rules! f {
    ($argc:expr) => {
        Function { argc: $argc }
    };
}

pub static BUILTIN_FUNCTIONS: Map<&'static str, Function> = phf_map! {
    "sin" => f!(1)
    // TODO: Add more functions here
};
