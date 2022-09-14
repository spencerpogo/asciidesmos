use std::collections::HashMap;

use ast::LStatements;
use phf::{phf_map, Map};

use crate::types::Loader;

#[derive(Clone, Debug)]
pub struct StdlibLoader {
    pub loader: Box<dyn Loader>,
    pub cache: HashMap<String, LStatements>,
}

pub static STDLIB_SOURCES: Map<&'static str, &'static str> = phf_map! {
    "test" => include_str!("test.dsm"),
};

impl StdlibLoader {
    pub fn new(loader: Box<dyn Loader>) -> Self {
        Self {
            loader,
            cache: HashMap::new(),
        }
    }

    pub fn load_lib(&mut self, name: &str) -> Option<LStatements> {
        if let Some(ast) = self.cache.get(name) {
            return Some(ast.clone());
        };
        match STDLIB_SOURCES.get(name) {
            Some(source_code) => match self.loader.parse_source(source_code) {
                Some(ast) => {
                    self.cache.insert(name.to_owned(), ast.clone());
                    Some(ast)
                }
                None => None,
            },
            None => None,
        }
    }
}
