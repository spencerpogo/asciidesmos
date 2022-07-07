use ast::LStatements;

use crate::types::Loader;

#[derive(Clone, Debug)]
pub struct StdlibLoader {
    pub loader: Box<dyn Loader>,
    pub test: Option<LStatements>,
}

static TEST_DSM: &str = include_str!("test.dsm");

impl StdlibLoader {
    pub fn new(loader: Box<dyn Loader>) -> Self {
        Self { loader, test: None }
    }

    pub fn load_lib(&mut self, lib: &str) -> Option<LStatements> {
        match lib {
            "test" => Some(StdlibLoader::load_resolved_lib(
                &mut self.loader,
                TEST_DSM,
                &mut self.test,
            )),
            _ => None,
        }
    }

    fn load_resolved_lib(
        loader: &mut Box<dyn Loader>,
        src: &str,
        lib: &mut Option<LStatements>,
    ) -> LStatements {
        match lib {
            Some(ast) => ast.clone(),
            None => {
                let ast = loader.load_string(src);
                *lib = Some(ast.clone());
                ast
            }
        }
    }
}
