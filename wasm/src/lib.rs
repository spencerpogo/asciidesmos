use desmos_lang::compiler::{compile_stmt, error::CompileError, Context};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn greet(name: &str) -> String {
    format!("Hello, {}!", name)
}

#[wasm_bindgen(getter_with_clone)]
pub struct EvalResult {
    pub ast: String,
    pub ir: String,
    pub output: String,
}

#[derive(Debug)]
pub enum EvalError {
    ParseErrors(parser::LexParseErrors),
    CompileError(CompileError),
}

impl From<parser::LexParseErrors> for EvalError {
    fn from(errs: parser::LexParseErrors) -> Self {
        Self::ParseErrors(errs)
    }
}

impl From<CompileError> for EvalError {
    fn from(err: CompileError) -> Self {
        Self::CompileError(err)
    }
}

fn eval(inp: &str) -> Result<EvalResult, EvalError> {
    let ast = parser::lex_and_parse(0, inp.to_string())?;
    let mut ctx = Context::new();
    // saving in variable avoids a clone
    let ast_str = format!("{:#?}", ast);
    let ir = ast
        .clone()
        .into_iter()
        .map(|s| compile_stmt(&mut ctx, s))
        .collect::<Result<Vec<_>, _>>()?;
    let ir_str = format!("{:#?}", ir);
    let output = ir
        .into_iter()
        .map(|l| latex::latex_to_str(l))
        .collect::<Vec<String>>()
        .join("\n");
    Ok(EvalResult {
        ast: ast_str,
        ir: ir_str,
        output,
    })
}

#[wasm_bindgen]
pub fn try_eval(inp: &str) -> EvalResult {
    let r = eval(inp);
    match r {
        Ok(result) => result,
        Err(e) => EvalResult {
            ast: "".to_string(),
            ir: "".to_string(),
            output: match e {
                EvalError::ParseErrors(p) => format!("Parse error: {:#?}", p),
                EvalError::CompileError(c) => format!("Compile error: {}", c),
            },
        },
    }
}
