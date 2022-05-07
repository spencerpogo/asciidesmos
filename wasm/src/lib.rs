use desmos_lang::{
    compiler::{compile_stmt, error::CompileError, Context},
    parser::parser::{parse, ParseError},
};
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
pub enum EvalError<'a> {
    ParseError(ParseError),
    CompileError(CompileError<'a>),
}

impl From<ParseError> for EvalError<'_> {
    fn from(err: ParseError) -> Self {
        Self::ParseError(err)
    }
}

impl<'a> From<CompileError<'a>> for EvalError<'a> {
    fn from(err: CompileError<'a>) -> Self {
        Self::CompileError(err)
    }
}

fn eval(inp: &str) -> Result<EvalResult, EvalError<'_>> {
    let ast = parse(inp)?;
    let ast_str = format!("{:#?}", ast);
    let ir = compile_stmt(&mut Context::new(), ast)?;
    let ir_str = format!("{:#?}", ir);
    let output = latex::latex_to_str(ir);
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
                EvalError::ParseError(p) => format!("Parse error: {}", p),
                EvalError::CompileError(c) => format!("Compile error: {}", c),
            },
        },
    }
}
