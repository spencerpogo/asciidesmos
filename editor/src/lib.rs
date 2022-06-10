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
    let latex_strs = ir
        .into_iter()
        .filter(Option::is_some)
        .map(|l| latex::latex_stmt_to_str(l.unwrap()))
        .collect::<Vec<String>>();
    let output = serde_json::to_string(&graph::CalcState {
        expressions: graph::Expressions::from_latex_strings(latex_strs),
        ..Default::default()
    })
    .unwrap();
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
        Err(e) => {
            let err = match e {
                EvalError::ParseErrors(p) => format!("Parse error: {:#?}", p),
                EvalError::CompileError(c) => format!("Compile error: {}", c),
            };
            EvalResult {
                ast: "".to_string(),
                ir: "".to_string(),
                output: serde_json::to_string(&serde_json::json!({ "error": err })).unwrap(),
            }
        }
    }
}

#[wasm_bindgen]
pub fn js_closure_test(f: &js_sys::Function) {
    f.call1(
        &JsValue::null(),
        &JsValue::from("Hello from rust. Build: 2"),
    )
    .unwrap();
}

fn dbg(log: &js_sys::Function, v: &dyn std::fmt::Debug) {
    log.call1(&JsValue::null(), &JsValue::from(format!("{:#?}", v)))
        .unwrap();
}

#[wasm_bindgen]
pub fn lsp_request(s: &str, log: &js_sys::Function) -> String {
    let msg: lsp_server::Message = serde_json::from_str(s).unwrap();
    dbg(log, &msg);
    let v = match msg {
        lsp_server::Message::Request(r) => lsp::handle_request(r),
        _ => None,
    };
    serde_json::to_string(&v).unwrap()
}
