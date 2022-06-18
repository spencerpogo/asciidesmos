use desmos_lang::compiler::error::CompileError;
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

fn dbg(log: &js_sys::Function, tag: &str, v: &dyn std::fmt::Debug) {
    log.call1(
        &JsValue::null(),
        &JsValue::from(format!("{}: {:#?}", tag, v)),
    )
    .unwrap();
}

#[wasm_bindgen]
pub struct LspState {
    state: lsp::State,
}

#[wasm_bindgen]
pub fn lsp_state_new() -> LspState {
    LspState { state: None }
}

#[wasm_bindgen]
pub fn lsp_request(state: &mut LspState, s: &str, log: &js_sys::Function) -> String {
    let msg: lsp_server::Message = serde_json::from_str(s).unwrap();
    //dbg(log, "msg", &msg);
    let resp = lsp::handle_request(&mut state.state, msg);
    dbg(log, "state", &state.state);
    match resp {
        None => "".to_string(),
        Some(v) => serde_json::to_string(&v).unwrap(),
    }
}
