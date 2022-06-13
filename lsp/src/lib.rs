//! A minimal example LSP server that can only respond to the `gotoDefinition` request. To use
//! this example, execute it and then send an `initialize` request.
//!
//! ```no_run
//! Content-Length: 85
//!
//! {"jsonrpc": "2.0", "method": "initialize", "id": 1, "params": {"capabilities": {}}}
//! ```
//!
//! This will respond with a server response. Then send it a `initialized` notification which will
//! have no response.
//!
//! ```no_run
//! Content-Length: 59
//!
//! {"jsonrpc": "2.0", "method": "initialized", "params": {}}
//! ```
//!
//! Once these two are sent, then we enter the main loop of the server. The only request this
//! example can handle is `gotoDefinition`:
//!
//! ```no_run
//! Content-Length: 159
//!
//! {"jsonrpc": "2.0", "method": "textDocument/definition", "id": 2, "params": {"textDocument": {"uri": "file://temp"}, "position": {"line": 1, "character": 1}}}
//! ```
//!
//! To finish up without errors, send a shutdown request:
//!
//! ```no_run
//! Content-Length: 67
//!
//! {"jsonrpc": "2.0", "method": "shutdown", "id": 3, "params": null}
//! ```
//!
//! The server will exit the main loop and finally we send a `shutdown` notification to stop
//! the server.
//!
//! ```
//! Content-Length: 54
//!
//! {"jsonrpc": "2.0", "method": "exit", "params": null}
//! ```
use std::error::Error;

use desmos_lang::compiler::Context;
use lsp_types::request::Completion;
use lsp_types::{
    request::GotoDefinition, GotoDefinitionResponse, InitializeParams, ServerCapabilities,
};
use lsp_types::{CompletionItem, CompletionOptions, CompletionResponse, OneOf};

use lsp_server::{Connection, ExtractError, Message, Request, RequestId, Response};

pub fn start(connection: Connection) -> Result<(), Box<dyn Error + Sync + Send>> {
    // Run the server and wait for the two threads to end (typically by trigger LSP Exit event).
    let server_capabilities = serde_json::to_value(&ServerCapabilities {
        definition_provider: Some(OneOf::Left(true)),
        ..Default::default()
    })
    .unwrap();
    let initialization_params = connection.initialize(server_capabilities)?;
    main_loop(connection, initialization_params)?;
    Ok(())
}

#[derive(Clone, Debug, PartialEq)]
pub struct State {
    pub src: String,
    pub ctx: Context,
}

pub fn main_loop(
    connection: Connection,
    params: serde_json::Value,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let _params: InitializeParams = serde_json::from_value(params).unwrap();
    eprintln!("starting example main loop");
    let mut state: Option<State> = None;
    for msg in &connection.receiver {
        eprintln!("got msg: {:?}", msg);
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                eprintln!("got request: {:?}", req);
                if let Some(resp) = handle_request(&mut state, req) {
                    connection.sender.send(Message::Response(resp))?;
                }
            }
            Message::Response(resp) => {
                eprintln!("got response: {:?}", resp);
            }
            Message::Notification(not) => {
                eprintln!("got notification: {:?}", not);
            }
        }
    }
    Ok(())
}

fn cast<R>(req: Request) -> Result<(RequestId, R::Params), ExtractError<Request>>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}

struct RequestDispatcher {
    pub req: Request,
    pub resp: Option<Response>,
}

impl RequestDispatcher {
    fn new(req: Request) -> Self {
        Self { req, resp: None }
    }

    fn on<R>(&mut self, handler: fn(R::Params) -> Option<R::Result>) -> &mut Self
    where
        R: lsp_types::request::Request,
    {
        if self.resp.is_some() {
            // already handled
            return self;
        }
        if self.req.method == R::METHOD {
            let params = serde_json::from_value::<R::Params>(self.req.params.clone())
                .expect("Failed to parse");
            self.resp = Some(Response::new_ok(self.req.id.clone(), handler(params)));
        }
        self
    }
}

pub fn handle_request(state: &mut Option<State>, req: Request) -> Option<Response> {
    let req = match cast::<lsp_types::request::Initialize>(req) {
        Ok((id, _params)) => {
            let server_capabilities = serde_json::to_value(&ServerCapabilities {
                definition_provider: Some(OneOf::Left(true)),
                completion_provider: Some(CompletionOptions {
                    ..Default::default()
                }),
                ..Default::default()
            })
            .unwrap();
            return Some(Response::new_ok(
                id,
                serde_json::json!({
                    "capabilities": server_capabilities,
                }),
            ));
        }
        Err(err @ ExtractError::JsonError { .. }) => panic!("{:?}", err),
        Err(ExtractError::MethodMismatch(req)) => req,
    };
    let req = match cast::<GotoDefinition>(req) {
        Ok((id, params)) => {
            eprintln!("got gotoDefinition request #{}: {:?}", id, params);
            let result = Some(GotoDefinitionResponse::Array(Vec::new()));
            let result = serde_json::to_value(&result).unwrap();
            return Some(Response {
                id,
                result: Some(result),
                error: None,
            });
        }
        Err(err @ ExtractError::JsonError { .. }) => panic!("{:?}", err),
        Err(ExtractError::MethodMismatch(req)) => req,
    };
    match cast::<Completion>(req) {
        Ok((id, params)) => {
            eprintln!("got completion request #{}: {:#?}", id, params);
            let item = CompletionItem::new_simple("hello".to_string(), "world".to_string());
            let result = CompletionResponse::Array(vec![item]);
            return Some(Response {
                id,
                result: Some(serde_json::to_value(&result).unwrap()),
                error: None,
            });
        }
        Err(err @ ExtractError::JsonError { .. }) => panic!("{:?}", err),
        Err(ExtractError::MethodMismatch(_)) => (),
    }
    None
}
