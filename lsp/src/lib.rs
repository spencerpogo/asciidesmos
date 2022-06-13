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
use lsp_types::request::{Completion, Initialize};
use lsp_types::{CompletionItem, CompletionOptions, CompletionResponse, InitializeResult, OneOf};
use lsp_types::{InitializeParams, ServerCapabilities};

use lsp_server::{Connection, Message, Request, Response};

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
    //let mut state: Option<State> = None;
    for msg in &connection.receiver {
        eprintln!("got msg: {:?}", msg);
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                eprintln!("got request: {:?}", req);
                if let Some(resp) = handle_request(req) {
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

#[derive(Clone, Debug)]
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

pub fn handle_request(req: Request) -> Option<Response> {
    let mut dispatcher = RequestDispatcher::new(req);
    dispatcher
        .on::<Initialize>(|_params| {
            let capabilities = ServerCapabilities {
                definition_provider: Some(OneOf::Left(true)),
                completion_provider: Some(CompletionOptions {
                    ..Default::default()
                }),
                ..Default::default()
            };
            Some(InitializeResult {
                capabilities,
                ..Default::default()
            })
        })
        .on::<Completion>(|_params| {
            let item = CompletionItem::new_simple("hello".to_string(), "world".to_string());
            Some(Some(CompletionResponse::Array(vec![item])))
        });
    dispatcher.resp
}
