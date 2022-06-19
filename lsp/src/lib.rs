// A minimal example LSP server that can only respond to the `gotoDefinition` request. To use
// this example, execute it and then send an `initialize` request.
//
// ```no_run
// Content-Length: 85
//
// {"jsonrpc": "2.0", "method": "initialize", "id": 1, "params": {"capabilities": {}}}
// ```
//
// This will respond with a server response. Then send it a `initialized` notification which will
// have no response.
//
// ```no_run
// Content-Length: 59
//
// {"jsonrpc": "2.0", "method": "initialized", "params": {}}
// ```
//
// Once these two are sent, then we enter the main loop of the server. The only request this
// example can handle is `gotoDefinition`:
//
// ```no_run
// Content-Length: 159
//
// {"jsonrpc": "2.0", "method": "textDocument/definition", "id": 2, "params": {"textDocument": {"uri": "file://temp"}, "position": {"line": 1, "character": 1}}}
// ```
//
// To finish up without errors, send a shutdown request:
//
// ```no_run
// Content-Length: 67
//
// {"jsonrpc": "2.0", "method": "shutdown", "id": 3, "params": null}
// ```
//
// The server will exit the main loop and finally we send a `shutdown` notification to stop
// the server.
//
// ```no_run
// Content-Length: 54
//
// {"jsonrpc": "2.0", "method": "exit", "params": null}
// ```
use std::error::Error;

use compiler::error::CompileError;
use compiler::Context;
use lsp_types::notification::{DidChangeTextDocument, DidOpenTextDocument};
use lsp_types::request::{Completion, Initialize};
use lsp_types::{
    CompletionItem, CompletionOptions, CompletionParams, CompletionResponse, InitializeResult,
    OneOf, TextDocumentSyncCapability, TextDocumentSyncKind, TextDocumentSyncOptions,
};
use lsp_types::{InitializeParams, ServerCapabilities};

use lsp_server::{Connection, Message, Response};
use parser::LexParseErrors;

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

pub type State = Option<StateVal>;

#[derive(Clone, Debug)]
pub enum StateVal {
    ParseErr(LexParseErrors),
    CompileErr(CompileError),
    Success(Context),
    Test2,
}

pub fn main_loop(
    connection: Connection,
    params: serde_json::Value,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let _params: InitializeParams = serde_json::from_value(params).unwrap();
    eprintln!("starting example main loop");
    let mut state: State = None;
    for msg in &connection.receiver {
        eprintln!("got msg: {:?}", msg);
        if let Some(resp) = handle_request(&mut state, msg) {
            connection.sender.send(Message::Response(resp))?;
        }
    }
    Ok(())
}

#[derive(Debug)]
struct RequestDispatcher<'a> {
    state: &'a mut State,
    msg: Option<Message>,
    handled: bool,
    pub resp: Option<Response>,
}

impl<'a> RequestDispatcher<'a> {
    fn new(state: &'a mut State, msg: Message) -> Self {
        Self {
            state,
            msg: Some(msg),
            handled: false,
            resp: None,
        }
    }

    fn on<R>(&mut self, handler: fn(&mut State, &R::Params) -> Option<R::Result>) -> &mut Self
    where
        R: lsp_types::request::Request,
    {
        if self.handled {
            return self;
        }
        match &self.msg {
            Some(Message::Request(r)) => {
                if &r.method == R::METHOD {
                    let r = match self.msg.take().unwrap() {
                        Message::Request(r) => r,
                        _ => unreachable!(),
                    };
                    let params =
                        serde_json::from_value::<R::Params>(r.params).expect("Failed to parse");
                    self.resp = Some(Response::new_ok(r.id.clone(), handler(self.state, &params)));
                    self.handled = true;
                }
            }
            _ => (),
        }
        self
    }

    fn on_notif<N>(&mut self, handler: fn(&mut State, N::Params)) -> &mut Self
    where
        N: lsp_types::notification::Notification,
    {
        if self.handled {
            return self;
        }
        match &self.msg {
            Some(Message::Notification(n)) => {
                if n.method == N::METHOD {
                    let n = match self.msg.take().unwrap() {
                        Message::Notification(n) => n,
                        _ => unreachable!(),
                    };
                    let params =
                        serde_json::from_value::<N::Params>(n.params).expect("Failed to parse");
                    handler(self.state, params);
                    self.handled = true;
                }
            }
            _ => (),
        }
        self
    }
}

pub fn completion_handler(
    state: &mut State,
    _params: &CompletionParams,
) -> Option<Option<CompletionResponse>> {
    match state {
        None => Some(None),
        Some(s) => match &s {
            StateVal::Success(ctx) => Some(Some(CompletionResponse::Array(
                ctx.variables
                    .iter()
                    .map(|(v, typ)| CompletionItem::new_simple(v.clone(), format!("{:#?}", typ)))
                    .collect(),
            ))),
            _ => Some(None),
        },
    }
}

pub fn handle_new_content(_state: &mut State, _content: String) {
    // todo
}

pub fn handle_request(state: &mut State, msg: Message) -> Option<Response> {
    let mut dispatcher = RequestDispatcher::new(state, msg);
    dispatcher
        .on::<Initialize>(|_state, _params| {
            let capabilities = ServerCapabilities {
                definition_provider: Some(OneOf::Left(true)),
                completion_provider: Some(CompletionOptions {
                    ..Default::default()
                }),
                text_document_sync: Some(TextDocumentSyncCapability::Options(
                    TextDocumentSyncOptions {
                        open_close: Some(true),
                        change: Some(TextDocumentSyncKind::FULL),
                        ..Default::default()
                    },
                )),
                ..Default::default()
            };
            Some(InitializeResult {
                capabilities,
                ..Default::default()
            })
        })
        .on_notif::<DidOpenTextDocument>(|state, params| {
            *state = Some(StateVal::Test2);
            handle_new_content(state, params.text_document.text)
        })
        .on_notif::<DidChangeTextDocument>(|state, params| {
            if params.content_changes.len() != 1 {
                panic!("Expected one content change");
            }
            handle_new_content(
                state,
                params.content_changes.into_iter().nth(0).unwrap().text,
            )
        })
        .on::<Completion>(completion_handler);
    dispatcher.resp
}
