use ariadne::{Color, Fmt, Label, Report, ReportKind};
use clap::{App, Arg};
use compiler::{compile_stmts, error::CompileError, Context};
use parser::LexParseErrors;
use std::fs::File;
use std::io::prelude::*;
use std::rc::Rc;
use types::FileID;

#[derive(Debug, Default)]
pub struct EvalError {
    pub parse_errors: parser::LexParseErrors,
    pub compile_error: Option<CompileError>,
}

#[derive(Clone)]
pub struct SrcFile {
    pub name: String,
    pub src: Rc<ariadne::Source>,
}

#[derive(Clone)]
pub struct Sources {
    pub files: slab::Slab<SrcFile>,
}

impl Sources {
    fn new() -> Self {
        Self {
            files: slab::Slab::new(),
        }
    }
}

impl ariadne::Cache<FileID> for Sources {
    fn fetch(&mut self, id: &FileID) -> Result<&ariadne::Source, Box<dyn std::fmt::Debug + '_>> {
        let f = self.files.get(*id).ok_or(Box::new("No such file") as _)?;
        Ok(&f.src)
    }
    fn display<'a>(&self, id: &'a FileID) -> Option<Box<dyn std::fmt::Display + 'a>> {
        self.files.get(*id).map(|v| Box::new(v.name.clone()) as _)
    }
}

enum Output {
    Latex,
    State,
}

struct Flags {
    tokens: bool,
    token_spans: bool,
    ast: bool,
    ir: bool,
    output: Output,
    dump_errs: bool,
}

#[derive(Clone, Debug)]
struct CliLoader;

impl compiler::Loader for CliLoader {
    fn load(&self, _path: &str) -> Option<compiler::LStatements> {
        unimplemented!()
    }

    fn parse_source(&self, source: &str) -> Option<compiler::LStatements> {
        parser::lex_and_parse(0, source.to_string()).0
    }
}

fn try_eval(
    id: types::FileID,
    inp: &str,
    flags: &Flags,
    mut out: impl std::io::Write + Sized,
) -> Result<(), EvalError> {
    // return as soon as we get an unrecoverable error.
    // Otherwise, collect errors from all phases
    let (tokens, lex_errors) = parser::lex(id, inp.to_string());
    if flags.tokens {
        let v: Box<dyn std::fmt::Debug> = match &tokens {
            Some(tokens) => {
                if flags.token_spans {
                    Box::new(tokens)
                } else {
                    Box::new(tokens.iter().map(|(_s, t)| t).collect::<Vec<_>>())
                }
            }
            None => Box::<Option<()>>::new(None),
        };
        eprintln!("{:#?}", v);
    }
    let tokens = match tokens {
        None => {
            return Err(EvalError {
                parse_errors: LexParseErrors {
                    lex_errors,
                    ..Default::default()
                },
                ..Default::default()
            })
        }
        Some(tokens) => tokens,
    };
    let (ast, parse_errors) = parser::parse(id, tokens);
    if flags.ast {
        eprintln!("{:#?}", ast);
    }
    let parse_errors = LexParseErrors {
        lex_errors,
        parse_errors,
    };
    let ast = match ast {
        None => {
            return Err(EvalError {
                parse_errors,
                ..Default::default()
            })
        }
        Some(ast) => ast,
    };
    let ir = match compile_stmts(&mut Context::new_with_loader(Box::new(CliLoader)), ast) {
        Err(compile_error) => {
            return Err(EvalError {
                parse_errors,
                compile_error: Some(compile_error),
            })
        }
        Ok(ir) => {
            if !parse_errors.is_empty() {
                return Err(EvalError { parse_errors, ..Default::default() })
            }
            ir
        },
    };
    if flags.ir {
        eprintln!("{:#?}", ir);
    }
    let r = ir
        .into_iter()
        .map(|l| latex::latex_stmt_to_str(l))
        .collect::<Vec<_>>();
    Ok(match flags.output {
        Output::Latex => write!(&mut out, "{}", r.join("\n")).unwrap(),
        Output::State => serde_json::to_writer(
            out,
            &graph::CalcState {
                expressions: graph::Expressions::from_latex_strings(r),
                ..Default::default()
            },
        )
        .unwrap(),
    })
}

pub fn print_parse_err_report(sources: &mut Sources, errs: parser::LexParseErrors) {
    let a: Vec<chumsky::error::Simple<String, types::Span>> = errs
        .lex_errors
        .into_iter()
        .map(|e| e.map(|c| format!("`{}`", c.to_string())))
        .chain(
            errs.parse_errors
                .into_iter()
                .map(|e| e.map(|c| c.to_str().to_string())),
        )
        .collect();
    let mut sources = sources;
    for e in a.into_iter() {
        let report =
            Report::<types::Span>::build(ReportKind::Error, e.span().file_id, e.span().range.start);
        match e.reason() {
            chumsky::error::SimpleReason::Unclosed { span, delimiter } => report
                .with_message(format!(
                    "Unclosed delimiter {}",
                    delimiter.fg(Color::Yellow)
                ))
                .with_label(
                    Label::new(span.clone())
                        .with_message(format!(
                            "Unclosed delimiter {}",
                            delimiter.fg(Color::Yellow)
                        ))
                        .with_color(Color::Yellow),
                )
                .with_label(
                    Label::new(e.span())
                        .with_message(format!(
                            "Must be closed before this {}",
                            e.found()
                                .unwrap_or(&"end of file".to_string())
                                .fg(Color::Red)
                        ))
                        .with_color(Color::Red),
                ),
            chumsky::error::SimpleReason::Unexpected => report
                .with_message(format!(
                    "Unexpected {}, expected {}",
                    if e.found().is_some() {
                        "token in input"
                    } else {
                        "end of input"
                    },
                    if e.expected().len() == 0 {
                        "something else".to_string()
                    } else {
                        e.expected()
                            .map(|expected| match expected {
                                Some(v) => v,
                                None => "end of input",
                            })
                            .collect::<Vec<_>>()
                            .join(", ")
                    }
                ))
                .with_label(
                    Label::new(e.span())
                        .with_message(format!(
                            "Unexpected {}",
                            e.found()
                                .unwrap_or(&"end of file".to_string())
                                .fg(Color::Red)
                        ))
                        .with_color(Color::Red),
                ),
            chumsky::error::SimpleReason::Custom(msg) => report.with_message(msg).with_label(
                Label::new(e.span())
                    .with_message(format!("{}", msg.fg(Color::Red)))
                    .with_color(Color::Red),
            ),
        }
        .finish()
        // might want to avoid this in the future
        .eprint(&mut sources)
        .unwrap();
    }
}

fn print_compile_error_report(sources: &mut Sources, err: CompileError) {
    let mut report =
        Report::<types::Span>::build(ReportKind::Error, err.span.file_id, err.span.range.start)
            .with_message(format!("{}", err.kind))
            .with_label(Label::new(err.span).with_color(Color::Red));

    if let Some(help) = err.kind.help() {
        report.set_help(help);
    }
    report.finish().eprint(sources).unwrap();
}

fn process(name: String, inp: &str, flags: &Flags) -> i32 {
    let mut sources = Sources::new();
    let id = sources.files.insert(SrcFile {
        name,
        src: Rc::new(ariadne::Source::from(inp)),
    });
    match try_eval(id, inp, &flags, std::io::stdout()) {
        Ok(()) => 0,
        Err(e) => {
            if flags.dump_errs {
                eprintln!("{:#?}", e);
            }
            let mut exit = 0;
            if !e.parse_errors.is_empty() {
                print_parse_err_report(&mut sources, e.parse_errors);
                exit = 1;
            }
            if let Some(compile_error) = e.compile_error {
                print_compile_error_report(&mut sources, compile_error);
                exit = 1;
            }
            exit
        }
    }
}

fn lsp_main() -> Result<(), Box<dyn std::error::Error + Sync + Send>> {
    // Note that  we must have our logging only write out to stderr.
    eprintln!("starting generic LSP server");

    // Create the transport. Includes the stdio (stdin and stdout) versions but this could
    // also be implemented to use sockets or HTTP.
    let (connection, io_threads) = lsp_server::Connection::stdio();
    lsp::start(connection)?;
    // Shut down gracefully.
    io_threads.join()?;
    eprintln!("shutting down server");
    Ok(())
}

fn main() {
    let app = App::new("desmosc")
        .version("0.1")
        .author("Scoder12 <scoder12.ml>")
        .about("A simple language for generating desmos graphs.")
        .arg(
            Arg::with_name("eval")
                .short("e")
                .long("eval")
                .help("Compile code from a cmdline arg")
                .takes_value(true)
                .conflicts_with("file"),
        )
        .arg(
            Arg::with_name("file")
                .short("f")
                .long("file")
                .help("Compile code from a file")
                .takes_value(true)
                .conflicts_with("eval"),
        )
        .arg(Arg::with_name("tokens").long("tokens").help("Dump tokens"))
        .arg(
            Arg::with_name("token spans")
                .long("token-spans")
                .help("When dumping tokens, include spans"),
        )
        .arg(Arg::with_name("ast").long("ast").help("Dumps AST"))
        .arg(
            Arg::with_name("ir")
                .long("ir")
                .help("Dumps IR (latex syntax tree)"),
        )
        .arg(
            Arg::with_name("dump errors")
                .long("dump-errs")
                .help("Dump raw error struct"),
        )
        .arg(
            Arg::with_name("output")
                .long("output")
                .takes_value(true)
                .possible_values(&["latex", "state"])
                .help("Output latex lines or calculator state"),
        )
        .arg(
            Arg::with_name("lsp")
                .long("lsp")
                .help("Start LSP server (hack)"),
        );

    let matches = app.get_matches();
    if matches.is_present("lsp") {
        lsp_main().unwrap();
        return;
    }
    // flags
    let flags = Flags {
        tokens: matches.is_present("tokens"),
        token_spans: matches.is_present("token spans"),
        ast: matches.is_present("ast"),
        ir: matches.is_present("ir"),
        output: match matches.value_of("output").unwrap_or("state") {
            "latex" => Output::Latex,
            "state" => Output::State,
            _ => unreachable!(),
        },
        dump_errs: matches.is_present("dump errors"),
    };

    let exit_code = if let Some(input) = matches.value_of("eval") {
        process("<string>".to_string(), input, &flags)
    } else if let Some(filename) = matches.value_of("file") {
        // TODO: Better error handling here?
        let mut file = File::open(filename).expect("Unable to read input");
        let mut contents = String::new();
        file.read_to_string(&mut contents)
            .expect("Unable to decode file contents");
        process(filename.to_string(), contents.as_str(), &flags)
    } else {
        unimplemented!("REPL/pipe unimplemented")
    };
    std::process::exit(exit_code)
}
