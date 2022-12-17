use ariadne::{Color, Fmt, Label, Report, ReportKind};
use clap::{App, Arg};
use compiler::{compile_stmts, error::CompileError, Context};
use std::fs::File;
use std::io::prelude::*;
use std::rc::Rc;
use types::FileID;

#[derive(Debug, Default)]
pub struct EvalError {
    pub lex_errors: Vec<parser::LexErr>,
    pub parse_errors: Vec<parser::ParseErr>,
    pub compile_error: Option<CompileError>,
}

impl EvalError {
    fn is_empty(&self) -> bool {
        self.lex_errors.is_empty() && self.parse_errors.is_empty() && self.compile_error.is_none()
    }
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
    let mut err = EvalError {
        ..Default::default()
    };

    let (tokens, lex_errors) = parser::lex(id, inp.to_string());
    err.lex_errors = lex_errors;
    let tokens = match tokens {
        None => return Err(err),
        Some(tokens) => tokens,
    };

    if flags.tokens && flags.token_spans {
        eprintln!("{:#?}", tokens);
    } else if flags.tokens {
        eprintln!("{:#?}", tokens.iter().map(|(_s, t)| t).collect::<Vec<_>>());
    };

    let (ast, parse_errors) = parser::parse(id, tokens);
    err.parse_errors = parse_errors;
    let ast = match ast {
        None => return Err(err),
        Some(ast) => ast,
    };

    if flags.ast {
        eprintln!("{:#?}", ast);
    }

    let ir = match compile_stmts(&mut Context::new_with_loader(Box::new(CliLoader)), ast) {
        Err(compile_error) => {
            err.compile_error = Some(compile_error);
            return Err(err);
        }
        Ok(ir) => ir,
    };

    if flags.ir {
        eprintln!("{:#?}", ir);
    }

    if !err.is_empty() {
        return Err(err);
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

// Format the error's input type as a String
type GenericParseErr = chumsky::error::Simple<String, types::Span>;

pub fn fmt_lex_errs<I>(lex_errs: I) -> impl IntoIterator<Item = GenericParseErr>
where
    I: IntoIterator<Item = parser::LexErr>,
{
    lex_errs.into_iter().map(|e| e.map(|c| format!("`{}`", c)))
}

pub fn fmt_parse_errs<I>(parse_errs: I) -> impl IntoIterator<Item = GenericParseErr>
where
    I: IntoIterator<Item = parser::ParseErr>,
{
    parse_errs
        .into_iter()
        .map(|e| e.map(|c| c.to_str().to_string()))
}

pub fn fmt_lexparse_errs(
    lex_errs: Vec<parser::LexErr>,
    parse_errs: Vec<parser::ParseErr>,
) -> impl IntoIterator<Item = GenericParseErr> {
    fmt_lex_errs(lex_errs)
        .into_iter()
        .chain(fmt_parse_errs(parse_errs))
}

pub fn print_parse_err_report<I>(sources: &mut Sources, errs: I)
where
    I: IntoIterator<Item = GenericParseErr>,
{
    // stack copy to avoid clones
    let mut sources = sources;
    for e in errs.into_iter() {
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
            print_parse_err_report(
                &mut sources,
                fmt_lexparse_errs(e.lex_errors, e.parse_errors),
            );
            if let Some(compile_error) = e.compile_error {
                print_compile_error_report(&mut sources, compile_error);
            }
            1
        }
    }
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
        );

    let matches = app.get_matches();
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
