use ariadne::{Color, Fmt, Label, Report, ReportKind};
use clap::{App, Arg};
use desmos_lang::compiler::{compile_stmt, error::CompileError, Context};
use std::fs::File;
use std::io::prelude::*;

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

enum Output {
    Latex,
    State,
}

struct Flags {
    ast: bool,
    ir: bool,
    output: Output,
}

fn try_eval(
    inp: &str,
    flags: Flags,
    mut out: impl std::io::Write + Sized,
) -> Result<(), EvalError> {
    let ast = parser::lex_and_parse(0, inp.to_string())?;
    if flags.ast {
        eprintln!("{:#?}", ast);
    }
    let mut ctx = Context::new();
    let ir = ast
        .into_iter()
        .map(|s| compile_stmt(&mut ctx, s))
        .collect::<Result<Vec<_>, _>>()?;
    if flags.ir {
        eprintln!("{:#?}", ir);
    }
    let r = ir
        .into_iter()
        .filter(Option::is_some)
        .map(|l| latex::latex_stmt_to_str(l.unwrap()))
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

pub fn print_parse_err_report(source: types::FileID, input: String, errs: parser::LexParseErrors) {
    let a: Vec<chumsky::error::Simple<String, types::Span>> = match errs {
        parser::LexParseErrors::LexErrors(errs) => errs
            .into_iter()
            .map(|e| e.map(|c| format!("`{}`", c.to_string())))
            .collect(),
        parser::LexParseErrors::ParseErrors(errs) => errs
            .into_iter()
            .map(|e| e.map(|c| c.to_str().to_string()))
            .collect(),
    };
    a.into_iter().for_each(|e| {
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
        .eprint(ariadne::sources(vec![(source, input.clone())].into_iter()))
        .unwrap();
    });
}

fn print_compile_error_report(source: types::FileID, input: String, err: CompileError) {
    let report =
        Report::<types::Span>::build(ReportKind::Error, err.span.file_id, err.span.range.start);
    report
        .with_message(format!("{}", err.kind))
        .with_label(Label::new(err.span).with_color(Color::Red))
        .finish()
        .eprint(ariadne::sources(vec![(source, input.clone())].into_iter()))
        .unwrap();
}

fn process(inp: &str, flags: Flags) -> i32 {
    match try_eval(inp, flags, std::io::stdout()) {
        Ok(()) => 0,
        Err(e) => {
            match e {
                EvalError::ParseErrors(errs) => print_parse_err_report(0, inp.to_string(), errs),
                EvalError::CompileError(c) => print_compile_error_report(0, inp.to_string(), c),
            };
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
        .arg(Arg::with_name("ast").long("ast").help("Dumps AST"))
        .arg(
            Arg::with_name("ir")
                .long("ir")
                .help("Dumps IR (latex syntax tree)"),
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
        lsp::main().unwrap();
        return;
    }
    // flags
    let flags = Flags {
        ast: matches.is_present("ast"),
        ir: matches.is_present("ir"),
        output: match matches.value_of("output").unwrap_or("state") {
            "latex" => Output::Latex,
            "state" => Output::State,
            _ => unreachable!(),
        },
    };

    let exit_code = if let Some(input) = matches.value_of("eval") {
        process(input, flags)
    } else if let Some(filename) = matches.value_of("file") {
        // TODO: Better error handling here?
        let mut file = File::open(filename).expect("Unable to read input");
        let mut contents = String::new();
        file.read_to_string(&mut contents)
            .expect("Unable to decode file contents");
        process(contents.as_str(), flags)
    } else {
        unimplemented!("REPL/pipe unimplemented")
    };
    std::process::exit(exit_code)
}
