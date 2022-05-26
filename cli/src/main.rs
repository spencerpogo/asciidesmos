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
        .map(|l| latex::latex_to_str(l.unwrap()))
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

fn process(inp: &str, flags: Flags) -> i32 {
    match try_eval(inp, flags, std::io::stdout()) {
        Ok(()) => 0,
        Err(e) => {
            match e {
                EvalError::ParseErrors(p) => match p {
                    parser::LexParseErrors::LexErrors(errs) => eprintln!("{:#?}", errs),
                    parser::LexParseErrors::ParseErrors(errs) => eprintln!("{:#?}", errs),
                },
                EvalError::CompileError(c) => eprintln!("{}", c),
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
        );

    let matches = app.get_matches();
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
