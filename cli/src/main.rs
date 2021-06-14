use clap::{App, Arg};
use desmos_lang::{
    compiler::{
        compiler::{compile_stmt, Context},
        error::CompileError,
    },
    core::latex::latex_to_str,
    parser::parser::{parse, ParseError},
};
use std::fs::File;
use std::io::prelude::*;

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

fn try_eval(inp: &str, debug: bool) -> Result<String, EvalError<'_>> {
    let ast = parse(inp)?;
    if debug {
        eprintln!("AST:\n{:#?}", ast);
    }
    let ir = compile_stmt(&mut Context::new(), ast)?;
    if debug {
        eprintln!("IR:\n{:#?}", ir);
    }
    let r = latex_to_str(ir);
    Ok(r)
}

fn process(inp: &str, debug: bool) -> i32 {
    match try_eval(inp, debug) {
        Ok(s) => {
            println!("{}", s);
            0
        }
        Err(e) => {
            match e {
                EvalError::ParseError(p) => eprintln!("{}", p),
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
        .arg(
            Arg::with_name("debug")
                .long("debug")
                .help("Dumps AST and IR"),
        );

    let matches = app.get_matches();
    // flags
    let debug = matches.is_present("debug");

    let exit_code = if let Some(input) = matches.value_of("eval") {
        process(input, debug)
    } else if let Some(filename) = matches.value_of("file") {
        // TODO: Better error handling here?
        let mut file = File::open(filename).expect("Unable to read input");
        let mut contents = String::new();
        file.read_to_string(&mut contents)
            .expect("Unable to decode file contents");
        process(contents.as_str(), debug)
    } else {
        unimplemented!("REPL/pipe unimplemented")
    };
    std::process::exit(exit_code)
}
