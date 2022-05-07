use clap::{App, Arg};
use desmos_lang::{
    compiler::{compile_stmt, error::CompileError, Context},
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

struct Flags {
    ast: bool,
    ir: bool,
}

fn try_eval(inp: &str, flags: Flags) -> Result<String, EvalError<'_>> {
    let ast = parse(inp)?;
    if flags.ast {
        eprintln!("{:#?}", ast);
    }
    let ir = compile_stmt(&mut Context::new(), ast)?;
    if flags.ir {
        eprintln!("{:#?}", ir);
    }
    let r = latex::latex_to_str(ir);
    Ok(r)
}

fn process(inp: &str, flags: Flags) -> i32 {
    match try_eval(inp, flags) {
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
        .arg(Arg::with_name("ast").long("ast").help("Dumps AST"))
        .arg(
            Arg::with_name("ir")
                .long("ir")
                .help("Dumps IR (latex syntax tree)"),
        );

    let matches = app.get_matches();
    // flags
    let flags = Flags {
        ast: matches.is_present("ast"),
        ir: matches.is_present("ir"),
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
