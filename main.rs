use clap::{App, Arg};
use desmos_lang;
use std::fs::File;
use std::io::prelude::*;

fn process(inp: &str, show_ast: bool) {
    let parse_result = desmos_lang::parse(inp);
    let ast = match parse_result {
        Ok((unparsed, ast)) => {
            if unparsed.len() > 0 {
                // TODO: handle this better
                println!("Parse Error: Unable to parse full input");
                return;
            }
            ast
        }
        Err(err) => {
            // TODO: Better error handling
            println!("Parse Error: {:#?}", err);
            return;
        }
    };

    if show_ast {
        println!("Parsed AST: {:#?}", ast);
    }
    let latex = desmos_lang::ast_to_latex(&ast);
    println!("{}", latex);
}

fn main() {
    let app = App::new("desmosc")
        .version("0.1")
        .author("Scoder12 <scoder12.ml>")
        .about("A simple language for generating desmos graphs.")
        .arg(
            Arg::with_name("eval")
                .short("e")
                .long("config")
                .help("Compile code from a cmdline arg")
                .takes_value(true)
                .conflicts_with("file"),
        )
        .arg(
            Arg::with_name("file")
                .short("-f")
                .long("--file")
                .help("Compile code from a file")
                .takes_value(true)
                .conflicts_with("eval"),
        )
        .arg(
            Arg::with_name("ast")
                .long("ast")
                .help("Prints parsed AST. Useful for debugging."),
        );

    let matches = app.get_matches();
    // flags
    let show_ast = matches.is_present("ast");

    if let Some(input) = matches.value_of("eval") {
        process(input, show_ast);
    } else if let Some(filename) = matches.value_of("file") {
        // TODO: Better error handling here?
        let mut file = File::open(filename).expect("Unable to read input");
        let mut contents = String::new();
        file.read_to_string(&mut contents)
            .expect("Unable to decode file contents");
        process(contents.as_str(), show_ast);
    } else {
        println!("Will read from stdin");
    }
}
