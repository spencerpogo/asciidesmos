use clap::{App, Arg};
use desmos_lang;
use std::fs::File;
use std::io::prelude::*;

fn process(inp: &str, _show_ast: bool) {
    match &mut desmos_lang::parser::parse(inp) {
        Err(e) => println!("{}", e),
        Ok(r) => {
            // This is safe to unwrap because there should only be panic-level
            //  assertion errors thrown while traversing the syntax tree
            let ast = desmos_lang::parser::process_token(r.next().unwrap()).unwrap();
            println!("{:#?}", ast);
            match desmos_lang::compiler::compile_expr(&mut desmos_lang::compiler::Context {}, ast) {
                Err(e) => println!("{}", e),
                Ok(res) => println!("{:#?}", res),
            }
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
