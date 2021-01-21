use clap::{App, Arg};
use desmos_lang;

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
                .short("ast")
                .help("Prints parsed AST. Useful for debugging."),
        );

    let matches = app.get_matches();

    if let Some(input) = matches.value_of("eval") {
        println!("Will eval: {:#?}", input);
    } else if let Some(filename) = matches.value_of("file") {
        println!("Will read from {:#?}", filename);
    } else {
        println!("Will read from stdin");
    }
}
