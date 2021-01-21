pub mod interpreter;
pub mod parser;

pub fn parse(inp: &str) {
    println!("{:#?}", parser::parser::parse(inp));
}
