pub mod parser;

/// Type-erased errors.
pub type BoxError = std::boxed::Box<
    dyn std::error::Error + std::marker::Send + std::marker::Sync, // needed for threads
>;

pub fn parse(inp: &str) {
    println!("{:#?}", parser::parser::parse(inp));
}
