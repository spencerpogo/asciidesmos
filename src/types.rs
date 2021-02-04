use std::fmt;

#[derive(Debug, Clone)]
pub struct AssertionError;

// Generation of an error is completely separate from how it is displayed.
// There's no need to be concerned about cluttering complex logic with the display style.
//
// Note that we don't store any extra info about the errors. This means we can't state
// which string failed to parse without modifying our types to carry that information.
impl fmt::Display for AssertionError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "assertion error while processing tokens")
    }
}

// Expression is a component of a statement
#[derive(Clone, Debug, PartialEq)]
pub enum Expression<'a> {
    Num {
        val: &'a str,
    },
    Variable {
        val: &'a str,
    },
    BinaryExpr {
        left: Box<Expression<'a>>,
        // Should probably make an enum for this, but its not worth the work to encode
        //  it just to stringify it again later
        operator: &'a str,
        right: Box<Expression<'a>>,
    },
    UnaryExpr {
        val: Box<Expression<'a>>,
        operator: &'a str,
    },
}
