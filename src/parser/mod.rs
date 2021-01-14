pub mod ast;
pub mod chars;
pub mod parser;

use nom_locate::LocatedSpan;
use nom_recursive::RecursiveInfo;

// Input type must implement trait HasRecursiveInfo
// nom_locate::LocatedSpan<T, RecursiveInfo> implements it.
pub type Span<'a> = LocatedSpan<&'a str, RecursiveInfo>;
