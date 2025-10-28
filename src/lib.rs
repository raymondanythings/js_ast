pub mod ast;
pub mod lexer;
pub mod parser;
pub mod visitor;

pub use crate::ast::*;
pub use crate::lexer::*;
pub use crate::parser::{ParseError, Parser, parse_program};
pub use crate::visitor::{Visitor, walk_program};

pub fn program_to_json(program: &Program) -> serde_json::Result<String> {
    serde_json::to_string_pretty(program)
}

pub fn program_to_value(program: &Program) -> serde_json::Result<serde_json::Value> {
    serde_json::to_value(program)
}
