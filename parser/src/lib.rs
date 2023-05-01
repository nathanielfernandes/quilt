use common::{
    error::{Error, ErrorS, SyntaxError},
    span::{Span, Spanned},
};
use literal::Literal;
use node::Node;
use peg::{error::ParseError, str::LineCol};

pub mod includes;
pub mod literal;
pub mod node;
pub mod parser;

#[inline]
fn syntax_error(err: ParseError<LineCol>, src_id: usize) -> ErrorS {
    let line = err.location.line;
    let column = err.location.column;
    let offset = err.location.offset;

    let mut expected: Vec<&str> = err.expected.tokens().collect();
    expected.sort();

    (
        SyntaxError::UnexpectedToken {
            line,
            column,
            expected,
        }
        .into(),
        Span(offset, offset, src_id),
    )
}

pub fn parse_literal(literal: &str) -> Result<Literal, Error> {
    match parser::parser::literal(literal, 0) {
        Ok(l) => Ok(l),
        Err(e) => Err(syntax_error(e, 0).0),
    }
}

pub fn parse_code(code: &str, src_id: usize) -> Result<Vec<Spanned<Node>>, ErrorS> {
    match parser::parser::code(code, src_id) {
        Ok(ast) => Ok(ast),
        Err(e) => Err(syntax_error(e, src_id)),
    }
}
