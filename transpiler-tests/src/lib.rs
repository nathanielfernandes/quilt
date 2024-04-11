use common::span::{Span, Spanned};
use parser::{
    literal::Literal,
    node::{Node, NodeType},
};

#[repr(C)]
pub enum ValueType {
    INT = 0,
    BOOL = 1,
    STRING = 2,
}

#[repr(C)]
union ValueC {
    int: i64,
    bool: bool,
    string: *const u8,
}

#[repr(C)]
pub struct Value {
    value_type: ValueType,
    value: ValueC,
}

pub struct Transpiler {}

impl Transpiler {
    pub fn new() -> Self {
        Self {}
    }

    pub fn transpile_literal(&mut self, literal: &Literal) -> String {
        let s = match literal {
            Literal::String(value) => format!("STRING({:?})", value),
            Literal::Int(value) => format!("INT({})", value),
            Literal::Bool(value) => format!("BOOL({})", value),
            _ => todo!(),
        };

        format!("ALLOCATE({})", s)
    }

    pub fn transpile_expression(&mut self, (expr, span): &Spanned<Node>) -> String {
        match expr {
            Node::Literal(literal) => self.transpile_literal(literal),
            Node::BuiltinCall((name, name_span), args) => {
                let args = args
                    .iter()
                    .map(|arg| self.transpile_expression(arg))
                    .collect::<Vec<_>>()
                    .join(", ");

                format!("{}({})", name, args)
            }

            _ => todo!(),
        }
    }

    pub fn transpile_statement(&mut self, stmt: &Spanned<Node>) -> String {
        if stmt.0.ntype() == NodeType::Expression {
            format!("{s};", s = self.transpile_expression(stmt))
        } else {
            todo!()
        }
    }

    pub fn transpile(mut self, ast: Vec<Spanned<Node>>) -> String {
        let statements = ast
            .iter()
            .map(|stmt| self.transpile_statement(stmt))
            .collect::<Vec<_>>()
            .join("\n");

        let template = include_str!("template.c");

        template.replace("//_main_//", &statements)
    }
}
