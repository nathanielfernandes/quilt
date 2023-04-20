use crate::{backend::utils::rlevec::RleVec, prelude::Span};

use super::{
    bytecode::{ByteCode, OpCode},
    value::Value,
};

#[derive(Debug, Clone, PartialEq)]
pub struct Chunk {
    pub ops: Vec<u8>,
    pub spans: RleVec<Span>,
    pub constants: Vec<Value>,
    pub symbols: Vec<String>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            ops: Vec::new(),
            spans: RleVec::new(),
            constants: Vec::new(),
            symbols: Vec::new(),
        }
    }

    #[inline]
    pub fn write_op(&mut self, op: OpCode, span: Span) {
        self.ops.write_op(op);
        self.spans.push(span);
    }

    #[inline]
    pub fn write_u8(&mut self, value: u8, span: Span) {
        self.ops.write_u8(value);
        self.spans.push(span);
    }

    #[inline]
    pub fn write_u16(&mut self, value: u16, span: Span) {
        self.ops.write_u16(value);
        self.spans.push_n(span, 2);
    }

    #[inline]
    pub fn patch_u8(&mut self, index: usize, value: u8) {
        self.ops.patch_u8(index, value);
    }

    #[inline]
    pub fn patch_op(&mut self, index: usize, op: OpCode) {
        self.ops.patch_op(index, op);
    }

    #[inline]
    pub fn patch_u16(&mut self, index: usize, value: u16) {
        self.ops.patch_u16(index, value);
    }

    #[inline]
    pub fn add_constant(&mut self, constant: Value) {
        self.constants.push(constant);
    }

    #[inline]
    pub fn add_symbol(&mut self, symbol: String) {
        self.symbols.push(symbol);
    }

    #[inline]
    pub fn get_constant(&self, index: u16) -> &Value {
        &self.constants[index as usize]
    }

    #[inline]
    pub fn get_symbol(&self, index: u16) -> &String {
        &self.symbols[index as usize]
    }

    #[inline]
    pub fn force_symbol(&self, index: u16) -> &str {
        self.symbols
            .get(index as usize)
            .map(|s| s.as_str())
            .unwrap_or("unknown")
    }

    #[inline]
    pub fn get_span(&self, index: usize) -> Span {
        *self.spans.get(index).unwrap_or(&Span::default())
    }

    #[inline]
    pub fn last_span(&self) -> Span {
        *self.spans.last().unwrap_or(&Span::default())
    }

    #[inline]
    pub fn first_span(&self) -> Span {
        *self.spans.first().unwrap_or(&Span::default())
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.ops.len()
    }
}
