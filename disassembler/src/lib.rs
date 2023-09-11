pub mod structure;

use bytecode::bytecode::*;
use bytecode::chunk::Chunk;

use common::sourcecache::SourceCache;
use interpreter::{
    value::{Function, Value},
    Script,
};
use structure::{FunctionDisassembly, Hint, Line};

pub struct Disassembler<'a> {
    pub src: &'a SourceCache,
    pub main: &'a Script,

    // keeps track of jumps to show on the dissasembly output
    jump_stack: Vec<usize>,
    output: Vec<FunctionDisassembly>,

    // keeps track of the current function being disassembled
    idx: usize,
}

impl<'a> Disassembler<'a> {
    pub fn new(main: &'a Script, src: &'a SourceCache) -> Self {
        Self {
            src,
            main,
            output: Vec::new(),
            jump_stack: Vec::new(),
            idx: 0,
        }
    }

    fn extract_fn(&self, function: &Function) -> (String, usize, String) {
        let (name, span) = &function.name;

        let line_number = self.src.get_lineno(span).unwrap_or(0);
        let filename = match self.src.get(span.2) {
            Some(file) => file.name.clone(),
            None => String::from("unknown"),
        };

        (name.clone(), line_number, filename)
    }

    pub fn disassemble(mut self) -> Vec<FunctionDisassembly> {
        self.disassemble_function(&self.main.function);
        self.output
    }

    pub fn disassemble_function(&mut self, function: &Function) {
        let (name, line_number, filename) = self.extract_fn(function);

        let function_dis = FunctionDisassembly {
            name,
            line_number,
            filename,
            disassembly: Vec::new(),
        };

        self.output.push(function_dis);
        self.disassemble_chunk(&function.chunk);

        for constant in &function.chunk.constants {
            if let Value::Function(func) = constant {
                self.idx += 1;
                self.disassemble_function(func);
            }
        }
    }

    fn disassemble_chunk(&mut self, chunk: &Chunk<Value>) {
        let mut offset = 0;

        while offset < chunk.ops.len() {
            let span = chunk.spans.get(offset).cloned().unwrap_or_default();

            if let Some(line_number) = self.src.get_lineno(&span) {
                let is_jump = if let Some(pos) = self.jump_stack.iter().position(|&x| x == offset) {
                    self.jump_stack.remove(pos);
                    true
                } else {
                    false
                };

                let opcode = chunk.ops[offset];
                let mut line = Line {
                    line_number,
                    span: (span.0, span.1),
                    is_jump,
                    offset,
                    opcode: opcode.name(),
                    oparg: None,
                    hint: None,
                };

                offset += 1;

                self.disassemble_op(opcode, chunk, &mut offset, &mut line);

                if let Some(function_dis) = self.output.get_mut(self.idx) {
                    function_dis.disassembly.push(line);
                }
            } else {
                offset += 1;
            }
        }
    }

    fn disassemble_op(
        &mut self,
        op: u8,
        chunk: &Chunk<Value>,
        offset: &mut usize,
        line: &mut Line,
    ) {
        #[allow(non_upper_case_globals)]
        match op {
            LoadNone => {
                line.hint = Some(Hint::None);
            }

            LoadConst | CreateFunction => {
                let const_offset = chunk.ops.read_u16(*offset);
                *offset += 2;

                let constant = chunk.get_constant(const_offset);

                line.oparg = Some(const_offset);
                line.hint = Some(self.create_hint(constant));
            }

            DefineGlobal | LoadGlobal | SetGlobal => {
                let global_offset = chunk.ops.read_u16(*offset);
                *offset += 2;

                let symbol = self
                    .main
                    .global_symbols
                    .get(global_offset)
                    .cloned()
                    .unwrap_or("unknown".to_string());

                line.oparg = Some(global_offset);
                line.hint = Some(Hint::Symbol(symbol));
            }

            LoadLocal | SetLocal => {
                let local_offset = chunk.ops.read_u16(*offset);
                *offset += 2;

                let symbol = chunk.force_symbol(local_offset).to_string();

                line.oparg = Some(local_offset);
                line.hint = Some(Hint::Symbol(symbol));
            }

            CreateClosure => {
                let const_offset = chunk.ops.read_u16(*offset);
                *offset += 2;

                if let Value::Function(func) = chunk.get_constant(const_offset) {
                    let (name, line_number, filename) = self.extract_fn(func);

                    for _ in 0..func.upvalue_count {
                        chunk.ops.read_u8(*offset);
                        *offset += 1;

                        chunk.ops.read_u16(*offset);
                        *offset += 2;
                    }

                    line.oparg = Some(const_offset);
                    line.hint = Some(Hint::Closure {
                        name,
                        filename,
                        line_number,
                        num_upvalues: func.upvalue_count,
                    });
                }
            }

            LoadUpvalue | SetUpvalue | PopMany => {
                let upvalue_offset = chunk.ops.read_u16(*offset);
                *offset += 2;

                line.oparg = Some(upvalue_offset);
            }

            CallFunction | Unpack | CreateArray | LoadNoneMany => {
                let arg_count = chunk.ops.read_u8(*offset);
                *offset += 1;

                line.oparg = Some(arg_count as u16);
            }

            CallBuiltin | EnterContext => {
                let builtin_offset = chunk.ops.read_u16(*offset);
                *offset += 2;

                let arg_count = chunk.ops.read_u8(*offset);
                *offset += 1;

                line.oparg = Some(arg_count as u16);
                line.hint = Some(Hint::Builtin(
                    self.main
                        .global_symbols
                        .get(builtin_offset)
                        .cloned()
                        .unwrap_or("unknown".to_string()),
                ));
            }

            JumpIfFalse | JumpForward | JumpBackward | JumpIfNotEq | IterNext => {
                let jump_offset = chunk.ops.read_u16(*offset);
                *offset += 2;

                let jump_addr = if let JumpBackward = op {
                    *offset - jump_offset as usize
                } else {
                    jump_offset as usize + *offset
                };

                self.jump_stack.push(jump_addr);

                line.oparg = Some(jump_offset);
                line.hint = Some(Hint::Jump(jump_addr));
            }

            // operations
            BinaryAdd => {
                line.hint = Some(Hint::Op("+"));
            }
            BinarySubtract => {
                line.hint = Some(Hint::Op("-"));
            }
            BinaryMultiply => {
                line.hint = Some(Hint::Op("*"));
            }
            BinaryDivide => {
                line.hint = Some(Hint::Op("/"));
            }
            BinaryModulo => {
                line.hint = Some(Hint::Op("%"));
            }
            BinaryPower => {
                line.hint = Some(Hint::Op("**"));
            }
            BinaryEqual => {
                line.hint = Some(Hint::Op("=="));
            }
            BinaryNotEqual => {
                line.hint = Some(Hint::Op("!="));
            }
            BinaryLess => {
                line.hint = Some(Hint::Op("<"));
            }
            BinaryLessEqual => {
                line.hint = Some(Hint::Op("<="));
            }
            BinaryGreater => {
                line.hint = Some(Hint::Op(">"));
            }
            BinaryGreaterEqual => {
                line.hint = Some(Hint::Op(">="));
            }
            BinaryAnd => {
                line.hint = Some(Hint::Op("&&"));
            }
            BinaryOr => {
                line.hint = Some(Hint::Op("||"));
            }
            BinaryJoin => {
                line.hint = Some(Hint::Op(".."));
            }
            UnaryNegate => {
                line.hint = Some(Hint::Op("-"));
            }
            UnaryNot => {
                line.hint = Some(Hint::Op("!"));
            }
            UnarySpread => {
                line.hint = Some(Hint::Op("..."));
            }

            BitwiseAnd => {
                line.hint = Some(Hint::Op("&"));
            }
            BitwiseOr => {
                line.hint = Some(Hint::Op("|"));
            }
            BitwiseXor => {
                line.hint = Some(Hint::Op("^"));
            }
            BitwiseLeftShift => {
                line.hint = Some(Hint::Op("<<"));
            }
            BitwiseRightShift => {
                line.hint = Some(Hint::Op(">>"));
            }
            BitwiseNot => {
                line.hint = Some(Hint::Op("~"));
            }

            _ => {}
        }
    }

    fn create_hint(&self, value: &Value) -> Hint {
        match value {
            Value::None => Hint::None,
            Value::Bool(bool) => Hint::Bool(*bool),
            Value::Int(int) => Hint::Int(*int),
            Value::Float(float) => Hint::Float(*float),
            Value::Range(l, r) => Hint::Range(*l, *r),
            Value::Color(c) => Hint::Color(*c),
            Value::String(s) => Hint::String(s.to_string()),
            Value::Special(s) => Hint::Special {
                r#type: s.0,
                id: s.1,
            },
            Value::Array(a) => Hint::Array(a.iter().map(|v| self.create_hint(v)).collect()),
            Value::Pair(p) => Hint::Pair(
                Box::new(self.create_hint(&p.0)),
                Box::new(self.create_hint(&p.1)),
            ),
            Value::Function(f) => {
                let (name, line_number, filename) = self.extract_fn(f);

                Hint::Function {
                    name,
                    filename,
                    line_number,
                }
            }
            Value::Closure(c) => {
                let (name, line_number, filename) = self.extract_fn(&c.function);

                Hint::Closure {
                    name,
                    filename,
                    line_number,
                    num_upvalues: c.upvalues.len() as u16,
                }
            }

            _ => Hint::None,
        }
    }
}
