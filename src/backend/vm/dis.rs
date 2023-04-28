use colored::Colorize;

use crate::prelude::SourceCache;

use super::{
    bytecode::{ByteCode, OpCode},
    chunk::Chunk,
    compiler::Script,
    value::{Function, Value},
};

pub struct Disassembler<'a, Data> {
    pub src: &'a SourceCache,
    pub entry: &'a Script<Data>,

    // keeps track of jumps to show on the dissasembly output
    jump_stack: Vec<usize>,
    output: String,
}

impl<'a, Data> Disassembler<'a, Data> {
    pub fn new(entry: &'a Script<Data>, src: &'a SourceCache) -> Self {
        Self {
            src,
            entry,
            output: String::new(),
            jump_stack: Vec::new(),
        }
    }

    pub fn disassemble(mut self) -> String {
        self.disassemble_function(&self.entry.function);
        self.output
    }

    pub fn disassemble_function(&mut self, function: &Function) {
        let (name, span) = &function.name;

        let lineno = self.src.get_lineno(span).unwrap_or(0);
        let filename = match self.src.get(span.2) {
            Some(file) => file.name.clone(),
            None => String::from("unknown"),
        };
        self.output.push_str(
            &format!(
                "\nDissasembly of <func {}, file \"{}\", line {}>:\n",
                name, filename, lineno
            )
            .cyan()
            .to_string(),
        );

        self.disassemble_chunk(&function.chunk);

        for constant in &function.chunk.constants {
            if let Value::Function(func) = constant {
                self.disassemble_function(func);
            }
        }
    }

    pub fn function_header(&mut self, function: &Function, closure: bool) -> String {
        let (name, span) = &function.name;
        let lineno = self.src.get_lineno(span).unwrap_or(0);
        let filename = match self.src.get(span.2) {
            Some(file) => file.name.clone(),
            None => String::from("unknown"),
        };

        let f = if closure { "closure" } else { "func" };
        let uv = if closure {
            format!(
                " ({} upvalue{})",
                function.upvalue_count,
                if function.upvalue_count > 1 { "s" } else { "" }
            )
        } else {
            String::new()
        };

        format!(
            "<{} {}{}, file \"{}\", line {}>",
            f, name, uv, filename, lineno
        )
        .cyan()
        .to_string()
    }

    pub fn disassemble_chunk(&mut self, chunk: &Chunk) {
        let mut offset = 0;
        let mut last_line = 0;

        while offset < chunk.ops.len() {
            let span = chunk.spans[offset];
            if let Some(line) = self.src.get_lineno(&span) {
                let mut arrow = "  ";

                if let Some(pos) = self.jump_stack.iter().position(|&x| x == offset) {
                    self.jump_stack.remove(pos);
                    arrow = ">>";
                }

                // line number
                if last_line != line {
                    if last_line != 0 {
                        self.output.push_str("\n");
                    }

                    self.output
                        .push_str(&format!("{}\t{}  ", line.to_string().red(), arrow.red()));
                } else {
                    self.output.push_str(&format!(
                        "{}\t{}  ",
                        " ".repeat(last_line.to_string().len()),
                        arrow.red()
                    ));
                }

                last_line = line;

                // offset and op
                self.output
                    .push_str(&format!("{:5}", offset.to_string().red()));

                self.disassemble_instruction(chunk, &mut offset);
            } else {
                offset += 1;
            }
        }
    }

    pub fn disassemble_instruction(&mut self, chunk: &Chunk, offset: &mut usize) {
        let op = OpCode::from(chunk.ops[*offset]);
        *offset += 1;

        self.output
            .push_str(&format!("{: <15}", format!("{:?}", op)).blue().to_string());

        match op {
            OpCode::LoadNone => {
                self.output
                    .push_str(format!("\t     ({})\n", "none".cyan()).as_str());
            }

            OpCode::BinaryAdd
            | OpCode::BinarySubtract
            | OpCode::BinaryMultiply
            | OpCode::BinaryDivide
            | OpCode::BinaryModulo
            | OpCode::BinaryPower
            | OpCode::BinaryEqual
            | OpCode::BinaryNotEqual
            | OpCode::BinaryLess
            | OpCode::BinaryLessEqual
            | OpCode::BinaryGreater
            | OpCode::BinaryGreaterEqual
            | OpCode::BinaryAnd
            | OpCode::BinaryOr
            | OpCode::BinaryJoin
            | OpCode::UnaryNegate
            | OpCode::UnaryNot
            | OpCode::UnarySpread => {
                let op = match op {
                    OpCode::BinaryAdd => "+",
                    OpCode::BinarySubtract => "-",
                    OpCode::BinaryMultiply => "*",
                    OpCode::BinaryDivide => "/",
                    OpCode::BinaryModulo => "%",
                    OpCode::BinaryPower => "**",
                    OpCode::BinaryEqual => "==",
                    OpCode::BinaryNotEqual => "!=",
                    OpCode::BinaryLess => "<",
                    OpCode::BinaryLessEqual => "<=",
                    OpCode::BinaryGreater => ">",
                    OpCode::BinaryGreaterEqual => ">=",
                    OpCode::BinaryAnd => "&&",
                    OpCode::BinaryOr => "||",
                    OpCode::BinaryJoin => "..",
                    OpCode::UnaryNegate => "-",
                    OpCode::UnaryNot => "!",
                    OpCode::UnarySpread => "...",
                    _ => unreachable!(),
                };

                self.output.push_str(&format!("\t     ({})\n", op));
            }

            OpCode::LoadConst | OpCode::CreateFunction => {
                let const_offset = chunk.ops.read_u16(*offset);
                *offset += 2;

                let constant = match chunk.get_constant(const_offset) {
                    Value::Function(func) => self.function_header(func, false),
                    constant => constant.display(),
                };

                self.output.push_str(&format!(
                    "\t{: <4} ({})\n",
                    const_offset.to_string().green(),
                    constant,
                ));
            }
            OpCode::DefineGlobal
            | OpCode::LoadLocal
            | OpCode::LoadGlobal
            | OpCode::SetLocal
            | OpCode::SetGlobal => {
                let local_offset = chunk.ops.read_u16(*offset);
                *offset += 2;

                let symbol = match op {
                    OpCode::DefineGlobal | OpCode::LoadGlobal | OpCode::SetGlobal => self
                        .entry
                        .global_symbols
                        .get(local_offset as usize)
                        .cloned()
                        .unwrap_or("unknown".to_string()),

                    _ => chunk.force_symbol(local_offset).to_string(),
                };

                self.output.push_str(&format!(
                    "\t{: <4} ({})\n",
                    local_offset.to_string().green(),
                    symbol.cyan(),
                ));
            }

            OpCode::CreateClosure => {
                let const_offset = chunk.ops.read_u16(*offset);
                *offset += 2;

                if let Value::Function(func) = chunk.get_constant(const_offset) {
                    let header = self.function_header(func, true);

                    for _ in 0..func.upvalue_count {
                        chunk.ops.read_u8(*offset);
                        *offset += 1;

                        chunk.ops.read_u16(*offset);
                        *offset += 2;
                    }

                    self.output.push_str(&format!(
                        "\t{: <3} ({})\n",
                        const_offset.to_string().green(),
                        header,
                    ));
                } else {
                    panic!("Expected function constant");
                }
            }

            OpCode::LoadUpvalue | OpCode::SetUpvalue | OpCode::PopMany => {
                let upvalue_offset = chunk.ops.read_u16(*offset);
                *offset += 2;

                self.output
                    .push_str(&format!("\t{}\n", upvalue_offset.to_string().green(),));
            }

            OpCode::CallFunction | OpCode::Unpack | OpCode::CreateArray | OpCode::LoadNoneMany => {
                let arg_count = chunk.ops.read_u8(*offset);
                *offset += 1;

                self.output
                    .push_str(&format!("\t{}\n", arg_count.to_string().green(),));
            }

            OpCode::CallBuiltin | OpCode::EnterContext => {
                let builtin_offset = chunk.ops.read_u16(*offset);
                *offset += 2;

                let arg_count = chunk.ops.read_u8(*offset);
                *offset += 1;

                self.output.push_str(&format!(
                    "\t{: <4} ({})\n",
                    arg_count.to_string().green(),
                    format!(
                        "<builtin @{}>",
                        self.entry
                            .global_symbols
                            .get(builtin_offset as usize)
                            .cloned()
                            .unwrap_or("unknown???".to_string()),
                    )
                    .cyan(),
                ));
            }

            OpCode::JumpIfFalse | OpCode::JumpForward | OpCode::JumpBackward | OpCode::IterNext => {
                let jump_offset = chunk.ops.read_u16(*offset);

                *offset += 2;

                // if let OpCode::IterNext = op {
                //     *offset += 2;
                // }

                let jump_addr = if let OpCode::JumpBackward = op {
                    *offset - jump_offset as usize
                } else {
                    jump_offset as usize + *offset
                };

                self.jump_stack.push(jump_addr);

                let jump = if jump_addr < *offset {
                    format!(">> {}", jump_addr).cyan()
                } else {
                    format!(">> {}", jump_addr).red()
                };

                self.output.push_str(&format!(
                    "\t{: <4} {}\n",
                    jump_offset.to_string().green(),
                    jump
                ));
            }

            _ => {
                self.output.push_str("\n");
            }
        }
    }
}
