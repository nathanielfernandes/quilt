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

    output: String,
}

impl<'a, Data> Disassembler<'a, Data> {
    pub fn new(entry: &'a Script<Data>, src: &'a SourceCache) -> Self {
        Self {
            src,
            entry,
            output: String::new(),
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
                // line number
                if last_line != line {
                    if last_line != 0 {
                        self.output.push_str("\n");
                    }

                    self.output
                        .push_str(&format!("{}\t", line.to_string().red()));
                } else {
                    self.output.push_str("\t");
                }

                last_line = line;

                // offset and op
                self.output
                    .push_str(&format!("{:4}", offset.to_string().red()));

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
            OpCode::LoadConst | OpCode::CreateFunction => {
                let const_offset = chunk.ops.read_u16(*offset);
                *offset += 2;

                let constant = match chunk.get_constant(const_offset) {
                    Value::Function(func) => self.function_header(func, false),
                    constant => constant.display(),
                };

                self.output.push_str(&format!(
                    "\t{: <3} ({})\n",
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
                    "\t{: <3} ({})\n",
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

            OpCode::LoadUpvalue | OpCode::SetUpvalue => {
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
                    "\t{: <3} ({})\n",
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

            OpCode::JumpIfFalse
            | OpCode::JumpForward
            | OpCode::JumpBackward
            | OpCode::IterNext
            | OpCode::PopMany => {
                let jump_offset = chunk.ops.read_u16(*offset);
                *offset += 2;

                if let OpCode::IterNext = op {
                    *offset += 2;
                }

                self.output
                    .push_str(&format!("\t{}\n", jump_offset.to_string().green(),));
            }

            _ => {
                self.output.push_str("\n");
            }
        }
    }
}
