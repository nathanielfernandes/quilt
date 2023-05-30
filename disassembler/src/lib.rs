use bytecode::bytecode::*;
use bytecode::chunk::Chunk;
use colored::Colorize;
use common::sourcecache::SourceCache;
use interpreter::{
    value::{Function, Value},
    Script,
};

pub struct Disassembler<'a> {
    pub src: &'a SourceCache,
    pub entry: &'a Script,

    // keeps track of jumps to show on the dissasembly output
    jump_stack: Vec<usize>,
    output: String,
}

impl<'a> Disassembler<'a> {
    pub fn new(entry: &'a Script, src: &'a SourceCache) -> Self {
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

    pub fn disassemble_chunk(&mut self, chunk: &Chunk<Value>) {
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

    pub fn disassemble_instruction(&mut self, chunk: &Chunk<Value>, offset: &mut usize) {
        let op = chunk.ops[*offset];
        *offset += 1;

        self.output
            .push_str(&format!("{: <15}", op.name()).blue().to_string());

        #[allow(non_upper_case_globals)]
        match op {
            LoadNone => {
                self.output
                    .push_str(format!("\t     ({})\n", "none".cyan()).as_str());
            }

            BinaryAdd | BinarySubtract | BinaryMultiply | BinaryDivide | BinaryModulo
            | BinaryPower | BinaryEqual | BinaryNotEqual | BinaryLess | BinaryLessEqual
            | BinaryGreater | BinaryGreaterEqual | BinaryAnd | BinaryOr | BinaryJoin
            | UnaryNegate | UnaryNot | UnarySpread => {
                let op = match op {
                    BinaryAdd => "+",
                    BinarySubtract => "-",
                    BinaryMultiply => "*",
                    BinaryDivide => "/",
                    BinaryModulo => "%",
                    BinaryPower => "**",
                    BinaryEqual => "==",
                    BinaryNotEqual => "!=",
                    BinaryLess => "<",
                    BinaryLessEqual => "<=",
                    BinaryGreater => ">",
                    BinaryGreaterEqual => ">=",
                    BinaryAnd => "&&",
                    BinaryOr => "||",
                    BinaryJoin => "..",
                    UnaryNegate => "-",
                    UnaryNot => "!",
                    UnarySpread => "...",
                    _ => unreachable!(),
                };

                self.output.push_str(&format!("\t     ({})\n", op));
            }

            LoadConst | CreateFunction => {
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
            DefineGlobal | LoadLocal | LoadGlobal | SetLocal | SetGlobal => {
                let local_offset = chunk.ops.read_u16(*offset);
                *offset += 2;

                let symbol = match op {
                    DefineGlobal | LoadGlobal | SetGlobal => self
                        .entry
                        .global_symbols
                        .get(local_offset)
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

            CreateClosure => {
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

            LoadUpvalue | SetUpvalue | PopMany => {
                let upvalue_offset = chunk.ops.read_u16(*offset);
                *offset += 2;

                self.output
                    .push_str(&format!("\t{}\n", upvalue_offset.to_string().green(),));
            }

            CallFunction | Unpack | CreateArray | LoadNoneMany => {
                let arg_count = chunk.ops.read_u8(*offset);
                *offset += 1;

                self.output
                    .push_str(&format!("\t{}\n", arg_count.to_string().green(),));
            }

            CallBuiltin | EnterContext => {
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
                            .get(builtin_offset)
                            .cloned()
                            .unwrap_or("unknown???".to_string()),
                    )
                    .cyan(),
                ));
            }

            JumpIfFalse | JumpForward | JumpBackward | IterNext => {
                let jump_offset = chunk.ops.read_u16(*offset);

                *offset += 2;

                // if let IterNext = op {
                //     *offset += 2;
                // }

                let jump_addr = if let JumpBackward = op {
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

pub trait View {
    fn display(&self) -> String;
}

impl View for Value {
    fn display(&self) -> String {
        match self {
            Value::None => "none".cyan().to_string(),
            Value::Bool(b) => b.to_string().yellow().to_string(),
            Value::Int(i) => i.to_string().yellow().to_string(),
            Value::Float(f) => f.to_string().yellow().to_string(),
            Value::Range(l, r) => {
                format!("{}:{}", l.to_string().yellow(), r.to_string().yellow())
            }
            Value::Color(c) => format!("#{:02x}{:02x}{:02x}{:02x}", c[0], c[1], c[2], c[3]),
            Value::String(s) => format!("\"{}\"", s).green().to_string(),
            Value::Array(l) => {
                if l.len() <= 10 {
                    format!(
                        "[{}]",
                        l.iter().map(|c| c.display()).collect::<Vec<_>>().join(", ")
                    )
                } else {
                    format!(
                        "[{}, {}, {}, {}, {}, {}, {}, {}, {}, ...]",
                        l[0].display(),
                        l[1].display(),
                        l[2].display(),
                        l[3].display(),
                        l[4].display(),
                        l[5].display(),
                        l[6].display(),
                        l[7].display(),
                        l[8].display(),
                    )
                }
            }
            Value::Pair(l) => format!("({}, {})", l.0.display(), l.1.display()),
            // Value::Spread(s) => format!(
            //     "...[{}]",
            //     s.iter().map(|c| c.display()).collect::<Vec<_>>().join(", ")
            // ),
            Value::Special(spec) => format!("<{} id={}>", spec.0, spec.1),
            Value::Function(f) => format!("<func {}>", f.name.0).cyan().to_string(),
            Value::Closure(c) => format!("<closure {}>", c.function.name.0)
                .cyan()
                .to_string(),

            Value::LoopCtx(idx) => format!("<loop_ctx {}>", idx).cyan().to_string(),
        }
    }
}
