use std::fmt::Write;

use colored::Colorize;

#[derive(Debug, Clone, serde::Serialize)]
pub enum Hint {
    Jump(usize),

    None,
    Bool(bool),
    Int(i64),
    Float(f64),
    Range(i32, i32),
    String(String),
    Array(Vec<Hint>),
    Pair(Box<Hint>, Box<Hint>),
    Color([u8; 4]),
    Special {
        r#type: &'static str,
        id: usize,
    },
    Function {
        name: String,
        filename: String,
        line_number: usize,
    },
    Closure {
        name: String,
        filename: String,
        line_number: usize,
        num_upvalues: u16,
    },
    Builtin(String),

    Symbol(String),
    Op(&'static str),
}

#[derive(Debug, Clone, serde::Serialize)]
pub struct Line {
    pub line_number: usize,
    pub span: (usize, usize),
    pub is_jump: bool,
    pub offset: usize,
    pub opcode: &'static str,
    pub oparg: Option<u16>,
    pub hint: Option<Hint>,
}

#[derive(Debug, Clone, serde::Serialize)]
pub struct FunctionDisassembly {
    pub name: String,
    pub line_number: usize,
    pub filename: String,
    pub disassembly: Vec<Line>,
}

impl std::fmt::Display for Hint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Hint::Jump(j) => f.write_str(&format!(">> {}", j).red().to_string()),

            Hint::None => f.write_str(&"none".cyan().to_string()),
            Hint::Bool(b) => f.write_str(&format!("{}", b).yellow().to_string()),
            Hint::Int(i) => f.write_str(&i.to_string().yellow().to_string()),
            Hint::Float(i) => f.write_str(&i.to_string().yellow().to_string()),
            Hint::Range(a, b) => f.write_str(
                &format!("{}:{}", a.to_string().yellow(), b.to_string().yellow()).to_string(),
            ),
            Hint::Color(c) => f.write_str(&format!(
                "#{:02x}{:02x}{:02x}{:02x}",
                c[0], c[1], c[2], c[3]
            )),
            Hint::String(s) => f.write_str(&format!("{:?}", s).green()),
            Hint::Array(l) => {
                let s = if l.len() <= 10 {
                    format!(
                        "[{}]",
                        l.iter()
                            .map(|c| c.to_string())
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                } else {
                    format!(
                        "[{}, {}, {}, {}, {}, {}, {}, {}, {}, ...]",
                        l[0], l[1], l[2], l[3], l[4], l[5], l[6], l[7], l[8],
                    )
                };

                f.write_str(&s)
            }
            Hint::Pair(left, right) => f.write_str(&format!("({}, {})", left, right)),
            Hint::Special { r#type, id } => f.write_str(&format!("<{} id={}>", r#type, id)),
            Hint::Function {
                name,
                filename,
                line_number,
            } => f.write_str(
                &format!(
                    "<func {}, file \"{}\", line {}>",
                    name.green(),
                    filename,
                    line_number
                )
                .cyan()
                .to_string(),
            ),
            Hint::Closure {
                name,
                filename,
                line_number,
                num_upvalues,
            } => f.write_str(
                &format!(
                    "<closure {}, file \"{}\", line {}, (upvalues={})>",
                    name, filename, line_number, num_upvalues
                )
                .cyan(),
            ),
            Hint::Builtin(name) => f.write_str(&format!("<builtin {}>", name).cyan().to_string()),

            Hint::Symbol(s) => f.write_str(&s.cyan().to_string()),
            Hint::Op(s) => write!(f, "{}", s),
        }
    }
}

impl Line {
    fn display(&self, last_line: usize) -> String {
        let mut s = String::new();

        if last_line != self.line_number {
            s.push_str(&format!("{: <4}   ", self.line_number).red().to_string());
        } else {
            s.push_str(&"       ".red().to_string());
        }

        if self.is_jump {
            s.push_str(&">>  ".red().to_string());
        } else {
            s.push_str(&"    ".red().to_string());
        }

        s.push_str(&format!("{: <4} ", self.offset).red().to_string());
        s.push_str(&format!("{: <15} ", self.opcode).blue().to_string());

        if let Some(oparg) = self.oparg {
            s.push_str(&format!("{: <4} ", oparg).green().to_string());
        } else {
            s.push_str("     ")
        }

        if let Some(hint) = &self.hint {
            if let Hint::Jump(offset) = hint {
                if *offset as usize > self.offset {
                    s.push_str(&format!(">> {}", offset).red().to_string());
                } else {
                    s.push_str(&format!("<< {}", offset).red().to_string());
                }
            } else {
                s.push_str(&format!("({})", hint));
            }
        }

        s.push('\n');

        s
    }
}

impl std::fmt::Display for Line {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.display(0))
    }
}

impl std::fmt::Display for FunctionDisassembly {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(
            &format!(
                "Disassembly of <func {}, file \"{}\", line {}>\n",
                self.name.green(),
                self.filename,
                self.line_number
            )
            .cyan()
            .to_string(),
        )?;

        let mut skip = true;
        let mut last_line = 0;
        for line in &self.disassembly {
            if line.line_number != last_line && !skip {
                f.write_char('\n')?;
            }

            f.write_str(&line.display(last_line))?;

            last_line = line.line_number;
            skip = false;
        }

        Ok(())
    }
}
