use crate::{
    frontend::{Expr, Op},
    shared::{Spanned, Value},
};

use colored::Colorize;

const SPACE: usize = 4;
fn sep(depth: usize) -> String {
    " ".repeat(depth * SPACE)
}

fn bracket(br: &str, depth: usize) -> String {
    if (depth + 1) % 2 == 0 {
        br.magenta()
    } else if (depth + 1) % 3 == 0 {
        br.cyan()
    } else {
        br.yellow()
    }
    .to_string()
}

fn list_block(args: &Vec<Spanned<Expr>>, depth: usize, br_depth: usize) -> String {
    args.iter()
        .map(|a| format!("{}{}", sep(depth), a.0.to_string(depth, br_depth)))
        .collect::<Vec<String>>()
        .join("\n")
}

impl Op {
    pub fn to_string(&self) -> String {
        match self {
            Op::Add => String::from("+"),
            Op::Sub => String::from("-"),
            Op::Mul => String::from("*"),
            Op::Div => String::from("/"),
            Op::Mod => String::from("%"),
            Op::Pow => String::from("^"),
            Op::Eq => String::from("=="),
            Op::Neq => String::from("!="),
            Op::Lt => String::from("<"),
            Op::Gt => String::from(">"),
            Op::Lte => String::from("<="),
            Op::Gte => String::from(">="),
            Op::And => String::from("&&"),
            Op::Or => String::from("||"),
            Op::Not => String::from("!"),
            Op::Neg => String::from("-"),
            Op::Join => String::from(".."),
            Op::Spread => String::from("..."),
        }
    }
}

impl Expr {
    pub fn to_string(&self, depth: usize, br_depth: usize) -> String {
        match &self {
            Expr::Literal(l) => match l {
                Value::None => String::from("none").yellow().to_string(),
                Value::Bool(b) => {
                    if *b {
                        String::from("true").yellow().to_string()
                    } else {
                        String::from("false").yellow().to_string()
                    }
                }
                Value::Int(n) => n.to_string().yellow().to_string(),
                Value::Float(n) => n.to_string().yellow().to_string(),
                Value::Str(n) => format!("\"{}\"", n).green().to_string(),
                Value::Color([r, g, b, a]) => {
                    format!(
                        "{} {}",
                        format!("#{:02x}{:02x}{:02x}{:02x}", r, g, b, a).cyan(),
                        format!("[{}, {}, {}, {}]", r, g, b, a).dimmed()
                    )
                }
                Value::List(_) => {
                    format!("{}...{}", bracket("[", br_depth), bracket("]", br_depth))
                }
                Value::Pair(left, right) => format!(
                    "{}{}, {}{}",
                    bracket("(", br_depth),
                    left.to_string(),
                    right.to_string(),
                    bracket(")", br_depth)
                ),
                Value::Spread(value) => {
                    format!("{}{:?}", "...".purple(), value)
                }
                Value::Special(s, id) => {
                    format!("<special id={} value={}>", s, id)
                }
                Value::Range(start, end) => {
                    format!("{}{}{}", start, ":", end)
                }
            },
            Expr::List(list) => format!(
                "{}{}{}",
                bracket("[", br_depth),
                list.iter()
                    .map(|a| a.0.to_string(depth, br_depth + 1))
                    .collect::<Vec<String>>()
                    .join(", "),
                bracket("]", br_depth)
            ),
            // Expr::RangeLoop(name, start, end, block) => {
            //     let mut s = format!(
            //         "{} {} {} {} {} {}",
            //         "for".purple(),
            //         name.0.red().to_string(),
            //         "in".purple(),
            //         start.0.to_string(depth, br_depth),
            //         "to".purple(),
            //         end.0.to_string(depth, br_depth)
            //     );

            //     if block.len() == 1 {
            //         s.push_str(&format!(
            //             " {} {} {}",
            //             bracket("{", br_depth),
            //             block[0].0.to_string(depth, br_depth + 1),
            //             bracket("}", br_depth)
            //         ));
            //     } else {
            //         s.push_str(&format!(
            //             " {} {} {}",
            //             bracket("{", br_depth),
            //             list_block(&block, depth + 1, br_depth + 1),
            //             bracket("}", br_depth)
            //         ));
            //     }

            //     s
            // }
            Expr::Import(path) => {
                format!("{} {}", "import".purple(), path.0.green().to_string())
            }
            Expr::Pair(left, right) => format!(
                "{}{}, {}{}",
                bracket("(", br_depth),
                left.0.to_string(depth, br_depth + 1),
                right.0.to_string(depth, br_depth + 1),
                bracket(")", br_depth)
            ),
            Expr::Ident(ident) => ident.red().to_string(),
            Expr::Yoink(ident) => format!("{}{}", "$".purple(), ident.red().to_string()),
            Expr::Declaration(name, value) => format!(
                "{} {} = {}",
                "let".purple(),
                name.0.red().to_string(),
                value.0.to_string(depth, br_depth)
            ),
            Expr::MultiDeclaration(names, value) => format!(
                "{} [{}] = {}",
                "let".purple(),
                names
                    .iter()
                    .map(|n| n.0.red().to_string())
                    .collect::<Vec<String>>()
                    .join(", "),
                value.0.to_string(depth, br_depth)
            ),
            Expr::Assignment(name, value) => format!(
                "{} = {}",
                name.0.red().to_string(),
                value.0.to_string(depth, br_depth)
            ),
            Expr::Binary(op, lhs, rhs) => format!(
                "{}{} {} {}{}",
                bracket("(", br_depth),
                lhs.0.to_string(depth, br_depth + 1),
                op.to_string(),
                rhs.0.to_string(depth, br_depth + 1),
                bracket(")", br_depth)
            ),
            Expr::Unary(op, value) => {
                format!("{}{}", op.to_string(), value.0.to_string(depth, br_depth),)
            }
            Expr::Function(name, args, body) => {
                let mut s = format!(
                    "{} {}{}{}{}",
                    "fn".purple(),
                    name.0.blue().to_string(),
                    bracket("(", br_depth),
                    args.into_iter()
                        .map(|a| a.0.clone())
                        .collect::<Vec<_>>()
                        .join(", ")
                        .red()
                        .to_string(),
                    bracket(")", br_depth)
                );

                if body.len() == 1 {
                    s.push_str(&format!(
                        " {} {} {}",
                        bracket("{", br_depth),
                        body[0].0.to_string(depth, br_depth + 1),
                        bracket("}", br_depth)
                    ));
                } else {
                    s.push_str(&format!(
                        " {}\n{}\n{}{}",
                        bracket("{", br_depth),
                        list_block(body, depth + 1, br_depth + 1),
                        sep(depth),
                        bracket("}", br_depth)
                    ));
                }

                s
            }
            Expr::Call(name, args) => format!(
                "{}{}{}{}",
                name.0.blue().to_string(),
                bracket("(", br_depth),
                args.iter()
                    .map(|a| a.0.to_string(depth, br_depth))
                    .collect::<Vec<String>>()
                    .join(", "),
                bracket(")", br_depth)
            ),
            Expr::BuiltinCall(name, args) => format!(
                "{}{}{}{}{}",
                "@".bright_blue().to_string(),
                name.0.bright_blue().to_string(),
                bracket("(", br_depth),
                args.iter()
                    .map(|a| a.0.to_string(depth, br_depth))
                    .collect::<Vec<String>>()
                    .join(", "),
                bracket(")", br_depth)
            ),
            Expr::Conditional(cond, then, otherwise) => {
                let mut s = format!(
                    "{} {}",
                    "if".purple(),
                    cond.0.to_string(depth, br_depth + 1),
                );

                if then.len() == 1 {
                    s.push_str(&format!(
                        " {} {} {}",
                        bracket("{", br_depth),
                        then[0].0.to_string(depth, br_depth + 1),
                        bracket("}", br_depth)
                    ));
                } else {
                    s.push_str(&format!(
                        " {}\n{}\n{}{}",
                        bracket("{", br_depth),
                        list_block(then, depth + 1, br_depth + 1),
                        sep(depth),
                        bracket("}", br_depth)
                    ));
                }

                if let Some(otherwise) = otherwise {
                    if otherwise.len() == 1 {
                        s.push_str(&format!(
                            " {} {} {} {}",
                            "else".purple(),
                            bracket("{", br_depth),
                            otherwise[0].0.to_string(depth, br_depth + 1),
                            bracket("}", br_depth)
                        ));
                    } else {
                        s.push_str(&format!(
                            " {} {}\n{}\n{}{}",
                            "else".purple(),
                            bracket("{", br_depth),
                            list_block(otherwise, depth + 1, br_depth + 1),
                            sep(depth),
                            bracket("}", br_depth)
                        ));
                    }
                }

                s
            }
            _ => "unimplemented".to_string(),
        }
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string(0, 0))
    }
}
