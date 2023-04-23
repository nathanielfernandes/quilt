use std::{
    cell::RefCell,
    hash::{Hash, Hasher},
    rc::Rc,
};

use colored::Colorize;

use crate::prelude::Spanned;

use super::chunk::Chunk;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    None,
    Bool(bool),
    Int(i32),
    Float(f32),
    Range(i32, i32),
    Color([u8; 4]),

    String(Rc<String>),
    Special(Rc<(&'static str, usize)>),

    List(Rc<Vec<Value>>),

    Pair(Rc<(Value, Value)>),
    Spread(Rc<Vec<Value>>),
    Function(Rc<Function>),
    Closure(Rc<Closure>),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::None => write!(f, "none"),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Int(i) => write!(f, "{}", i),
            Value::Float(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "{}", s),
            Value::Color([r, g, b, a]) => write!(f, "#{:02x}{:02x}{:02x}{:02x}", r, g, b, a),
            Value::List(l) => {
                write!(f, "[")?;
                for (i, v) in l.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", v)?;
                }
                write!(f, "]")
            }
            Value::Pair(pair) => write!(f, "({}, {})", pair.0, pair.1),
            Value::Range(l, r) => write!(f, "{}:{}", l, r),
            Value::Spread(l) => {
                for (i, v) in l.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", v)?;
                }
                Ok(())
            }
            Value::Special(special) => write!(f, "<special id={} value={}>", special.0, special.1),

            Value::Function(func) => write!(f, "<function {}>", func.name.0),
            Value::Closure(closure) => write!(f, "<closure {}>", closure.function.name.0),
        }
    }
}

impl Value {
    #[inline]
    pub fn ntype(&self) -> &'static str {
        match self {
            Value::None => "none",
            Value::Bool(_) => "bool",
            Value::Int(_) => "int",
            Value::Float(_) => "float",
            Value::Range(_, _) => "range",
            Value::Color(_) => "color",
            Value::String(_) => "str",
            Value::Special(s) => s.0,
            Value::List(_) => "list",
            Value::Pair(_) => "pair",
            Value::Spread(_) => "spread",
            Value::Function(_) => "function",
            Value::Closure(_) => "closure",
        }
    }
    pub fn display(&self) -> String {
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
            Value::List(l) => {
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

            Value::Pair(l) => format!("({} {})", l.0.display(), l.1.display()),
            Value::Spread(s) => format!(
                "...[{}]",
                s.iter().map(|c| c.display()).collect::<Vec<_>>().join(", ")
            ),
            Value::Special(spec) => format!("<{} id={}>", spec.0, spec.1),
            Value::Function(f) => format!("<func {}>", f.name.0).cyan().to_string(),
            Value::Closure(c) => format!("<closure {}>", c.function.name.0)
                .cyan()
                .to_string(),
        }
    }
}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Value::None => 0.hash(state),
            Value::Bool(b) => b.hash(state),
            Value::Int(i) => i.hash(state),
            Value::Float(n) => n.to_bits().hash(state),
            Value::Range(a, b) => {
                a.hash(state);
                b.hash(state);
            }
            Value::Color(c) => {
                c.hash(state);
            }
            Value::String(s) => s.hash(state),
            Value::Special(s) => s.hash(state),
            Value::List(l) => l.hash(state),
            Value::Pair(p) => p.hash(state),
            Value::Spread(s) => s.hash(state),
            Value::Function(f) => f.hash(state),
            Value::Closure(c) => c.hash(state),
        }
    }
}

impl Eq for Value {}

impl From<crate::shared::ParserValue> for Value {
    fn from(value: crate::shared::ParserValue) -> Self {
        match value {
            crate::shared::ParserValue::None => Value::None,
            crate::shared::ParserValue::Bool(b) => Value::Bool(b),
            crate::shared::ParserValue::Int(i) => Value::Int(i),
            crate::shared::ParserValue::Float(f) => Value::Float(f),
            crate::shared::ParserValue::Color(c) => Value::Color(c),
            crate::shared::ParserValue::Str(s) => Value::String(Rc::new(s)),
            crate::shared::ParserValue::List(l) => {
                Value::List(Rc::new(l.into_iter().map(Value::from).collect()))
            }
            crate::shared::ParserValue::Pair(l, r) => {
                Value::Pair(Rc::new((Value::from(*l), Value::from(*r))))
            }
            crate::shared::ParserValue::Spread(s) => {
                Value::Spread(Rc::new(s.into_iter().map(Value::from).collect()))
            }
            crate::shared::ParserValue::Special(id, a) => Value::Special(Rc::new((id, a))),
            crate::shared::ParserValue::Range(l, r) => Value::Range(l, r),
        }
    }
}

impl From<&crate::shared::ParserValue> for Value {
    fn from(value: &crate::shared::ParserValue) -> Self {
        match value {
            crate::shared::ParserValue::None => Value::None,
            crate::shared::ParserValue::Bool(b) => Value::Bool(*b),
            crate::shared::ParserValue::Int(i) => Value::Int(*i),
            crate::shared::ParserValue::Float(f) => Value::Float(*f),
            crate::shared::ParserValue::Color(c) => Value::Color(*c),
            crate::shared::ParserValue::Str(s) => Value::String(Rc::new(s.clone())),
            crate::shared::ParserValue::List(l) => {
                Value::List(Rc::new(l.iter().map(Value::from).collect()))
            }
            crate::shared::ParserValue::Pair(l, r) => {
                Value::Pair(Rc::new((Value::from(&**l), Value::from(&**r))))
            }
            crate::shared::ParserValue::Spread(s) => {
                Value::Spread(Rc::new(s.iter().map(Value::from).collect()))
            }
            crate::shared::ParserValue::Special(id, a) => Value::Special(Rc::new((id, *a))),
            crate::shared::ParserValue::Range(l, r) => Value::Range(*l, *r),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: Spanned<String>,
    pub arity: u8,
    pub upvalue_count: u16,
    pub chunk: Chunk,
}

impl Hash for Function {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let (name, span) = &self.name;

        name.hash(state);
        span.0.hash(state);
        span.1.hash(state);
        span.2.hash(state);

        self.arity.hash(state);
        self.upvalue_count.hash(state);
        self.chunk.len().hash(state);
    }
}

#[derive(Debug, Clone)]
pub struct Closure {
    pub function: Rc<Function>,
    pub upvalues: Vec<Rc<RefCell<Upvalue>>>,
}

impl PartialEq for Closure {
    fn eq(&self, other: &Self) -> bool {
        self.function == other.function
            && self.upvalues.len() == other.upvalues.len()
            && self
                .upvalues
                .iter()
                .zip(other.upvalues.iter())
                .all(|(a, b)| match (&*a.borrow(), &*b.borrow()) {
                    (Upvalue::Open(l), Upvalue::Open(r)) => l == r,
                    (Upvalue::Closed(l), Upvalue::Closed(r)) => l == r,
                    _ => false,
                })
    }
}

impl Hash for Closure {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.function.hash(state);
        self.upvalues.len().hash(state);
        self.upvalues.iter().for_each(|u| u.borrow().hash(state));
    }
}

#[derive(Debug, Clone)]
pub enum Upvalue {
    Open(usize),
    Closed(Value),
}

impl Upvalue {
    pub fn new(location: usize) -> Self {
        Self::Open(location)
    }

    pub fn get(&self, stack: &[Value]) -> Value {
        match self {
            Upvalue::Open(l) => stack[*l].clone(),
            Upvalue::Closed(c) => c.clone(),
        }
    }

    pub fn set(&mut self, stack: &mut [Value], value: Value) {
        match self {
            Upvalue::Open(l) => stack[*l] = value,
            Upvalue::Closed(c) => *c = value,
        }
    }

    pub fn is_open(&self) -> bool {
        matches!(self, Upvalue::Open(_))
    }

    pub fn is_open_at(&self, location: usize) -> bool {
        if let Upvalue::Open(l) = self {
            *l == location
        } else {
            false
        }
    }

    pub fn close_if(&mut self, stack: &[Value], predicate: impl FnOnce(usize) -> bool) -> bool {
        if let Upvalue::Open(l) = self {
            if predicate(*l) {
                let value = stack[*l].clone();
                *self = Upvalue::Closed(value);
                return true;
            }
        }

        false
    }
}

impl Hash for Upvalue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Upvalue::Open(l) => l.hash(state),
            Upvalue::Closed(c) => c.hash(state),
        }
    }
}
