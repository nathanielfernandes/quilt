use std::{
    cell::RefCell,
    hash::{Hash, Hasher},
    rc::Rc,
};

use bytecode::chunk::Chunk;
use common::span::Spanned;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    None,
    Bool(bool),
    Int(i32),
    Float(f32),
    Range(i32, i32),
    Color([u8; 4]),

    String(Rc<String>),
    Special(Box<(&'static str, usize)>),

    Array(Rc<Vec<Value>>),

    // specialized value for looping
    LoopCtx(usize),

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
            Value::Array(l) => {
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

            Value::LoopCtx(idx) => write!(f, "<loop_ctx {}>", idx),
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
            Value::Array(_) => "list",
            Value::Pair(_) => "pair",
            Value::Spread(_) => "spread",
            Value::Function(_) => "function",
            Value::Closure(_) => "closure",
            Value::LoopCtx(_) => "loop_ctx",
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
            Value::Array(l) => l.hash(state),
            Value::Pair(p) => p.hash(state),
            Value::Spread(s) => s.hash(state),
            Value::Function(f) => f.hash(state),
            Value::Closure(c) => c.hash(state),
            Value::LoopCtx(idx) => idx.hash(state),
        }
    }
}

impl Eq for Value {}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: Spanned<String>,
    pub arity: u8,
    pub upvalue_count: u16,
    pub chunk: Chunk<Value>,
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
    pub upvalues: Rc<Vec<Rc<RefCell<Upvalue>>>>,
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

impl From<i32> for Value {
    fn from(i: i32) -> Self {
        Value::Int(i)
    }
}

impl From<f32> for Value {
    fn from(f: f32) -> Self {
        Value::Float(f)
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Self {
        Value::Bool(b)
    }
}

impl From<String> for Value {
    fn from(s: String) -> Self {
        Value::String(Rc::new(s))
    }
}

impl From<&str> for Value {
    fn from(s: &str) -> Self {
        Value::String(Rc::new(s.to_owned()))
    }
}

impl From<&String> for Value {
    fn from(s: &String) -> Self {
        Value::String(Rc::new(s.to_owned()))
    }
}

impl From<(i32, i32)> for Value {
    fn from((a, b): (i32, i32)) -> Self {
        Value::Range(a, b)
    }
}

impl From<[u8; 4]> for Value {
    fn from(c: [u8; 4]) -> Self {
        Value::Color(c)
    }
}

impl From<(&'static str, usize)> for Value {
    fn from((s, i): (&'static str, usize)) -> Self {
        Value::Special(Box::new((s, i)))
    }
}

impl From<Option<Value>> for Value {
    fn from(o: Option<Value>) -> Self {
        match o {
            Some(v) => v,
            None => Value::None,
        }
    }
}
