use std::{
    cell::RefCell,
    hash::{Hash, Hasher},
    ops::Range,
    rc::Rc,
};

use bytecode::chunk::Chunk;
use common::{
    error::Error,
    span::Spanned,
    vecc::{GetSize, Vecc},
};

// max single value size is 256kb
pub const MAX_VALUE_SIZE: usize = 1024 * 256;

impl GetSize for Value {
    #[inline]
    fn get_size(&self) -> usize {
        match self {
            Value::String(s) => s.len(),
            Value::Array(l) => l.get_size(),
            Value::Pair(pair) => {
                let (a, b) = pair.as_ref();
                a.get_size() + b.get_size()
            }
            _ => 0x10,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value<const ARRAY_MAX_SIZE: usize = MAX_VALUE_SIZE> {
    None,
    Bool(bool),
    Int(i32),
    Float(f32),
    Range(i32, i32),
    Color([u8; 4]),

    String(Rc<String>),
    Special(Box<(&'static str, usize)>),

    Array(Rc<Vecc<Value, ARRAY_MAX_SIZE>>),

    // specialized value for looping
    LoopCtx(usize),

    Pair(Rc<(Value, Value)>),
    // Spread(Rc<Vec<Value>>),
    Function(Rc<Function>),
    Closure(Rc<Closure>),
}

impl<const ARRAY_MAX_SIZE: usize> Value<ARRAY_MAX_SIZE> {
    pub const ARRAY_MAX_SIZE: usize = ARRAY_MAX_SIZE;
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
            // Value::Spread(l) => {
            //     for (i, v) in l.iter().enumerate() {
            //         if i > 0 {
            //             write!(f, ", ")?;
            //         }
            //         write!(f, "{}", v)?;
            //     }
            //     Ok(())
            // }
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
            // Value::Spread(_) => "spread",
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
            // Value::Spread(s) => s.hash(state),
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

macro_rules! impl_from {
    (float $($t:ty),*) => {
        $(
            impl From<$t> for Value {
                fn from(v: $t) -> Self {
                    Value::Float(v as f32)
                }
            }
        )*
    };
    (int $($t:ty),*) => {
        $(
            impl From<$t> for Value {
                fn from(v: $t) -> Self {
                    Value::Int(v as i32)
                }
            }
        )*
    };
}

impl_from!(float f32, f64);
impl_from!(int i8, i16, i32, i64, u8, u16, u32, u64);

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

impl From<Range<i32>> for Value {
    fn from(r: Range<i32>) -> Self {
        Value::Range(r.start, r.end)
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

impl<T: Into<Value>> From<Option<T>> for Value {
    fn from(o: Option<T>) -> Self {
        match o {
            Some(v) => v.into(),
            None => Value::None,
        }
    }
}

impl<L: Into<Value>, R: Into<Value>> From<(L, R)> for Value {
    fn from((a, b): (L, R)) -> Self {
        Value::Pair(Rc::new((a.into(), b.into())))
    }
}

pub fn make_value_array(values: Vec<Value>) -> Result<Value, Error> {
    Vecc::try_new_from(values).map(|v| Value::Array(Rc::new(v)))
}

impl<T: Into<Value>> From<Vec<T>> for Value {
    fn from(v: Vec<T>) -> Self {
        Value::Array(Rc::new(Vecc::new_from(
            v.into_iter().map(|v| v.into()).collect(),
        )))
    }
}
