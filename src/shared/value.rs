use colored::Colorize;

/// A value in the quilt language.
///
/// This is the type that is used to represent all values in the quilt language.
///
/// ### Variants
/// * `None`: The `none` value
/// * `Bool`: A boolean value
/// * `Int`: An integer value
/// * `Float`: A floating point value
/// * `Str`: A string value
/// * `Color`: A color value
/// * `List`: A list of values
/// * `Pair`: A pair of values
/// * `Range`: A range iterator
/// * `Spread`: A spread of values
/// * `Special`: A custom value
#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    // primitives
    None,
    Bool(bool),
    Int(i32),
    Float(f32),
    Range(i32, i32),
    Color([u8; 4]),

    // collections
    List(Vec<Value>),
    Str(String),
    Pair(Box<Value>, Box<Value>),
    Spread(Vec<Value>),
    Special(&'static str, usize),
}

impl Value {
    pub fn display(&self) -> String {
        match self {
            Value::None => "none".cyan().to_string(),
            Value::Bool(b) => b.to_string().yellow().to_string(),
            Value::Int(i) => i.to_string().yellow().to_string(),
            Value::Float(n) => n.to_string().yellow().to_string(),
            Value::Str(s) => format!("'{}'", s).green().to_string(),
            Value::Color([r, g, b, a]) => format!("#{:02x}{:02x}{:02x}{:02x}", r, g, b, a),
            Value::List(l) => {
                let mut s = "[".to_string();
                for (i, v) in l.iter().enumerate() {
                    if i > 0 {
                        s.push_str(", ");
                    }
                    s.push_str(&v.display());
                }
                s.push(']');
                s
            }
            Value::Pair(a, b) => format!("({}, {})", a.display(), b.display()),
            Value::Range(a, b) => format!("{}:{}", a, b),
            Value::Spread(l) => {
                let mut s = "(".to_string();
                for (i, v) in l.iter().enumerate() {
                    if i > 0 {
                        s.push_str(", ");
                    }
                    s.push_str(&v.display());
                }
                s.push(')');
                s
            }
            Value::Special(name, _) => name.to_string().cyan().to_string(),
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::None => write!(f, "none"),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Int(i) => write!(f, "{}", i),
            Value::Float(n) => write!(f, "{}", n),
            Value::Str(s) => write!(f, "{}", s),
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
            Value::Pair(l, r) => write!(f, "({}, {})", l, r),
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
            Value::Special(s, id) => write!(f, "<special id={} value={}>", s, id),
        }
    }
}

impl Value {
    /// Returns the type of the value as a string.
    pub fn ntype(&self) -> &'static str {
        match self {
            Value::None => "none",
            Value::Bool(_) => "bool",
            Value::Int(_) => "int",
            Value::Float(_) => "float",
            Value::Str(_) => "str",
            Value::Color(_) => "color",
            Value::List(_) => "list",
            Value::Pair(_, _) => "pair",
            Value::Range(_, _) => "range",
            Value::Spread(_) => "spread",
            Value::Special(id, _) => id,
        }
    }
}

impl From<char> for Value {
    fn from(c: char) -> Self {
        Value::Str(c.to_string())
    }
}

impl From<i32> for Value {
    fn from(i: i32) -> Self {
        Value::Int(i)
    }
}

impl From<i16> for Value {
    fn from(i: i16) -> Self {
        Value::Int(i as i32)
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
        Value::Str(s)
    }
}

impl From<&str> for Value {
    fn from(s: &str) -> Self {
        Value::Str(s.to_string())
    }
}

impl From<[u8; 4]> for Value {
    fn from(c: [u8; 4]) -> Self {
        Value::Color(c)
    }
}

impl From<Vec<Value>> for Value {
    fn from(l: Vec<Value>) -> Self {
        Value::List(l)
    }
}

impl From<(Value, Value)> for Value {
    fn from(p: (Value, Value)) -> Self {
        Value::Pair(Box::new(p.0), Box::new(p.1))
    }
}
