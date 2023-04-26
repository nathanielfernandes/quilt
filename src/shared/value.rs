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
pub enum ParserValue {
    // primitives
    None,
    Bool(bool),
    Int(i32),
    Float(f32),
    Range(i32, i32),
    Color([u8; 4]),

    // collections
    Array(Vec<ParserValue>),
    Str(String),
    Pair(Box<ParserValue>, Box<ParserValue>),
    Spread(Vec<ParserValue>),
    Special(&'static str, usize),
}

impl ParserValue {
    pub fn display(&self) -> String {
        match self {
            ParserValue::None => "none".cyan().to_string(),
            ParserValue::Bool(b) => b.to_string().yellow().to_string(),
            ParserValue::Int(i) => i.to_string().yellow().to_string(),
            ParserValue::Float(n) => n.to_string().yellow().to_string(),
            ParserValue::Str(s) => format!("'{}'", s).green().to_string(),
            ParserValue::Color([r, g, b, a]) => format!("#{:02x}{:02x}{:02x}{:02x}", r, g, b, a),
            ParserValue::Array(l) => {
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
            ParserValue::Pair(a, b) => format!("({}, {})", a.display(), b.display()),
            ParserValue::Range(a, b) => format!("{}:{}", a, b),
            ParserValue::Spread(l) => {
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
            ParserValue::Special(name, _) => name.to_string().cyan().to_string(),
        }
    }
}

impl std::fmt::Display for ParserValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserValue::None => write!(f, "none"),
            ParserValue::Bool(b) => write!(f, "{}", b),
            ParserValue::Int(i) => write!(f, "{}", i),
            ParserValue::Float(n) => write!(f, "{}", n),
            ParserValue::Str(s) => write!(f, "{}", s),
            ParserValue::Color([r, g, b, a]) => write!(f, "#{:02x}{:02x}{:02x}{:02x}", r, g, b, a),
            ParserValue::Array(l) => {
                write!(f, "[")?;
                for (i, v) in l.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", v)?;
                }
                write!(f, "]")
            }
            ParserValue::Pair(l, r) => write!(f, "({}, {})", l, r),
            ParserValue::Range(l, r) => write!(f, "{}:{}", l, r),
            ParserValue::Spread(l) => {
                for (i, v) in l.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", v)?;
                }
                Ok(())
            }
            ParserValue::Special(s, id) => write!(f, "<special id={} value={}>", s, id),
        }
    }
}

impl ParserValue {
    /// Returns the type of the value as a string.
    pub fn ntype(&self) -> &'static str {
        match self {
            ParserValue::None => "none",
            ParserValue::Bool(_) => "bool",
            ParserValue::Int(_) => "int",
            ParserValue::Float(_) => "float",
            ParserValue::Str(_) => "str",
            ParserValue::Color(_) => "color",
            ParserValue::Array(_) => "list",
            ParserValue::Pair(_, _) => "pair",
            ParserValue::Range(_, _) => "range",
            ParserValue::Spread(_) => "spread",
            ParserValue::Special(id, _) => id,
        }
    }
}

impl From<char> for ParserValue {
    fn from(c: char) -> Self {
        ParserValue::Str(c.to_string())
    }
}

impl From<i32> for ParserValue {
    fn from(i: i32) -> Self {
        ParserValue::Int(i)
    }
}

impl From<i16> for ParserValue {
    fn from(i: i16) -> Self {
        ParserValue::Int(i as i32)
    }
}

impl From<f32> for ParserValue {
    fn from(f: f32) -> Self {
        ParserValue::Float(f)
    }
}

impl From<bool> for ParserValue {
    fn from(b: bool) -> Self {
        ParserValue::Bool(b)
    }
}

impl From<String> for ParserValue {
    fn from(s: String) -> Self {
        ParserValue::Str(s)
    }
}

impl From<&str> for ParserValue {
    fn from(s: &str) -> Self {
        ParserValue::Str(s.to_string())
    }
}

impl From<[u8; 4]> for ParserValue {
    fn from(c: [u8; 4]) -> Self {
        ParserValue::Color(c)
    }
}

impl From<Vec<ParserValue>> for ParserValue {
    fn from(l: Vec<ParserValue>) -> Self {
        ParserValue::Array(l)
    }
}

impl From<(ParserValue, ParserValue)> for ParserValue {
    fn from(p: (ParserValue, ParserValue)) -> Self {
        ParserValue::Pair(Box::new(p.0), Box::new(p.1))
    }
}
