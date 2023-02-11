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
/// * `Custom`: A custom value
#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    None,
    Bool(bool),
    Int(i32),
    Float(f32),
    Str(String),
    Color([u8; 4]),
    List(Vec<Value>),
    Pair(Box<Value>, Box<Value>),

    Special(&'static str, usize),
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
            Value::List(l) => write!(f, "{:?}", l),
            Value::Pair(l, r) => write!(f, "({}, {})", l, r),
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
            Value::Special(id, _) => id,
        }
    }
}
