/// A parsed literal value.
///
/// ### Variants
/// * `None`: The `none` value
/// * `Bool`: A boolean value
/// * `Int`: An integer value
/// * `Float`: A floating point value
/// * `Range`: A range iterator
/// * `Color`: A color value
/// * `String`: A string value
/// * `Array`: A list of values
/// * `Pair`: A pair of values
#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    None,
    Bool(bool),
    Int(i64),
    Float(f64),
    Range(i32, i32),
    Color([u8; 4]),

    String(String),
    Array(Vec<Literal>),
    Pair(Box<Literal>, Box<Literal>),
}
