use common::{
    error::{Error, OverflowError, TypeError},
    vecc::GetSize,
    Rc,
};

use super::value::Value;

impl Value {
    #[inline]
    pub fn not(&self) -> Result<Value, Error> {
        match self {
            Value::Bool(b) => Ok(Value::Bool(!b)),
            _ => Err(TypeError::UnsupportedUnaryOperation {
                op: "!",
                rhs: self.ntype(),
            }
            .into()),
        }
    }

    #[inline]
    pub fn negate(&self) -> Result<Value, Error> {
        match self {
            Value::Int(i) => Ok(Value::Int(-i)),
            Value::Float(f) => Ok(Value::Float(-f)),

            _ => Err(TypeError::UnsupportedUnaryOperation {
                op: "-",
                rhs: self.ntype(),
            }
            .into()),
        }
    }

    // #[inline]
    // pub fn spread(&self) -> Result<Value, Error> {
    //     match self {
    //         Value::Array(l) => Ok(Value::Spread(l.clone())),
    //         Value::Pair(pair) => Ok(Value::Spread(Rc::new(vec![pair.0.clone(), pair.1.clone()]))),
    //         Value::String(s) => Ok(Value::Spread(Rc::new(
    //             s.chars()
    //                 .map(|c| Value::String(Rc::new(c.to_string())))
    //                 .collect(),
    //         ))),

    //         _ => Err(TypeError::UnsupportedUnaryOperation {
    //             op: Op::Spread,
    //             rhs: self.ntype(),
    //         }
    //         .into()),
    //     }
    // }

    // #[inline]
    // pub fn spread(&self) -> Result<Vec<Value>, Error> {
    //     match self {
    //         Value::List(l) => Ok((**l).clone()),
    //         Value::Pair(pair) => Ok(vec![pair.0.clone(), pair.1.clone()]),

    //         _ => Err(TypeError::UnsupportedUnaryOperation {
    //             op: Op::Spread,
    //             rhs: self.ntype(),
    //         }
    //         .into()),
    //     }
    // }

    #[inline]
    pub fn add(&self, other: &Value, string_max_size: usize) -> Result<Value, Error> {
        match (self, other) {
            (Value::String(lhs), Value::String(rhs)) => {
                if lhs.get_size() + rhs.get_size() > string_max_size {
                    return Err(OverflowError::StringTooLarge.into());
                }

                Ok(Value::String(Rc::new(lhs.to_string() + rhs)))
            }
            (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs + rhs)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs + rhs)),
            (Value::Int(lhs), Value::Float(rhs)) => Ok(Value::Float(*lhs as f64 + rhs)),
            (Value::Float(lhs), Value::Int(rhs)) => Ok(Value::Float(lhs + *rhs as f64)),
            (Value::Color(lhs), Value::Color(rhs)) => Ok(Value::Color([
                lhs[0].saturating_add(rhs[0]),
                lhs[1].saturating_add(rhs[1]),
                lhs[2].saturating_add(rhs[2]),
                lhs[3].saturating_add(rhs[3]),
            ])),
            _ => Err(TypeError::UnsupportedBinaryOperation {
                op: "+",
                lhs: self.ntype(),
                rhs: other.ntype(),
            }
            .into()),
        }
    }

    #[inline]
    pub fn subtract(&self, other: &Value) -> Result<Value, Error> {
        match (self, other) {
            (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs - rhs)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs - rhs)),
            (Value::Int(lhs), Value::Float(rhs)) => Ok(Value::Float(*lhs as f64 - rhs)),
            (Value::Float(lhs), Value::Int(rhs)) => Ok(Value::Float(lhs - *rhs as f64)),
            (Value::Color(lhs), Value::Color(rhs)) => Ok(Value::Color([
                lhs[0].saturating_sub(rhs[0]),
                lhs[1].saturating_sub(rhs[1]),
                lhs[2].saturating_sub(rhs[2]),
                lhs[3].saturating_sub(rhs[3]),
            ])),
            _ => Err(TypeError::UnsupportedBinaryOperation {
                op: "-",
                lhs: self.ntype(),
                rhs: other.ntype(),
            }
            .into()),
        }
    }

    #[inline]
    pub fn multiply(&self, other: &Value, string_max_size: usize) -> Result<Value, Error> {
        match (self, other) {
            (Value::String(lhs), Value::Int(rhs)) => {
                if *rhs < 0 {
                    return Err(TypeError::UnsupportedBinaryOperation {
                        op: "*",
                        lhs: self.ntype(),
                        rhs: "-int",
                    }
                    .into());
                }

                // let rhs = *rhs.min(&256) as usize;
                let rhs = *rhs as usize;

                // check if string is too long
                if lhs.len() * rhs > string_max_size {
                    return Err(OverflowError::StringTooLarge.into());
                }

                Ok(Value::String(Rc::new(lhs.repeat(rhs))))
            }
            (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs * rhs)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs * rhs)),
            (Value::Int(lhs), Value::Float(rhs)) => Ok(Value::Float(*lhs as f64 * rhs)),
            (Value::Float(lhs), Value::Int(rhs)) => Ok(Value::Float(lhs * *rhs as f64)),
            (Value::Color(lhs), Value::Color(rhs)) => Ok(Value::Color([
                lhs[0].saturating_mul(rhs[0]),
                lhs[1].saturating_mul(rhs[1]),
                lhs[2].saturating_mul(rhs[2]),
                lhs[3].saturating_mul(rhs[3]),
            ])),
            _ => Err(TypeError::UnsupportedBinaryOperation {
                op: "*",
                lhs: self.ntype(),
                rhs: other.ntype(),
            }
            .into()),
        }
    }

    #[inline]
    pub fn divide(&self, other: &Value) -> Result<Value, Error> {
        if other == &Value::Int(0) || other == &Value::Float(0.0) {
            return Err(Error::ZeroDivisionError);
        }
        match (self, other) {
            (Value::String(lhs), Value::String(rhs)) => {
                Ok(Value::String(Rc::new(lhs.replace(&**rhs, ""))))
            }
            (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs / rhs)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs / rhs)),
            (Value::Int(lhs), Value::Float(rhs)) => Ok(Value::Float(*lhs as f64 / rhs)),
            (Value::Float(lhs), Value::Int(rhs)) => Ok(Value::Float(lhs / *rhs as f64)),
            (Value::Color(lhs), Value::Color(rhs)) => Ok(Value::Color([
                lhs[0].saturating_div(rhs[0]),
                lhs[1].saturating_div(rhs[1]),
                lhs[2].saturating_div(rhs[2]),
                lhs[3].saturating_div(rhs[3]),
            ])),
            _ => Err(TypeError::UnsupportedBinaryOperation {
                op: "/",
                lhs: self.ntype(),
                rhs: other.ntype(),
            }
            .into()),
        }
    }

    #[inline]
    pub fn modulo(&self, other: &Value) -> Result<Value, Error> {
        if other == &Value::Int(0) || other == &Value::Float(0.0) {
            return Err(Error::ZeroDivisionError);
        }
        match (self, other) {
            (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs % rhs)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs % rhs)),
            (Value::Int(lhs), Value::Float(rhs)) => Ok(Value::Float(*lhs as f64 % rhs)),
            (Value::Float(lhs), Value::Int(rhs)) => Ok(Value::Float(lhs % *rhs as f64)),
            _ => Err(TypeError::UnsupportedBinaryOperation {
                op: "%",
                lhs: self.ntype(),
                rhs: other.ntype(),
            }
            .into()),
        }
    }

    #[inline]
    pub fn power(&self, other: &Value) -> Result<Value, Error> {
        match (self, other) {
            (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs.pow(*rhs as u32))),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs.powf(*rhs))),
            (Value::Int(lhs), Value::Float(rhs)) => Ok(Value::Float((*lhs as f64).powf(*rhs))),
            (Value::Float(lhs), Value::Int(rhs)) => Ok(Value::Float(lhs.powf(*rhs as f64))),
            _ => Err(TypeError::UnsupportedBinaryOperation {
                op: "**",
                lhs: self.ntype(),
                rhs: other.ntype(),
            }
            .into()),
        }
    }

    #[inline]
    pub fn and(&self, other: &Value) -> Result<Value, Error> {
        match (self, other) {
            (Value::Bool(lhs), Value::Bool(rhs)) => Ok(Value::Bool(*lhs && *rhs)),
            _ => Err(TypeError::UnsupportedBinaryOperation {
                op: "&&",
                lhs: self.ntype(),
                rhs: other.ntype(),
            }
            .into()),
        }
    }

    #[inline]
    pub fn or(&self, other: &Value) -> Result<Value, Error> {
        match (self, other) {
            (Value::Bool(lhs), Value::Bool(rhs)) => Ok(Value::Bool(*lhs || *rhs)),
            _ => Err(TypeError::UnsupportedBinaryOperation {
                op: "||",
                lhs: self.ntype(),
                rhs: other.ntype(),
            }
            .into()),
        }
    }

    #[inline]
    pub fn equal(&self, other: &Value) -> Result<Value, Error> {
        Ok(Value::Bool(self == other))
    }

    #[inline]
    pub fn not_equal(&self, other: &Value) -> Result<Value, Error> {
        Ok(Value::Bool(self != other))
    }

    #[inline]
    pub fn greater_than(&self, other: &Value) -> Result<Value, Error> {
        match (self, other) {
            (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Bool(lhs > rhs)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Bool(lhs > rhs)),
            (Value::Int(lhs), Value::Float(rhs)) => Ok(Value::Bool(*lhs as f64 > *rhs)),
            (Value::Float(lhs), Value::Int(rhs)) => Ok(Value::Bool(*lhs > *rhs as f64)),
            _ => Err(TypeError::UnsupportedBinaryOperation {
                op: ">",
                lhs: self.ntype(),
                rhs: other.ntype(),
            }
            .into()),
        }
    }

    #[inline]
    pub fn greater_than_or_equal(&self, other: &Value) -> Result<Value, Error> {
        match (self, other) {
            (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Bool(lhs >= rhs)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Bool(lhs >= rhs)),
            (Value::Int(lhs), Value::Float(rhs)) => Ok(Value::Bool(*lhs as f64 >= *rhs)),
            (Value::Float(lhs), Value::Int(rhs)) => Ok(Value::Bool(*lhs >= *rhs as f64)),
            _ => Err(TypeError::UnsupportedBinaryOperation {
                op: ">=",
                lhs: self.ntype(),
                rhs: other.ntype(),
            }
            .into()),
        }
    }

    #[inline]
    pub fn less_than(&self, other: &Value) -> Result<Value, Error> {
        match (self, other) {
            (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Bool(lhs < rhs)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Bool(lhs < rhs)),
            (Value::Int(lhs), Value::Float(rhs)) => Ok(Value::Bool((*lhs as f64) < *rhs)),
            (Value::Float(lhs), Value::Int(rhs)) => Ok(Value::Bool(*lhs < *rhs as f64)),
            _ => Err(TypeError::UnsupportedBinaryOperation {
                op: "<",
                lhs: self.ntype(),
                rhs: other.ntype(),
            }
            .into()),
        }
    }

    #[inline]
    pub fn less_than_or_equal(&self, other: &Value) -> Result<Value, Error> {
        match (self, other) {
            (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Bool(lhs <= rhs)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Bool(lhs <= rhs)),
            (Value::Int(lhs), Value::Float(rhs)) => Ok(Value::Bool(*lhs as f64 <= *rhs)),
            (Value::Float(lhs), Value::Int(rhs)) => Ok(Value::Bool(*lhs <= *rhs as f64)),
            _ => Err(TypeError::UnsupportedBinaryOperation {
                op: "<=",
                lhs: self.ntype(),
                rhs: other.ntype(),
            }
            .into()),
        }
    }

    #[inline]
    pub fn is_truthy(&self) -> Result<bool, Error> {
        match self {
            Value::Bool(b) => Ok(*b),
            Value::Int(i) => Ok(*i != 0),
            Value::Float(f) => Ok(*f != 0.0),
            Value::String(s) => Ok(!s.is_empty()),
            Value::None => Ok(false),
            Value::Color(c) => Ok(c != &[0, 0, 0, 0]),
            _ => Err(TypeError::Expected("truthy", self.ntype()).into()),
        }
    }

    #[inline]
    pub fn join(
        &self,
        other: &Value,
        string_max_size: usize,
        array_max_size: usize,
    ) -> Result<Value, Error> {
        match (self, other) {
            (Value::String(lhs), Value::String(rhs)) => {
                if lhs.get_size() + rhs.get_size() > string_max_size {
                    return Err(OverflowError::StringTooLarge.into());
                }

                Ok(Value::String(Rc::new(lhs.to_string() + rhs)))
            }
            (Value::String(lhs), Value::Int(rhs)) => {
                let rhs = rhs.to_string();

                if lhs.get_size() + rhs.get_size() > string_max_size {
                    return Err(OverflowError::StringTooLarge.into());
                }

                Ok(Value::String(Rc::new(lhs.to_string() + &rhs)))
            }
            (Value::String(lhs), Value::Float(rhs)) => {
                let rhs = rhs.to_string();

                if lhs.get_size() + rhs.get_size() > string_max_size {
                    return Err(OverflowError::StringTooLarge.into());
                }

                Ok(Value::String(Rc::new(lhs.to_string() + &rhs)))
            }
            (Value::String(lhs), Value::Bool(rhs)) => {
                let rhs = rhs.to_string();

                if lhs.get_size() + rhs.get_size() > string_max_size {
                    return Err(OverflowError::StringTooLarge.into());
                }

                Ok(Value::String(Rc::new(lhs.to_string() + &rhs)))
            }
            (Value::String(lhs), Value::None) => {
                let rhs = "none".to_string();

                if lhs.get_size() + rhs.get_size() > string_max_size {
                    return Err(OverflowError::StringTooLarge.into());
                }

                Ok(Value::String(Rc::new(lhs.to_string() + &rhs)))
            }
            (Value::Int(lhs), Value::String(rhs)) => {
                let lhs = lhs.to_string();

                if lhs.get_size() + rhs.get_size() > string_max_size {
                    return Err(OverflowError::StringTooLarge.into());
                }

                Ok(Value::String(Rc::new(lhs + rhs)))
            }
            (Value::Float(lhs), Value::String(rhs)) => {
                let lhs = lhs.to_string();

                if lhs.get_size() + rhs.get_size() > string_max_size {
                    return Err(OverflowError::StringTooLarge.into());
                }

                Ok(Value::String(Rc::new(lhs + rhs)))
            }
            (Value::Bool(lhs), Value::String(rhs)) => {
                let lhs = lhs.to_string();

                if lhs.get_size() + rhs.get_size() > string_max_size {
                    return Err(OverflowError::StringTooLarge.into());
                }

                Ok(Value::String(Rc::new(lhs + rhs)))
            }
            (Value::None, Value::String(rhs)) => {
                let lhs = "none".to_string();

                if lhs.get_size() + rhs.get_size() > string_max_size {
                    return Err(OverflowError::StringTooLarge.into());
                }

                Ok(Value::String(Rc::new(lhs + rhs)))
            }
            (Value::Array(lhs), Value::Array(rhs)) => {
                if lhs.get_size() + rhs.get_size() > array_max_size {
                    return Err(OverflowError::ArrayTooLarge.into());
                }

                let mut new_list = (**lhs).clone();
                new_list.extend(rhs.to_vec())?;

                Ok(Value::Array(Rc::new(new_list)))
            }
            (Value::Color(lhs), Value::Color(rhs)) => {
                // lerp colors
                let mut new_color = [0; 4];
                for i in 0..4 {
                    new_color[i] = (lhs[i] + rhs[i]) / 2;
                }

                Ok(Value::Color(new_color))
            }

            _ => Err(TypeError::UnsupportedBinaryOperation {
                op: "..",
                lhs: self.ntype(),
                rhs: other.ntype(),
            }
            .into()),
        }
    }

    #[inline]
    pub fn bitwise_and(&self, other: &Value) -> Result<Value, Error> {
        match (self, other) {
            (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs & rhs)),
            _ => Err(TypeError::UnsupportedBinaryOperation {
                op: "&",
                lhs: self.ntype(),
                rhs: other.ntype(),
            }
            .into()),
        }
    }

    #[inline]
    pub fn bitwise_or(&self, other: &Value) -> Result<Value, Error> {
        match (self, other) {
            (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs | rhs)),
            _ => Err(TypeError::UnsupportedBinaryOperation {
                op: "|",
                lhs: self.ntype(),
                rhs: other.ntype(),
            }
            .into()),
        }
    }

    #[inline]
    pub fn bitwise_xor(&self, other: &Value) -> Result<Value, Error> {
        match (self, other) {
            (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs ^ rhs)),
            _ => Err(TypeError::UnsupportedBinaryOperation {
                op: "^",
                lhs: self.ntype(),
                rhs: other.ntype(),
            }
            .into()),
        }
    }

    #[inline]
    pub fn bitwise_not(&self) -> Result<Value, Error> {
        match self {
            Value::Int(i) => Ok(Value::Int(!i)),
            _ => Err(TypeError::UnsupportedUnaryOperation {
                op: "~",
                rhs: self.ntype(),
            }
            .into()),
        }
    }

    #[inline]
    pub fn bitwise_shift_left(&self, other: &Value) -> Result<Value, Error> {
        match (self, other) {
            (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs << rhs)),
            _ => Err(TypeError::UnsupportedBinaryOperation {
                op: "<<",
                lhs: self.ntype(),
                rhs: other.ntype(),
            }
            .into()),
        }
    }

    #[inline]
    pub fn bitwise_shift_right(&self, other: &Value) -> Result<Value, Error> {
        match (self, other) {
            (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs >> rhs)),
            _ => Err(TypeError::UnsupportedBinaryOperation {
                op: ">>",
                lhs: self.ntype(),
                rhs: other.ntype(),
            }
            .into()),
        }
    }
}

// boundary checks
// wraps negatives around
pub fn fix_index(idx: i64, len: usize) -> Result<usize, Error> {
    let index = if idx < 0 {
        if -idx > len as i64 {
            Err(Error::IndexError(idx))?
        } else {
            len + idx as usize
        }
    } else {
        idx as usize
    };

    if index >= len {
        Err(Error::IndexError(idx))?
    }

    Ok(index)
}
