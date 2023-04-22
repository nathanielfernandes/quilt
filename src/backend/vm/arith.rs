use std::rc::Rc;

use crate::prelude::{Error, Op, TypeError};

use super::value::Value;

impl Value {
    #[inline]
    pub fn not(&self) -> Result<Value, Error> {
        match self {
            Value::Bool(b) => Ok(Value::Bool(!b)),
            _ => Err(TypeError::UnsupportedUnaryOperation {
                op: Op::Not,
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
                op: Op::Neg,
                rhs: self.ntype(),
            }
            .into()),
        }
    }

    #[inline]
    pub fn spread(&self) -> Result<Value, Error> {
        match self {
            Value::List(l) => Ok(Value::Spread(l.clone())),
            Value::Pair(pair) => Ok(Value::Spread(Rc::new(vec![pair.0.clone(), pair.1.clone()]))),
            Value::String(s) => Ok(Value::Spread(Rc::new(
                s.chars()
                    .map(|c| Value::String(Rc::new(c.to_string())))
                    .collect(),
            ))),

            _ => Err(TypeError::UnsupportedUnaryOperation {
                op: Op::Spread,
                rhs: self.ntype(),
            }
            .into()),
        }
    }

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
    pub fn add(&self, other: &Value) -> Result<Value, Error> {
        match (self, other) {
            (Value::String(lhs), Value::String(rhs)) => {
                Ok(Value::String(Rc::new(lhs.to_string() + rhs)))
            }
            (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs + rhs)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs + rhs)),
            (Value::Int(lhs), Value::Float(rhs)) => Ok(Value::Float(*lhs as f32 + rhs)),
            (Value::Float(lhs), Value::Int(rhs)) => Ok(Value::Float(lhs + *rhs as f32)),
            (Value::Color(lhs), Value::Color(rhs)) => Ok(Value::Color([
                lhs[0].saturating_add(rhs[0]),
                lhs[1].saturating_add(rhs[1]),
                lhs[2].saturating_add(rhs[2]),
                lhs[3].saturating_add(rhs[3]),
            ])),
            _ => Err(TypeError::UnsupportedBinaryOperation {
                op: Op::Add,
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
            (Value::Int(lhs), Value::Float(rhs)) => Ok(Value::Float(*lhs as f32 - rhs)),
            (Value::Float(lhs), Value::Int(rhs)) => Ok(Value::Float(lhs - *rhs as f32)),
            (Value::Color(lhs), Value::Color(rhs)) => Ok(Value::Color([
                lhs[0].saturating_sub(rhs[0]),
                lhs[1].saturating_sub(rhs[1]),
                lhs[2].saturating_sub(rhs[2]),
                lhs[3].saturating_sub(rhs[3]),
            ])),
            _ => Err(TypeError::UnsupportedBinaryOperation {
                op: Op::Sub,
                lhs: self.ntype(),
                rhs: other.ntype(),
            }
            .into()),
        }
    }

    #[inline]
    pub fn multiply(&self, other: &Value) -> Result<Value, Error> {
        match (self, other) {
            (Value::String(lhs), Value::Int(rhs)) => {
                Ok(Value::String(Rc::new(lhs.repeat((*rhs).min(64) as usize))))
            }
            (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs * rhs)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs * rhs)),
            (Value::Int(lhs), Value::Float(rhs)) => Ok(Value::Float(*lhs as f32 * rhs)),
            (Value::Float(lhs), Value::Int(rhs)) => Ok(Value::Float(lhs * *rhs as f32)),
            (Value::Color(lhs), Value::Color(rhs)) => Ok(Value::Color([
                lhs[0].saturating_mul(rhs[0]),
                lhs[1].saturating_mul(rhs[1]),
                lhs[2].saturating_mul(rhs[2]),
                lhs[3].saturating_mul(rhs[3]),
            ])),
            _ => Err(TypeError::UnsupportedBinaryOperation {
                op: Op::Mul,
                lhs: self.ntype(),
                rhs: other.ntype(),
            }
            .into()),
        }
    }

    #[inline]
    pub fn divide(&self, other: &Value) -> Result<Value, Error> {
        match (self, other) {
            (Value::String(lhs), Value::String(rhs)) => {
                Ok(Value::String(Rc::new(lhs.replace(&**rhs, ""))))
            }
            (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs / rhs)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs / rhs)),
            (Value::Int(lhs), Value::Float(rhs)) => Ok(Value::Float(*lhs as f32 / rhs)),
            (Value::Float(lhs), Value::Int(rhs)) => Ok(Value::Float(lhs / *rhs as f32)),
            (Value::Color(lhs), Value::Color(rhs)) => Ok(Value::Color([
                lhs[0].saturating_div(rhs[0]),
                lhs[1].saturating_div(rhs[1]),
                lhs[2].saturating_div(rhs[2]),
                lhs[3].saturating_div(rhs[3]),
            ])),
            _ => Err(TypeError::UnsupportedBinaryOperation {
                op: Op::Div,
                lhs: self.ntype(),
                rhs: other.ntype(),
            }
            .into()),
        }
    }

    #[inline]
    pub fn modulo(&self, other: &Value) -> Result<Value, Error> {
        match (self, other) {
            (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs % rhs)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs % rhs)),
            (Value::Int(lhs), Value::Float(rhs)) => Ok(Value::Float(*lhs as f32 % rhs)),
            (Value::Float(lhs), Value::Int(rhs)) => Ok(Value::Float(lhs % *rhs as f32)),
            _ => Err(TypeError::UnsupportedBinaryOperation {
                op: Op::Mod,
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
            (Value::Int(lhs), Value::Float(rhs)) => Ok(Value::Float((*lhs as f32).powf(*rhs))),
            (Value::Float(lhs), Value::Int(rhs)) => Ok(Value::Float(lhs.powf(*rhs as f32))),
            _ => Err(TypeError::UnsupportedBinaryOperation {
                op: Op::Pow,
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
                op: Op::And,
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
                op: Op::Or,
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
            (Value::Int(lhs), Value::Float(rhs)) => Ok(Value::Bool(*lhs as f32 > *rhs)),
            (Value::Float(lhs), Value::Int(rhs)) => Ok(Value::Bool(*lhs > *rhs as f32)),
            _ => Err(TypeError::UnsupportedBinaryOperation {
                op: Op::Gt,
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
            (Value::Int(lhs), Value::Float(rhs)) => Ok(Value::Bool(*lhs as f32 >= *rhs)),
            (Value::Float(lhs), Value::Int(rhs)) => Ok(Value::Bool(*lhs >= *rhs as f32)),
            _ => Err(TypeError::UnsupportedBinaryOperation {
                op: Op::Gte,
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
            (Value::Int(lhs), Value::Float(rhs)) => Ok(Value::Bool((*lhs as f32) < *rhs)),
            (Value::Float(lhs), Value::Int(rhs)) => Ok(Value::Bool(*lhs < *rhs as f32)),
            _ => Err(TypeError::UnsupportedBinaryOperation {
                op: Op::Lt,
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
            (Value::Int(lhs), Value::Float(rhs)) => Ok(Value::Bool(*lhs as f32 <= *rhs)),
            (Value::Float(lhs), Value::Int(rhs)) => Ok(Value::Bool(*lhs <= *rhs as f32)),
            _ => Err(TypeError::UnsupportedBinaryOperation {
                op: Op::Lte,
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
    pub fn join(&self, other: &Value) -> Result<Value, Error> {
        match (self, other) {
            (Value::String(lhs), Value::String(rhs)) => {
                Ok(Value::String(Rc::new(lhs.to_string() + rhs)))
            }
            (Value::String(lhs), Value::Int(rhs)) => {
                Ok(Value::String(Rc::new(lhs.to_string() + &rhs.to_string())))
            }
            (Value::String(lhs), Value::Float(rhs)) => {
                Ok(Value::String(Rc::new(lhs.to_string() + &rhs.to_string())))
            }
            (Value::String(lhs), Value::Bool(rhs)) => {
                Ok(Value::String(Rc::new(lhs.to_string() + &rhs.to_string())))
            }
            (Value::String(lhs), Value::None) => {
                Ok(Value::String(Rc::new(lhs.to_string() + "none")))
            }
            (Value::Int(lhs), Value::String(rhs)) => {
                Ok(Value::String(Rc::new(lhs.to_string() + rhs)))
            }
            (Value::Float(lhs), Value::String(rhs)) => {
                Ok(Value::String(Rc::new(lhs.to_string() + rhs)))
            }
            (Value::Bool(lhs), Value::String(rhs)) => {
                Ok(Value::String(Rc::new(lhs.to_string() + rhs)))
            }

            (Value::List(lhs), Value::List(rhs)) => {
                let mut new_list = (**lhs).clone();
                new_list.extend((**rhs).clone());
                Ok(Value::List(Rc::new(new_list)))
            }

            _ => Err(TypeError::UnsupportedBinaryOperation {
                op: Op::Join,
                lhs: self.ntype(),
                rhs: other.ntype(),
            }
            .into()),
        }
    }
}
