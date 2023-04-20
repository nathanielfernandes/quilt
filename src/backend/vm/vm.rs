use std::{collections::hash_map::Entry, rc::Rc};

use arrayvec::ArrayVec;
use fxhash::FxHashMap;

use crate::prelude::{Error, ErrorS, NameError, OverflowError, TypeError};

use super::{
    bytecode::*,
    compiler::VmEntry,
    value::{Closure, Upvalue, Value},
};

pub struct VM<Data, const STACK_SIZE: usize, const CALL_STACK_SIZE: usize> {
    pub data: Data, // any data that the VM builtins needs to access

    globals: FxHashMap<u16, Value>,
    global_symbols: Vec<String>,

    open_upvalues: Vec<Upvalue>,

    frames: ArrayVec<CallFrame, CALL_STACK_SIZE>,
    frame: CallFrame,

    stack: Vec<Value>,
    sp: usize,
}

impl<Data, const SS: usize, const CSS: usize> VM<Data, SS, CSS> {
    pub fn new(data: Data, entry: VmEntry) -> Self {
        Self {
            data,

            globals: FxHashMap::default(),
            global_symbols: entry.global_symbols,

            open_upvalues: Vec::with_capacity(256),

            frames: ArrayVec::new(),
            frame: {
                CallFrame {
                    closure: Rc::new(Closure {
                        function: Rc::new(entry.function),
                        upvalues: Vec::new(),
                    }),
                    ip: 0,
                    st: 0,
                }
            },

            stack: {
                let mut stack = Vec::with_capacity(SS);
                stack.resize(SS, Value::None);
                stack
            },
            sp: 0,
        }
    }

    pub fn run(&mut self) -> Result<Value, ErrorS> {
        loop {
            if self.frame.ip >= self.frame.closure.function.chunk.len() {
                return Err(self.error(OverflowError::InstructionOverflow.into()));
            }

            #[allow(non_upper_case_globals)]
            match self.read_u8() {
                Halt => {
                    println!("Halt");
                    break;
                }

                Return => {
                    let result = self.pop()?.clone();
                    self.close_upvalues(self.frame.st);

                    // clean up stack
                    // for i in self.frame.st..self.sp {
                    //     self.stack[i] = Value::None;
                    // }
                    self.sp = self.frame.st;

                    match self.frames.pop() {
                        Some(frame) => {
                            self.frame = frame;
                            self.push(result)?;
                        }
                        None => {
                            return Ok(result);
                        }
                    }
                }

                Pop => {
                    self.pop()?;
                }

                LoadConst => {
                    let constant = self.read_constant().clone();
                    self.push(constant)?;
                }

                LoadLocal => {
                    let idx = self.read_u16() as usize;
                    let value = self.stack[self.frame.st + idx].clone();
                    self.push(value)?;
                }

                SetLocal => {
                    let idx = self.read_u16() as usize;
                    let value = self.peek(0)?;

                    self.stack[self.frame.st + idx] = value.clone();
                }

                LoadGlobal => {
                    // let name = self.read_symbol().to_string();
                    let name = self.read_u16();

                    match self.globals.get(&name) {
                        Some(value) => self.push(value.clone())?,
                        None => {
                            let name = self
                                .global_symbols
                                .get(name as usize)
                                .cloned()
                                .unwrap_or(String::from("unkown???"));

                            return Err(self.error_1(NameError::Undefined(name).into()));
                        }
                    }
                }

                DefineGlobal => {
                    // let name = self.read_symbol().to_string();
                    let name = self.read_u16();

                    let value = self.pop()?.clone();
                    self.globals.insert(name, value);
                }

                SetGlobal => {
                    // let name = self.read_symbol().to_string();
                    let name = self.read_u16();
                    let value = self.peek(0)?.clone();

                    match self.globals.entry(name) {
                        Entry::Occupied(mut entry) => {
                            entry.insert(value);
                        }
                        Entry::Vacant(entry) => {
                            return Err({
                                let span = self
                                    .frame
                                    .closure
                                    .function
                                    .chunk
                                    .get_span(self.frame.ip - 1);

                                let name = self
                                    .global_symbols
                                    .get(*entry.key() as usize)
                                    .cloned()
                                    .unwrap_or(String::from("unkown???"));

                                (NameError::Undefined(name).into(), span)
                            })
                        }
                    }
                }

                LoadUpvalue => {
                    let idx = self.read_u16() as usize;
                    let upvalue = &self.frame.closure.upvalues[idx];
                    let value = upvalue.get(&self.stack);
                    self.push(value)?;
                }

                CloseUpvalue => {
                    self.close_upvalues(self.sp - 1);
                    self.pop()?;
                }

                CreateFunction => {
                    let idx = self.read_u16();
                    let value = self.get_constant(idx);
                    let function = match value {
                        Value::Function(function) => function.clone(),
                        _ => return Err(self.error(TypeError::NotCallable(value.ntype()).into())),
                    };

                    let closure = Closure {
                        function,
                        upvalues: Vec::new(),
                    };

                    self.push(Value::Closure(closure.into()))?;
                }

                CreateClosure => {
                    let idx = self.read_u16();
                    let value = self.get_constant(idx);
                    let function = match value {
                        Value::Function(function) => function.clone(),
                        _ => return Err(self.error(TypeError::NotCallable(value.ntype()).into())),
                    };

                    let mut upvalues = Vec::with_capacity(function.upvalue_count as usize);
                    for _ in 0..function.upvalue_count {
                        let is_local = self.read_u8();
                        let index = self.read_u16() as usize;

                        let upvalue: Upvalue = if is_local != 0 {
                            self.capture_upvalue(self.frame.st + index)
                        } else {
                            self.frame.closure.upvalues[index].clone()
                        };

                        upvalues.push(upvalue);
                    }

                    let closure = Closure { function, upvalues };
                    self.push(Value::Closure(Rc::new(closure)))?;
                }

                CallFunction => {
                    let argc = self.read_u8();
                    let value = self.peek(argc as usize)?;

                    if self.frames.len() >= CSS {
                        return Err(self.error(OverflowError::StackOverflow.into()));
                    }

                    match value {
                        Value::Closure(closure) => {
                            if argc != closure.function.arity {
                                return Err(self.error(
                                    TypeError::MismatchedArity {
                                        name: closure.function.name.0.clone(),
                                        expected: closure.function.arity,
                                        got: argc,
                                    }
                                    .into(),
                                ));
                            }

                            let frame = CallFrame {
                                closure: closure.clone(),
                                ip: 0,
                                st: self.sp - argc as usize - 1,
                            };

                            unsafe {
                                self.frames
                                    .push_unchecked(std::mem::replace(&mut self.frame, frame))
                            }
                        }
                        Value::Function(function) => {
                            if argc != function.arity {
                                return Err(self.error(
                                    TypeError::MismatchedArity {
                                        name: function.name.0.clone(),
                                        expected: function.arity,
                                        got: argc,
                                    }
                                    .into(),
                                ));
                            }

                            let closure = Closure {
                                function: function.clone(),
                                upvalues: Vec::new(),
                            };

                            let frame = CallFrame {
                                closure: Rc::new(closure),
                                ip: 0,
                                st: self.sp - argc as usize - 1,
                            };

                            unsafe {
                                self.frames
                                    .push_unchecked(std::mem::replace(&mut self.frame, frame))
                            }
                        }

                        _ => Err(self.error(TypeError::NotCallable(value.ntype()).into()))?,
                    }
                }

                Jump => {
                    let offset = self.read_u16() as usize;
                    self.frame.ip += offset;
                }
                JumpIfFalse => {
                    let offset = self.read_u16() as usize;
                    let value = self.peek(0)?;

                    if !value.is_truthy().map_err(|e| self.error_1(e))? {
                        self.frame.ip += offset;
                    }
                }

                UnaryNegate => {
                    let value = self.pop()?.negate().map_err(|e| self.error_1(e))?;
                    self.push(value)?;
                }

                UnaryNot => {
                    let value = self.pop()?.not().map_err(|e| self.error_1(e))?;
                    self.push(value)?;
                }

                // UnarySpread => {
                //     let value = self.pop()?.spread().map_err(|e| self.error_1(e))?;
                //     self.push(value)?;
                // }
                BinaryAdd => {
                    let (lhs, rhs) = self.pop_double_ref()?;
                    let value = lhs.add(rhs).map_err(|e| self.error_1(e))?;

                    self.push(value)?;
                }

                BinarySubtract => {
                    let (lhs, rhs) = self.pop_double_ref()?;
                    let value = lhs.subtract(rhs).map_err(|e| self.error_1(e))?;

                    self.push(value)?;
                }
                BinaryMultiply => {
                    let (lhs, rhs) = self.pop_double_ref()?;

                    let value = lhs.multiply(&rhs).map_err(|e| self.error_1(e))?;
                    self.push(value)?;
                }

                BinaryDivide => {
                    let (lhs, rhs) = self.pop_double_ref()?;

                    let value = lhs.divide(rhs).map_err(|e| self.error_1(e))?;
                    self.push(value)?;
                }

                BinaryModulo => {
                    let (lhs, rhs) = self.pop_double_ref()?;

                    let value = lhs.modulo(rhs).map_err(|e| self.error_1(e))?;
                    self.push(value)?;
                }

                BinaryPower => {
                    let (lhs, rhs) = self.pop_double_ref()?;

                    let value = lhs.power(rhs).map_err(|e| self.error_1(e))?;
                    self.push(value)?;
                }

                BinaryEqual => {
                    let (lhs, rhs) = self.pop_double_ref()?;

                    let value = lhs.equal(rhs).map_err(|e| self.error_1(e))?;
                    self.push(value)?;
                }

                BinaryNotEqual => {
                    let (lhs, rhs) = self.pop_double_ref()?;

                    let value = lhs.not_equal(rhs).map_err(|e| self.error_1(e))?;
                    self.push(value)?;
                }

                BinaryLess => {
                    let (lhs, rhs) = self.pop_double_ref()?;

                    let value = lhs.less_than(rhs).map_err(|e| self.error_1(e))?;

                    self.push(value)?;
                }

                BinaryLessEqual => {
                    let (lhs, rhs) = self.pop_double_ref()?;

                    let value = lhs.less_than_or_equal(rhs).map_err(|e| self.error_1(e))?;
                    self.push(value)?;
                }

                BinaryGreater => {
                    let (lhs, rhs) = self.pop_double_ref()?;

                    let value = lhs.greater_than(rhs).map_err(|e| self.error_1(e))?;
                    self.push(value)?;
                }

                BinaryGreaterEqual => {
                    let (lhs, rhs) = self.pop_double_ref()?;

                    let value = lhs
                        .greater_than_or_equal(rhs)
                        .map_err(|e| self.error_1(e))?;
                    self.push(value)?;
                }

                BinaryAnd => {
                    let (lhs, rhs) = self.pop_double_ref()?;

                    let value = lhs.and(rhs).map_err(|e| self.error_1(e))?;
                    self.push(value)?;
                }

                BinaryOr => {
                    let (lhs, rhs) = self.pop_double_ref()?;

                    let value = lhs.or(rhs).map_err(|e| self.error_1(e))?;
                    self.push(value)?;
                }

                BinaryJoin => {
                    let (lhs, rhs) = self.pop_double_ref()?;

                    let value = lhs.join(rhs).map_err(|e| self.error_1(e))?;
                    self.push(value)?;
                }

                op => {
                    unimplemented!("Opcode: {:?}", OpCode::from(op));
                }
            }
        }

        Ok(Value::None)
    }

    fn capture_upvalue(&mut self, location: usize) -> Upvalue {
        for upvalue in self.open_upvalues.iter_mut() {
            if let Upvalue::Open(loc) = upvalue {
                if *loc == location {
                    return upvalue.clone();
                }
            }
        }

        let upvalue = Upvalue::new(location);
        self.open_upvalues.push(upvalue.clone());

        upvalue
    }

    fn close_upvalues(&mut self, last: usize) {
        if last >= self.stack.len() {
            return;
        }

        for idx in (0..self.open_upvalues.len()).rev() {
            let upvalue = &mut self.open_upvalues[idx];
            if let Upvalue::Open(l) = upvalue {
                if *l >= last {
                    let value = self.stack[*l].clone();
                    *upvalue = Upvalue::Closed(value);

                    self.open_upvalues.swap_remove(idx);
                }
            }
        }
    }

    #[inline]
    pub fn error(&self, error: Error) -> ErrorS {
        let span = self.frame.closure.function.chunk.get_span(self.frame.ip);
        (error, span)
    }

    #[inline]
    pub fn error_1(&self, error: Error) -> ErrorS {
        let span = self
            .frame
            .closure
            .function
            .chunk
            .get_span(self.frame.ip.saturating_sub(1));
        (error, span)
    }

    #[inline]
    pub fn push(&mut self, value: Value) -> Result<(), ErrorS> {
        if self.sp > SS {
            return Err(self.error(OverflowError::StackOverflow.into()));
        }

        self.stack[self.sp] = value;
        self.sp += 1;

        Ok(())
    }

    #[inline]
    pub fn pop(&mut self) -> Result<&Value, ErrorS> {
        if self.sp == 0 {
            return Err(self.error(OverflowError::StackUnderflow.into()));
        }
        self.sp -= 1;

        // let popped = std::mem::replace(&mut self.stack[self.sp], Value::None);
        // let popped = self.stack[self.sp].clone();

        Ok(&self.stack[self.sp])
    }

    #[inline]
    pub fn pop_double_ref(&mut self) -> Result<(&Value, &Value), ErrorS> {
        if self.sp < 2 {
            return Err(self.error(OverflowError::StackUnderflow.into()));
        }
        self.sp -= 2;

        Ok((&self.stack[self.sp], &self.stack[self.sp + 1]))
    }

    #[inline]
    pub fn peek(&self, distance: usize) -> Result<&Value, ErrorS> {
        if self.sp < distance + 1 {
            return Err(self.error(OverflowError::StackUnderflow.into()));
        }

        Ok(&self.stack[self.sp - distance - 1])
    }

    #[inline]
    pub fn read_u8(&mut self) -> u8 {
        let value = self.frame.closure.function.chunk.ops.read_u8(self.frame.ip);
        self.frame.ip += 1;
        value
    }

    #[inline]
    pub fn read_u16(&mut self) -> u16 {
        let value = self
            .frame
            .closure
            .function
            .chunk
            .ops
            .read_u16(self.frame.ip);
        self.frame.ip += 2;
        value
    }

    #[inline]
    pub fn read_constant(&mut self) -> &Value {
        let idx = self.read_u16();
        &self.frame.closure.function.chunk.constants[idx as usize]
    }

    #[inline]
    pub fn get_constant(&self, idx: u16) -> &Value {
        &self.frame.closure.function.chunk.constants[idx as usize]
    }

    #[inline]
    pub fn read_symbol(&mut self) -> &str {
        let idx = self.read_u16();
        &self.frame.closure.function.chunk.symbols[idx as usize]
    }

    #[inline]
    pub fn get_symbol(&self, idx: u16) -> &str {
        &self.frame.closure.function.chunk.symbols[idx as usize]
    }

    // pub fn profile(
    //     &mut self,
    // ) -> Result<(Value, FxHashMap<u8, (usize, std::time::Duration)>), ErrorS> {
    //     let mut profile: FxHashMap<u8, (usize, std::time::Duration)> = FxHashMap::default();
    //     let mut return_value = Value::None;
    //     loop {
    //         if self.frame.ip >= self.frame.closure.function.chunk.len() {
    //             return Err(self.error(OverflowError::InstructionOverflow.into()));
    //         }

    //         let op = self.read_u8();

    //         let start = std::time::Instant::now();

    //         #[allow(non_upper_case_globals)]
    //         match op {
    //             Halt => {
    //                 println!("Halt");
    //                 break;
    //             }

    //             Return => {
    //                 let result = self.pop()?.clone();
    //                 self.close_upvalues(self.frame.st);

    //                 // clean up stack
    //                 // for i in self.frame.st..self.sp {
    //                 //     self.stack[i] = Value::None;
    //                 // }
    //                 self.sp = self.frame.st;

    //                 // self.stack.truncate(self.frame.st);

    //                 match self.frames.pop() {
    //                     Some(frame) => {
    //                         self.frame = frame;
    //                         self.push(result)?;
    //                     }
    //                     None => {
    //                         return_value = result;
    //                         break;
    //                     }
    //                 }
    //             }

    //             Pop => {
    //                 self.pop()?;
    //             }

    //             LoadConst => {
    //                 let constant = self.read_constant().clone();
    //                 self.push(constant)?;
    //             }

    //             LoadLocal => {
    //                 let idx = self.read_u16() as usize;
    //                 let value = &self.stack[self.frame.st + idx];
    //                 self.push(value.clone())?;
    //             }

    //             SetLocal => {
    //                 let idx = self.read_u16() as usize;
    //                 let value = self.peek(0)?;

    //                 self.stack[self.frame.st + idx] = value.clone();
    //             }

    //             LoadGlobal => {
    //                 let name = self.read_symbol().to_string();

    //                 match self.globals.get(&name) {
    //                     Some(value) => self.push(value.clone())?,
    //                     None => return Err(self.error(NameError::Undefined(name).into())),
    //                 }
    //             }

    //             DefineGlobal => {
    //                 let name = self.read_symbol().to_string();
    //                 let value = self.pop()?.clone();
    //                 self.globals.insert(name, value);
    //             }

    //             SetGlobal => {
    //                 let name = self.read_symbol().to_string();
    //                 let value = self.peek(0)?.clone();

    //                 match self.globals.entry(name) {
    //                     Entry::Occupied(mut entry) => {
    //                         entry.insert(value);
    //                     }
    //                     Entry::Vacant(entry) => Err({
    //                         let span = self.frame.closure.function.chunk.get_span(self.frame.ip);
    //                         (NameError::Undefined(entry.key().to_string()).into(), span)
    //                     })?,
    //                 }
    //             }

    //             LoadUpvalue => {
    //                 let idx = self.read_u16() as usize;
    //                 let upvalue = &self.frame.closure.upvalues[idx];
    //                 let value = upvalue.get(&self.stack);
    //                 self.push(value)?;
    //             }

    //             CloseUpvalue => {
    //                 self.close_upvalues(self.sp - 1);
    //                 self.pop()?;
    //             }

    //             CreateFunction => {
    //                 let value = self.read_constant().clone();
    //                 let function = match value {
    //                     Value::Function(function) => function,
    //                     _ => Err(self.error(TypeError::NotCallable(value.ntype()).into()))?,
    //                 };
    //                 self.push(Value::Function(function))?;
    //             }

    //             CreateClosure => {
    //                 let value = self.read_constant().clone();
    //                 let function = match value {
    //                     Value::Function(function) => function,
    //                     _ => Err(self.error(TypeError::NotCallable(value.ntype()).into()))?,
    //                 };
    //                 let mut upvalues = Vec::with_capacity(function.upvalue_count as usize);

    //                 for _ in 0..function.upvalue_count {
    //                     let is_local = self.read_u8();
    //                     let index = self.read_u16() as usize;

    //                     let upvalue: Upvalue = if is_local != 0 {
    //                         self.capture_upvalue(self.frame.st + index)
    //                     } else {
    //                         self.frame.closure.upvalues[index].clone()
    //                     };

    //                     upvalues.push(upvalue);
    //                 }

    //                 let closure = Closure { function, upvalues };
    //                 self.push(Value::Closure(Rc::new(closure)))?;
    //             }

    //             CallFunction => {
    //                 let argc = self.read_u8();
    //                 let value = self.peek(argc as usize)?.clone();

    //                 if self.frames.len() >= CSS {
    //                     return Err(self.error(OverflowError::StackOverflow.into()));
    //                 }

    //                 match value {
    //                     Value::Closure(closure) => {
    //                         if argc != closure.function.arity {
    //                             Err(self.error(
    //                                 TypeError::MismatchedArity {
    //                                     name: closure.function.name.0.clone(),
    //                                     expected: closure.function.arity,
    //                                     got: argc,
    //                                 }
    //                                 .into(),
    //                             ))?;
    //                         }

    //                         let frame = CallFrame {
    //                             closure,
    //                             ip: 0,
    //                             st: self.sp - argc as usize - 1,
    //                         };

    //                         unsafe {
    //                             self.frames
    //                                 .push_unchecked(std::mem::replace(&mut self.frame, frame))
    //                         }
    //                     }
    //                     Value::Function(function) => {
    //                         if argc != function.arity {
    //                             Err(self.error(
    //                                 TypeError::MismatchedArity {
    //                                     name: function.name.0.clone(),
    //                                     expected: function.arity,
    //                                     got: argc,
    //                                 }
    //                                 .into(),
    //                             ))?;
    //                         }

    //                         let closure = Closure {
    //                             function,
    //                             upvalues: Vec::new(),
    //                         };

    //                         let frame = CallFrame {
    //                             closure: Rc::new(closure),
    //                             ip: 0,
    //                             st: self.sp - argc as usize - 1,
    //                         };

    //                         unsafe {
    //                             self.frames
    //                                 .push_unchecked(std::mem::replace(&mut self.frame, frame))
    //                         }
    //                     }

    //                     _ => Err(self.error(TypeError::NotCallable(value.ntype()).into()))?,
    //                 }
    //             }

    //             Jump => {
    //                 let offset = self.read_u16() as usize;
    //                 self.frame.ip += offset;
    //             }
    //             JumpIfFalse => {
    //                 let offset = self.read_u16() as usize;
    //                 let value = self.peek(0)?;

    //                 if !value.is_truthy().map_err(|e| self.error_1(e))? {
    //                     self.frame.ip += offset;
    //                 }
    //             }

    //             UnaryNegate => {
    //                 let value = self.pop()?.negate().map_err(|e| self.error_1(e))?;
    //                 self.push(value)?;
    //             }

    //             UnaryNot => {
    //                 let value = self.pop()?.not().map_err(|e| self.error_1(e))?;
    //                 self.push(value)?;
    //             }

    //             // UnarySpread => {
    //             //     let value = self.pop()?.spread().map_err(|e| self.error_1(e))?;
    //             //     self.push(value)?;
    //             // }
    //             BinaryAdd => {
    //                 let rhs = self.pop()?;
    //                 let lhs = self.pop()?;

    //                 let value = lhs.add(&rhs).map_err(|e| self.error_1(e))?;
    //                 self.push(value)?;
    //             }

    //             BinarySubtract => {
    //                 let value = {
    //                     let rhs = &self.stack[self.sp - 1];
    //                     let lhs = &self.stack[self.sp - 2];

    //                     lhs.subtract(rhs).map_err(|e| self.error_1(e))?
    //                 };

    //                 self.weak_pop(2)?;
    //                 self.push(value)?;
    //             }

    //             BinaryMultiply => {
    //                 let rhs = self.pop()?;
    //                 let lhs = self.pop()?;

    //                 let value = lhs.multiply(&rhs).map_err(|e| self.error_1(e))?;
    //                 self.push(value)?;
    //             }

    //             BinaryDivide => {
    //                 let rhs = self.pop()?;
    //                 let lhs = self.pop()?;

    //                 let value = lhs.divide(&rhs).map_err(|e| self.error_1(e))?;
    //                 self.push(value)?;
    //             }

    //             BinaryModulo => {
    //                 let rhs = self.pop()?;
    //                 let lhs = self.pop()?;

    //                 let value = lhs.modulo(&rhs).map_err(|e| self.error_1(e))?;
    //                 self.push(value)?;
    //             }

    //             BinaryPower => {
    //                 let rhs = self.pop()?;
    //                 let lhs = self.pop()?;

    //                 let value = lhs.power(&rhs).map_err(|e| self.error_1(e))?;
    //                 self.push(value)?;
    //             }

    //             BinaryEqual => {
    //                 let rhs = self.pop()?;
    //                 let lhs = self.pop()?;

    //                 let value = lhs.equal(&rhs).map_err(|e| self.error_1(e))?;
    //                 self.push(value)?;
    //             }

    //             BinaryNotEqual => {
    //                 let rhs = self.pop()?;
    //                 let lhs = self.pop()?;

    //                 let value = lhs.not_equal(&rhs).map_err(|e| self.error_1(e))?;
    //                 self.push(value)?;
    //             }

    //             BinaryLess => {
    //                 let value = {
    //                     let rhs = &self.stack[self.sp - 1];
    //                     let lhs = &self.stack[self.sp - 2];

    //                     lhs.less_than(rhs).map_err(|e| self.error_1(e))?
    //                 };

    //                 self.weak_pop(2)?;
    //                 self.push(value)?;
    //             }

    //             BinaryLessEqual => {
    //                 let rhs = self.pop()?;
    //                 let lhs = self.pop()?;

    //                 let value = lhs.less_than_or_equal(&rhs).map_err(|e| self.error_1(e))?;
    //                 self.push(value)?;
    //             }

    //             BinaryGreater => {
    //                 let rhs = self.pop()?;
    //                 let lhs = self.pop()?;

    //                 let value = lhs.greater_than(&rhs).map_err(|e| self.error_1(e))?;
    //                 self.push(value)?;
    //             }

    //             BinaryGreaterEqual => {
    //                 let rhs = self.pop()?;
    //                 let lhs = self.pop()?;

    //                 let value = lhs
    //                     .greater_than_or_equal(&rhs)
    //                     .map_err(|e| self.error_1(e))?;
    //                 self.push(value)?;
    //             }

    //             BinaryAnd => {
    //                 let rhs = self.pop()?;
    //                 let lhs = self.pop()?;

    //                 let value = lhs.and(&rhs).map_err(|e| self.error_1(e))?;
    //                 self.push(value)?;
    //             }

    //             BinaryOr => {
    //                 let rhs = self.pop()?;
    //                 let lhs = self.pop()?;

    //                 let value = lhs.or(&rhs).map_err(|e| self.error_1(e))?;
    //                 self.push(value)?;
    //             }

    //             BinaryJoin => {
    //                 let rhs = self.pop()?;
    //                 let lhs = self.pop()?;

    //                 let value = lhs.join(&rhs).map_err(|e| self.error_1(e))?;
    //                 self.push(value)?;
    //             }

    //             op => {
    //                 unimplemented!("Opcode: {:?}", OpCode::from(op));
    //             }
    //         }

    //         let elapsed = start.elapsed();

    //         match profile.entry(op) {
    //             Entry::Occupied(entry) => {
    //                 let (c, time) = entry.into_mut();
    //                 *time += elapsed;
    //                 *c += 1
    //             }
    //             Entry::Vacant(entry) => {
    //                 entry.insert((1, elapsed));
    //             }
    //         }
    //     }

    //     Ok((return_value, profile))
    // }
}

pub struct CallFrame {
    pub closure: Rc<Closure>,
    pub ip: usize,
    pub st: usize,
}
