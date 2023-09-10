use std::{cell::RefCell, collections::hash_map::Entry, rc::Rc};

use bytecode::bytecode::*;
use common::{error::*, pool::Pool, span::Span, vecc::Vecc};
use fxhash::FxHashMap;

use crate::{
    arith::fix_index,
    builtins::{BuiltinAdderFn, BuiltinFn, BuiltinFnMap, BuiltinList, ContextExit, VmData},
    value::{Closure, Upvalue, Value},
    Script,
};

pub struct VmOptions {
    // Note: max_runtime is checked every 64 instructions,
    // so it may exceed the max_runtime by up to 64 instructions
    pub max_runtime: std::time::Duration,
    pub stack_size: usize,
    pub call_stack_size: usize,
    pub string_max_size: usize,
    pub array_max_size: usize,
}

impl VmOptions {
    pub fn build() -> Self {
        Self::default()
    }

    pub fn with_max_runtime(mut self, max_runtime: std::time::Duration) -> Self {
        self.max_runtime = max_runtime;
        self
    }

    pub fn with_stack_size(mut self, stack_size: usize) -> Self {
        self.stack_size = stack_size;
        self
    }

    pub fn with_call_stack_size(mut self, call_stack_size: usize) -> Self {
        self.call_stack_size = call_stack_size;
        self
    }

    pub fn with_string_max_size(mut self, string_max_size: usize) -> Self {
        self.string_max_size = string_max_size;
        self
    }

    pub fn with_array_max_size(mut self, array_max_size: usize) -> Self {
        self.array_max_size = array_max_size;
        self
    }
}

impl Default for VmOptions {
    fn default() -> Self {
        Self {
            max_runtime: std::time::Duration::MAX,
            stack_size: 500000, // roughly 8mb
            call_stack_size: 1000,
            string_max_size: 1024 * 256, // 256kb
            array_max_size: 1024 * 256,  // 256kb
        }
    }
}

pub struct VM<Data = ()>
where
    Data: VmData,
{
    pub data: Data, // any data that the VM builtins needs to access

    builtins: BuiltinFnMap<Data>,
    exit_fn_stack: Vec<ContextExit<Data>>,
    block_results: Vec<Value>,

    globals: FxHashMap<u16, Value>,
    global_symbols: Pool<String, u16>,

    open_upvalues: Vec<Rc<RefCell<Upvalue>>>,

    frames: Vec<CallFrame>,
    frame: CallFrame,

    stack: Vec<Value>,
    sp: usize,

    start_time: std::time::Instant,
    options: VmOptions,
}

impl<Data> VM<Data>
where
    Data: VmData,
{
    pub fn new(data: Data, script: Script, options: VmOptions) -> Self {
        Self {
            data,

            builtins: BuiltinFnMap::default(),
            exit_fn_stack: Vec::with_capacity(256),
            block_results: Vec::with_capacity(256),

            globals: FxHashMap::default(),
            global_symbols: script.global_symbols,

            open_upvalues: Vec::with_capacity(256),

            frames: Vec::with_capacity(options.call_stack_size),
            frame: {
                CallFrame {
                    closure: Closure {
                        function: Rc::new(script.function),
                        upvalues: Rc::new(Vec::new()),
                    },
                    ip: 0,
                    st: 0,
                }
            },

            stack: {
                let mut stack = Vec::with_capacity(options.stack_size);
                stack.resize(options.stack_size, Value::None);
                stack
            },
            sp: 0,

            start_time: std::time::Instant::now(),
            options,
        }
    }

    pub fn reset_state(&mut self) {
        self.sp = 0;
        self.frames.clear();
        self.frame.ip = 0;
        self.frame.st = 0;
        self.open_upvalues.clear();
        self.block_results.clear();
        self.exit_fn_stack.clear();
    }

    pub fn update_script(&mut self, script: Script) {
        self.reset_state();

        self.frame.closure.function = Rc::new(script.function);
        self.global_symbols = script.global_symbols;
    }

    pub fn new_with(
        data: Data,
        script: Script,
        options: VmOptions,
        builtins: &[BuiltinAdderFn<Data>],
    ) -> Self {
        let mut vm = Self::new(data, script, options);
        builtins
            .iter()
            .for_each(|add: &fn(&mut VM<Data>)| add(&mut vm));
        vm
    }

    #[inline]
    pub fn reserve_globals(&mut self, n: usize) {
        self.global_symbols.reserve(n);
        self.globals.reserve(n);
    }

    #[inline]
    pub fn insert_global<S: Into<String>, V: Into<Value>>(&mut self, name: S, value: V) {
        let id = self.global_symbols.add(name.into());
        self.globals.insert(id, value.into());
    }

    #[inline]
    pub fn add_builtins(&mut self, builtins: BuiltinAdderFn<Data>) {
        builtins(self);
    }

    #[inline]
    pub fn add_builtins_list<const N: usize>(&mut self, builtins: BuiltinList<N, Data>) {
        self.global_symbols.reserve(N);
        self.builtins.reserve(N);
        for (name, func) in builtins {
            let name = self.global_symbols.add(name);
            self.builtins.insert(name, func);
        }
    }

    #[inline]
    pub fn finish(self) -> Data {
        self.data
    }

    pub fn trace_back(&self) -> TraceBack {
        let mut trace = Vec::new();

        // push the current frame
        let name = self.frame.closure.function.name.0.clone();
        let span = self
            .frame
            .closure
            .function
            .chunk
            .get_span(self.frame.ip.saturating_sub(1));
        trace.push((name, span));

        for frame in self.frames.iter().skip(1).rev().take(8) {
            let name = frame.closure.function.name.0.clone();
            let span = frame
                .closure
                .function
                .chunk
                .get_span(frame.ip.saturating_sub(1));
            trace.push((name, span));
        }

        if self.frames.len() > 10 {
            let n = self.frames.len() - 10;
            let s = if n == 1 { "" } else { "s" };
            trace.push((format!("... {} more frame{}", n, s), Span::default()));
        }

        // push the main frame
        if let Some(frame) = self.frames.first() {
            let name = frame.closure.function.name.0.clone();
            let span = frame.closure.function.chunk.get_span(frame.ip);
            trace.push((name, span));
        }

        trace
    }

    pub fn run(&mut self) -> Result<Value, ErrorS> {
        self.start_time = std::time::Instant::now();
        let mut n = 0;
        loop {
            if n > 64 {
                self.has_runtime_exceeded()?;

                n = 0;
            } else {
                n += 1;
            }

            if self.frame.ip >= self.frame.closure.function.chunk.len() {
                return Err(self.error(OverflowError::InstructionOverflow.into()));
            }

            // let op = self.read_u8();
            // println!(
            //     "stack [{}]",
            //     &self.stack[self.frame.st..self.sp]
            //         .iter()
            //         .map(|v| v.display())
            //         .collect::<Vec<_>>()
            //         .join(", ")
            // );
            // println!("{:?} ", OpCode::from(op));

            #[allow(non_upper_case_globals)]
            match self.read_u8() {
                Halt => {
                    return Err(self.error_1(Error::Halt));
                }
                LoadNone => {
                    self.push(Value::None)?;
                }

                LoadNoneMany => {
                    let count = self.read_u8();
                    self.push_many(count as usize)?;
                }

                Return => {
                    let result = self.pop()?.clone();
                    self.close_upvalues(self.frame.st);

                    for idx in self.frame.st..self.sp {
                        self.close_upvalues(idx);
                    }

                    // clean up stack
                    // for i in self.frame.st..self.sp {
                    //     self.stack[i] = Value::None;
                    // }

                    // println!("sp: {}", self.sp);

                    match self.frames.pop() {
                        Some(frame) => {
                            self.data.on_exit_function();
                            self.sp = self.frame.st;
                            self.frame = frame;
                            self.push(result)?;
                        }
                        None => {
                            debug_assert!(self.sp == 0, "sp: {}, stack not empty!", self.sp);

                            self.sp = self.frame.st;
                            return Ok(result);
                        }
                    }
                }

                BlockResult => {
                    let result = self.pop()?.clone();
                    self.block_results.push(result);
                }

                BlockReturn => {
                    if let Some(result) = self.block_results.pop() {
                        self.push(result)?;
                    } else {
                        return Err(self.error(OverflowError::StackUnderflow.into()));
                    }
                }

                Pop => {
                    self.pop()?;
                }

                Swap => {
                    // swaps the top 2 values on the stack
                    self.swap()?;
                }

                SwapPop => {
                    self.swap()?;
                    self.pop()?;
                }

                PopMany => {
                    let count = self.read_u16() as usize;
                    self.pop_many(count)?;
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
                                .get(name)
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
                                    .get(*entry.key())
                                    .cloned()
                                    .unwrap_or(String::from("unkown???"));

                                (NameError::Undefined(name).into(), span)
                            })
                        }
                    }
                }

                SetUpvalue => {
                    let idx = self.read_u16() as usize;
                    let value = self.peek(0)?.clone();
                    let upvalue = &self.frame.closure.upvalues[idx];
                    {
                        upvalue.borrow_mut().set(&mut self.stack, value);
                    }
                }

                LoadUpvalue => {
                    let idx = self.read_u16() as usize;
                    let upvalue = &self.frame.closure.upvalues[idx];
                    {
                        let value = upvalue.borrow().get(&self.stack);
                        self.push(value)?;
                    }
                }

                IndexGet => {
                    let value = self.pop()?.clone();
                    let index = self.pop()?.clone();

                    let idx = match index {
                        Value::Int(n) => n,
                        _ => {
                            return Err(
                                self.error_1(TypeError::Expected("int", index.ntype()).into())
                            );
                        }
                    };

                    match value {
                        Value::Array(array) => {
                            let idx =
                                fix_index(idx, array.len()).map_err(|e| self.error_1(e.into()))?;
                            let value = array[idx].clone();
                            self.push(value)?;
                        }
                        Value::String(string) => {
                            let chars = string.chars().collect::<Vec<_>>();
                            let idx =
                                fix_index(idx, chars.len()).map_err(|e| self.error_1(e.into()))?;
                            let value = chars[idx].to_string();
                            self.push(Value::String(Rc::new(value)))?;
                        }
                        Value::Color(color) => {
                            let idx = fix_index(idx, 4).map_err(|e| self.error_1(e.into()))?;
                            let value = color[idx].clone();
                            self.push(value.into())?;
                        }
                        Value::Pair(pair) => {
                            let (a, b) = pair.as_ref();
                            let idx = fix_index(idx, 2).map_err(|e| self.error_1(e.into()))?;

                            let value = match idx {
                                0 => a.clone(),
                                1 => b.clone(),
                                _ => unreachable!(),
                            };

                            self.push(value)?;
                        }
                        _ => {
                            return Err(self
                                .error_1(TypeError::Expected("indexable", value.ntype()).into()));
                        }
                    }
                }

                // IndexSet => {
                //     let target = self.pop()?.clone();
                //     let index = self.pop()?.clone();
                //     let value = self.peek(0)?.clone();

                //     let idx = match index {
                //         Value::Int(n) => n,
                //         _ => {
                //             return Err(
                //                 self.error_1(TypeError::Expected("int", index.ntype()).into())
                //             );
                //         }
                //     };

                //     match target {
                //         Value::Array(array) => {
                //             let mut array = array.borrow_mut();

                //             let idx =
                //                 fix_index(idx, array.len()).map_err(|e| self.error_1(e.into()))?;
                //             array.set(idx, value).map_err(|e| self.error_1(e.into()))?;
                //         }
                //         _ => {
                //             return Err(
                //                 self.error_1(TypeError::Expected("mutable", target.ntype()).into())
                //             );
                //         }
                //     }
                // }
                CloseUpvalue => {
                    self.close_upvalues(self.sp - 1);
                    self.pop()?;
                }

                CreatePair => {
                    let b = self.pop()?.clone();
                    let a = self.pop()?.clone();
                    self.push(Value::Pair(Rc::new((a, b))))?;
                }

                CreateArray => {
                    let argc = self.read_u8();
                    let mut array = Vecc::with_capacity(argc as usize, self.options.array_max_size);

                    for _ in 0..argc {
                        array
                            .push(self.pop()?.clone())
                            .map_err(|e| self.error_1(e.into()))?;
                    }

                    // println!("array, {}", array.get_size());

                    self.push(Value::Array(Rc::new(array)))?;
                }

                CreateRange => {
                    let b = self.pop()?.clone();
                    let a = self.pop()?.clone();

                    match (a, b) {
                        (Value::Int(a), Value::Int(b)) => {
                            self.push(Value::Range(a as i32, b as i32))?;
                        }
                        (Value::Int(_), b) => {
                            return Err(self.error_1(TypeError::Expected("int", b.ntype()).into()));
                        }
                        (a, Value::Int(_)) => {
                            return Err(self.error_1(TypeError::Expected("int", a.ntype()).into()));
                        }

                        (a, _) => {
                            return Err(self.error_1(TypeError::Expected("int", a.ntype()).into()));
                        }
                    }
                }

                Unpack => {
                    let argc = self.read_u8();
                    let value = self.pop_swap_out()?;
                    match (&value, argc) {
                        (Value::Pair(pair), 2) => {
                            let (a, b) = (**pair).clone();
                            self.push(b)?;
                            self.push(a)?;
                        }
                        (Value::Pair(_), _) => {
                            return Err(self.error_1(TypeError::InsufficientValues(2, argc).into()));
                        }
                        (Value::Array(list), _) => {
                            let len = list.len();
                            if len != argc as usize {
                                return Err(
                                    self.error_1(TypeError::InsufficientValues(len, argc).into())
                                );
                            }

                            for i in (0..argc).rev() {
                                self.push(list[i as usize].clone())?;
                            }
                        }
                        _ => return Err(self.error_1(TypeError::Unpackable(value.ntype()).into())),
                    };
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
                        upvalues: Rc::new(Vec::new()),
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

                        let upvalue = if is_local == 1 {
                            self.capture_upvalue(self.frame.st + index)
                        } else {
                            self.frame.closure.upvalues[index].clone()
                        };

                        upvalues.push(upvalue);
                    }

                    let closure = Closure {
                        function,
                        upvalues: Rc::new(upvalues),
                    };
                    self.push(Value::Closure(Rc::new(closure)))?;
                }

                ExitContext => {
                    if let Some(exit) = self.exit_fn_stack.pop() {
                        exit(&mut self.data).map_err(|e| self.error_1(e.into()))?;
                    }
                }

                EnterContext => {
                    let builtin = self.read_u16();
                    let argc = self.read_u8() as usize;

                    match self.builtins.get(&builtin) {
                        Some(builtin) => {
                            let (entry, exit) = match builtin {
                                BuiltinFn::Fn(builtin) => (builtin, None),
                                BuiltinFn::Context(entry, exit) => (entry, Some(exit)),
                            };

                            if self.sp < argc {
                                return Err(self.error(OverflowError::StackUnderflow.into()));
                            }

                            let args = &self.stack[self.sp - argc..self.sp];
                            self.sp -= argc;

                            if let Some(exit) = exit {
                                self.exit_fn_stack.push(*exit);
                            }

                            let result = (entry)(&mut self.data, &args, &self.options)
                                .map_err(|e| self.error_1(e.into()))?;

                            self.push(result)?;
                        }
                        None => {
                            let name = self
                                .global_symbols
                                .get(builtin)
                                .cloned()
                                .unwrap_or(String::from("unkown???"));

                            return Err(self.error_1(NameError::UndefinedBuiltin(name).into()));
                        }
                    }
                }

                CallBuiltin => {
                    let builtin = self.read_u16();
                    let argc = self.read_u8() as usize;

                    match self.builtins.get(&builtin) {
                        Some(builtin) => {
                            let builtin =
                                match builtin {
                                    BuiltinFn::Fn(builtin) => builtin,
                                    BuiltinFn::Context(_, _) => Err(self
                                        .error(TypeError::NotCallable("context manager").into()))?,
                                };

                            if self.sp < argc {
                                return Err(self.error(OverflowError::StackUnderflow.into()));
                            }

                            let args = &self.stack[self.sp - argc..self.sp];
                            self.sp -= argc;

                            let result = (builtin)(&mut self.data, &args, &self.options)
                                .map_err(|e| self.error_1(e.into()))?;

                            self.push(result)?;
                        }
                        None => {
                            let name = self
                                .global_symbols
                                .get(builtin)
                                .cloned()
                                .unwrap_or(String::from("unkown???"));

                            return Err(self.error_1(NameError::UndefinedBuiltin(name).into()));
                        }
                    }

                    self.has_runtime_exceeded()?;
                }

                CallFunction => {
                    let argc = self.read_u8();
                    let value = self.peek(argc as usize)?;

                    if self.frames.len() >= self.options.call_stack_size {
                        return Err(self.error(OverflowError::StackOverflow.into()));
                    }

                    let function = match value {
                        Value::Closure(closure) => &closure.function,
                        Value::Function(function) => function,
                        _ => Err(self.error_1(TypeError::NotCallable(value.ntype()).into()))?,
                    };

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

                    let closure = match value {
                        Value::Closure(closure) => (**closure).clone(),
                        Value::Function(function) => Closure {
                            function: function.clone(),
                            upvalues: Rc::new(Vec::new()),
                        },
                        _ => Err(self.error_1(TypeError::NotCallable(value.ntype()).into()))?,
                    };

                    let frame = CallFrame {
                        closure,
                        ip: 0,
                        st: self.sp - argc as usize - 1,
                    };

                    self.frames.push(std::mem::replace(&mut self.frame, frame));
                    self.data.on_enter_function();
                }

                JumpForward => {
                    let offset = self.read_u16() as usize;
                    self.frame.ip += offset;
                }

                JumpBackward => {
                    let offset = self.read_u16() as usize;
                    self.frame.ip -= offset;
                }

                JumpIfFalse => {
                    let offset = self.read_u16() as usize;
                    let value = self.peek(0)?;

                    if !value.is_truthy().map_err(|e| self.error_1(e))? {
                        self.frame.ip += offset;
                    }
                }

                NewLoopCtx => {
                    self.push(Value::LoopCtx(0))?;
                }

                // stack[sp-3] contains loop ctx
                // stack[sp-2] contains iterable
                // stack[sp-1] contains previous loop iteration's value
                IterNext => {
                    let exit_offset = self.read_u16() as usize;
                    let loop_idx = {
                        let value = &mut self.stack[self.sp - 3];
                        // println!("iter next: {}", value.display());
                        if let Value::LoopCtx(loop_idx) = value {
                            let idx = *loop_idx;

                            *loop_idx += 1;

                            idx
                        } else {
                            return Err(self.error_1(TypeError::FailedToIterate.into()));
                        }
                    };

                    let iterable = &self.stack[self.sp - 2];
                    // println!("iterable: {}", iterable.display());

                    let value = match iterable {
                        Value::Array(list) => {
                            if let Some(item) = list.get(loop_idx) {
                                item.clone()
                            } else {
                                // println!("iter next: end of list");
                                // end of iteration reached
                                self.frame.ip += exit_offset;
                                continue;
                            }
                        }
                        Value::Range(start, end) => {
                            let loop_idx = start + loop_idx as i32;
                            if loop_idx < *end {
                                Value::Int(loop_idx as i64)
                            } else {
                                self.frame.ip += exit_offset;
                                continue;
                            }
                        }
                        Value::String(string) => {
                            if let Some(item) = string.chars().nth(loop_idx) {
                                Value::String(item.to_string().into())
                            } else {
                                self.frame.ip += exit_offset;
                                continue;
                            }
                        }
                        Value::Color(color) => {
                            if let Some(item) = color.get(loop_idx) {
                                Value::Int(*item as i64)
                            } else {
                                self.frame.ip += exit_offset;
                                continue;
                            }
                        }
                        _ => Err(self.error_1(TypeError::NotIterable(iterable.ntype()).into()))?,
                    };

                    self.push(value)?;
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
                    let sms = self.options.string_max_size;
                    let (lhs, rhs) = self.pop_double_ref()?;
                    let value = lhs.add(rhs, sms).map_err(|e| self.error_1(e))?;

                    self.push(value)?;
                }

                BinarySubtract => {
                    let (lhs, rhs) = self.pop_double_ref()?;
                    let value = lhs.subtract(rhs).map_err(|e| self.error_1(e))?;

                    self.push(value)?;
                }

                BinaryMultiply => {
                    let sms = self.options.string_max_size;
                    let (lhs, rhs) = self.pop_double_ref()?;
                    let value = lhs.multiply(&rhs, sms).map_err(|e| self.error_1(e))?;
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
                    let sms = self.options.string_max_size;
                    let ams = self.options.array_max_size;
                    let (lhs, rhs) = self.pop_double_ref()?;

                    let value = lhs.join(rhs, sms, ams).map_err(|e| self.error_1(e))?;
                    self.push(value)?;
                }

                BitwiseAnd => {
                    let (lhs, rhs) = self.pop_double_ref()?;

                    let value = lhs.bitwise_and(rhs).map_err(|e| self.error_1(e))?;
                    self.push(value)?;
                }

                BitwiseOr => {
                    let (lhs, rhs) = self.pop_double_ref()?;

                    let value = lhs.bitwise_or(rhs).map_err(|e| self.error_1(e))?;
                    self.push(value)?;
                }

                BitwiseXor => {
                    let (lhs, rhs) = self.pop_double_ref()?;

                    let value = lhs.bitwise_xor(rhs).map_err(|e| self.error_1(e))?;
                    self.push(value)?;
                }

                BitwiseLeftShift => {
                    let (lhs, rhs) = self.pop_double_ref()?;

                    let value = lhs.bitwise_shift_left(rhs).map_err(|e| self.error_1(e))?;
                    self.push(value)?;
                }

                BitwiseRightShift => {
                    let (lhs, rhs) = self.pop_double_ref()?;

                    let value = lhs.bitwise_shift_right(rhs).map_err(|e| self.error_1(e))?;
                    self.push(value)?;
                }

                BitwiseNot => {
                    let value = self.pop()?.bitwise_not().map_err(|e| self.error_1(e))?;
                    self.push(value)?;
                }

                op => {
                    unimplemented!("Opcode: {}", op.name());
                }
            }
        }
    }

    #[inline]
    fn has_runtime_exceeded(&self) -> Result<(), ErrorS> {
        if self.start_time.elapsed() > self.options.max_runtime {
            Err(self.errorf(Error::RuntimeLimitExceeded(self.options.max_runtime)))
        } else {
            Ok(())
        }
    }

    #[inline]
    fn capture_upvalue(&mut self, location: usize) -> Rc<RefCell<Upvalue>> {
        for upvalue in self.open_upvalues.iter_mut() {
            if let Upvalue::Open(loc) = *upvalue.borrow() {
                if loc == location {
                    return upvalue.clone();
                }
            }
        }

        let upvalue = Rc::new(RefCell::new(Upvalue::new(location)));
        self.open_upvalues.push(upvalue.clone());

        upvalue
    }

    #[inline]
    fn close_upvalues(&mut self, last: usize) {
        if last >= self.stack.len() {
            return;
        }

        let value = &self.stack[last];
        for upvalue in self.open_upvalues.iter() {
            if upvalue.borrow().is_open_at(last) {
                upvalue.replace(Upvalue::Closed(value.clone()));
            }
        }

        self.open_upvalues
            .retain(|upvalue| upvalue.borrow().is_open());
    }

    #[inline]
    fn error(&self, error: Error) -> ErrorS {
        let span = self.frame.closure.function.chunk.get_span(self.frame.ip);
        (error, span)
    }

    #[inline]
    fn error_1(&self, error: Error) -> ErrorS {
        let span = self
            .frame
            .closure
            .function
            .chunk
            .get_span(self.frame.ip.saturating_sub(1));
        (error, span)
    }

    #[inline]
    fn errorf(&self, error: Error) -> ErrorS {
        let span = if self.frames.is_empty() {
            self.frame.closure.function.chunk.get_span(self.frame.ip)
        } else {
            self.frame.closure.function.name.1
        };

        (error, span)
    }

    #[inline]
    fn push(&mut self, value: Value) -> Result<(), ErrorS> {
        // print!("-> {}\n", value.display());
        if self.sp >= self.options.stack_size {
            return Err(self.error(OverflowError::StackOverflow.into()));
        }

        self.stack[self.sp] = value;
        self.sp += 1;

        Ok(())
    }

    #[inline]
    fn push_many(&mut self, n: usize) -> Result<(), ErrorS> {
        if self.sp + n >= self.options.stack_size {
            return Err(self.error_1(OverflowError::StackOverflow.into()));
        }

        self.sp += n;

        Ok(())
    }

    #[inline]
    fn pop(&mut self) -> Result<&Value, ErrorS> {
        if self.sp == 0 {
            return Err(self.error_1(OverflowError::StackUnderflow.into()));
        }
        self.sp -= 1;

        // let popped = std::mem::replace(&mut self.stack[self.sp], Value::None);
        // let popped = self.stack[self.sp].clone();
        // print!("<- {}\n", self.stack[self.sp].display());
        Ok(&self.stack[self.sp])
    }

    #[inline]
    fn pop_many(&mut self, n: usize) -> Result<(), ErrorS> {
        if self.sp < n {
            return Err(self.error_1(OverflowError::StackUnderflow.into()));
        }
        self.sp -= n;

        Ok(())
    }

    #[inline]
    pub fn pop_swap_out(&mut self) -> Result<Value, ErrorS> {
        if self.sp == 0 {
            return Err(self.error(OverflowError::StackUnderflow.into()));
        }
        self.sp -= 1;

        let popped = std::mem::replace(&mut self.stack[self.sp], Value::None);
        // let popped = self.stack[self.sp].clone();

        Ok(popped)
    }

    #[inline]
    fn pop_double_ref(&mut self) -> Result<(&Value, &Value), ErrorS> {
        if self.sp < 2 {
            return Err(self.error(OverflowError::StackUnderflow.into()));
        }
        self.sp -= 2;

        Ok((&self.stack[self.sp], &self.stack[self.sp + 1]))
    }

    #[inline]
    fn peek(&self, distance: usize) -> Result<&Value, ErrorS> {
        if self.sp < distance + 1 {
            return Err(self.error(OverflowError::StackUnderflow.into()));
        }

        Ok(&self.stack[self.sp - distance - 1])
    }

    // #[inline]
    // fn peek_mut(&mut self, distance: usize) -> Result<&mut Value, ErrorS> {
    //     if self.sp < distance + 1 {
    //         return Err(self.error(OverflowError::StackUnderflow.into()));
    //     }

    //     Ok(&mut self.stack[self.sp - distance - 1])
    // }

    #[inline]
    fn swap(&mut self) -> Result<(), ErrorS> {
        if self.sp < 2 {
            return Err(self.error(OverflowError::StackUnderflow.into()));
        }

        self.stack.swap(self.sp - 1, self.sp - 2);

        Ok(())
    }

    #[inline]
    fn read_u8(&mut self) -> u8 {
        let value = self.frame.closure.function.chunk.ops.read_u8(self.frame.ip);
        self.frame.ip += 1;
        value
    }

    #[inline]
    fn read_u16(&mut self) -> u16 {
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
    fn read_constant(&mut self) -> &Value {
        let idx = self.read_u16();
        &self.frame.closure.function.chunk.constants[idx as usize]
    }

    #[inline]
    fn get_constant(&self, idx: u16) -> &Value {
        &self.frame.closure.function.chunk.constants[idx as usize]
    }
}

struct CallFrame {
    pub closure: Closure,
    pub ip: usize,
    pub st: usize,
}
