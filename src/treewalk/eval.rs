use std::{
    rc::Rc,
    time::{Duration, Instant},
};

use ariadne::Color;
use colored::Colorize;
use fxhash::FxHashMap;

use crate::{
    builtins::{BuiltinAdder, BuiltinFnMap},
    frontend::{Expr, Op},
    prelude::{BuiltinFn, BuiltinFnReg, RuntimeError},
    shared::{Span, Spanned, Value},
};

/// Options for the VM
/// ### Fields
/// * `max_expr_depth` - the maximum depth of an expression
/// * `max_call_stack` - the maximum depth of the call stack
/// * `max_loops` - the maximum number of loops that can be executed in a run
/// * `loop_min` - the minimum value of a loop
/// * `loop_max` - the maximum value of a loop
/// * `max_runtime` - the maximum runtime of a run
pub struct VMOpts {
    pub max_expr_depth: usize,
    pub max_call_stack: usize,
    pub max_loops: usize,

    pub loop_min: i32,
    pub loop_max: i32,

    pub max_runtime: Option<Duration>,
}

impl Default for VMOpts {
    fn default() -> Self {
        Self {
            max_expr_depth: 25,
            max_call_stack: 5,
            max_loops: 2,

            loop_min: 0,
            loop_max: 256,

            max_runtime: Some(Duration::from_secs(5)),
        }
    }
}

/// Type that represents a runtime Function in the VM
/// ### Fields
/// * `args` - the arguments of the function
/// * `body` - the body of the function
///
#[derive(Clone, Debug)]
pub(crate) struct Function {
    pub args: Vec<Spanned<String>>,
    pub body: Vec<Spanned<Expr>>,
}

/// A VM that can execute Quilt code
/// ### Fields
/// * `frames` - the stack of frames
/// * `functions` - the functions defined in the VM
/// * `data` - the data that builtins in the VM can access
/// * `builtins` - the builtins that the VM can access
///
/// ### Examples
/// ```no_run
/// use quilt::prelude::*;
///
/// let mut vm = VM::with_stdio(());
/// let ast = parse("@print('Hello, world!')").unwrap();
/// vm.eval(ast).map_err(|e| e.print()).unwrap();
/// ```
pub struct VM<Data> {
    pub frames: Vec<FxHashMap<String, Value>>,
    pub(crate) functions: FxHashMap<String, Rc<Function>>,

    /// Can be used to store any state the VM needs to access
    pub data: Data,
    pub builtins: BuiltinFnMap<Data>,

    start: Instant,
    loop_count: usize,
    depth: usize,

    pub options: VMOpts,
}

impl<Data> VM<Data> {
    /// Creates a new VM with no builtins
    pub fn new(data: Data) -> Self {
        Self {
            frames: vec![FxHashMap::default()],
            functions: FxHashMap::default(),
            data,
            builtins: BuiltinFnMap::default(),

            start: Instant::now(),
            loop_count: 0,
            depth: 0,

            options: VMOpts::default(),
        }
    }

    pub fn new_with_opts(data: Data, options: VMOpts) -> Self {
        Self {
            frames: vec![FxHashMap::default()],
            functions: FxHashMap::default(),
            data,
            builtins: BuiltinFnMap::default(),

            start: Instant::now(),

            loop_count: 0,
            depth: 0,

            options,
        }
    }

    /// Creates a new VM with the standard library builtins (std, math)
    pub fn with_std(data: Data) -> Self {
        let mut vm = Self::new(data);
        vm.add_builtins(crate::std::std);
        vm.add_builtins(crate::std::math);
        vm
    }

    /// Creates a new VM with the standard library builtins (std, math, io)
    pub fn with_stdio(data: Data) -> Self {
        let mut vm = Self::new(data);
        vm.add_builtins(crate::std::std);
        vm.add_builtins(crate::std::math);
        vm.add_builtins(crate::std::io);

        vm
    }

    /// Adds a builtin group to the VM
    pub fn add_builtins(&mut self, adder: BuiltinAdder<Data>) {
        adder(&mut self.builtins);
    }

    /// Adds multiple builtin groups to the VM
    pub fn add_builtins_from(&mut self, adders: &[BuiltinAdder<Data>]) {
        for adder in adders {
            adder(&mut self.builtins);
        }
    }

    /// Remove and get a variable from the current frame
    pub fn remove(&mut self, (name, span): Spanned<String>) -> Result<Value, RuntimeError> {
        for frame in self.frames.iter_mut().rev() {
            if let Some(result) = frame.remove(&name) {
                return Ok(result);
            }
        }

        Err(RuntimeError {
            msg: format!("Variable '{}' is not defined", name)
                .red()
                .to_string(),
            span,
            help: Some(
                format!("define it with 'let {} = ...'", name)
                    .yellow()
                    .to_string(),
            ),
            color: None,
        })
    }

    /// Gets a variable from the current frame
    pub fn get(&self, (name, span): Spanned<String>) -> Result<Value, RuntimeError> {
        for frame in self.frames.iter().rev() {
            if let Some(expr) = frame.get(&name) {
                return Ok(expr.clone());
            }
        }

        Err(RuntimeError {
            msg: format!("Variable '{}' is not defined", name)
                .red()
                .to_string(),
            span,
            help: Some(
                format!("define it with 'let {} = ...'", name)
                    .yellow()
                    .to_string(),
            ),
            color: None,
        })
    }

    /// Declares a variable in the current frame
    pub fn declare(&mut self, name: &str, value: Value) {
        if let Some(result) = self.frames.last_mut() {
            result.insert(name.to_string(), value);
        }
    }

    /// Pushes a new frame onto the stack
    ///
    /// ⚠️ *The maximum stack size is 5* ⚠️
    pub fn push_frame(&mut self, span: Span) -> Result<(), RuntimeError> {
        if self.frames.len() >= self.options.max_call_stack {
            return Err(RuntimeError {
                msg: format!(
                    "Maximum call stack size exceeded ({})",
                    self.options.max_call_stack
                )
                .red()
                .to_string(),
                span,
                help: None,
                color: None,
            });
        }

        self.frames.push(FxHashMap::default());
        Ok(())
    }

    /// Pops a frame off the stack
    pub fn pop_frame(&mut self) -> Option<FxHashMap<String, Value>> {
        self.frames.pop()
    }

    /// update the value of a variable in the current frame
    /// or any of the parent frames
    ///
    /// ### Errors
    /// If the variable is not declared
    pub fn set(&mut self, name: &String, value: Value, span: Span) -> Result<(), RuntimeError> {
        for frame in self.frames.iter_mut().rev() {
            if let Some(result) = frame.get_mut(name) {
                *result = value;
                return Ok(());
            }
        }
        Err(RuntimeError {
            msg: format!("Variable '{}' is not declared", name)
                .red()
                .to_string(),
            span,
            help: Some(
                format!("declare it with 'let {} = ...'", name)
                    .yellow()
                    .to_string(),
            ),
            color: None,
        })
    }

    /// evals a list of expressions (ast)
    pub fn eval(&mut self, ast: Vec<Spanned<Expr>>) -> Result<Value, RuntimeError> {
        self.start = Instant::now();
        let mut result = Value::None;
        for expr in ast {
            result = self.eval_expr(expr)?;
        }
        Ok(result)
    }

    pub fn finish(self) -> Data {
        self.data
    }

    /// evals a single expression
    pub fn eval_expr(&mut self, (expr, span): Spanned<Expr>) -> Result<Value, RuntimeError> {
        if let Some(max_runtime) = self.options.max_runtime {
            if self.start.elapsed() > max_runtime {
                return Err(RuntimeError {
                    msg: format!("Maximum runtime exceeded ({:?})", max_runtime)
                        .red()
                        .to_string(),
                    span,
                    help: None,
                    color: None,
                });
            }
        }

        if self.depth > self.options.max_expr_depth {
            return Err(RuntimeError {
                msg: format!(
                    "Maximum expression depth exceeded ({})",
                    self.options.max_expr_depth
                )
                .red()
                .to_string(),
                span,
                help: None,
                color: None,
            });
        }

        self.depth += 1;

        let result = match expr {
            Expr::Import((namespace, span), block) => {
                if let Some(block) = block {
                    let mut result = Value::None;
                    for expr in block {
                        result = self.eval_expr(expr)?;
                    }
                    Ok(result)
                } else {
                    Err(RuntimeError {
                        msg: format!("Unresolved import: '{}'", namespace)
                            .red()
                            .to_string(),
                        span,
                        help: Some("Only top level imports are supported".yellow().to_string()),
                        color: None,
                    })
                }
            }
            Expr::Literal(l) => Ok(l),
            Expr::Ident(name) => self.get((name, span)),
            Expr::Yoink(name) => self.remove((name, span)),
            Expr::List(items) => Ok(Value::List(
                items
                    .into_iter()
                    .map(|item| self.eval_expr(item))
                    .collect::<Result<Vec<_>, _>>()?,
            )),
            Expr::Pair(left, right) => Ok(Value::Pair(
                Box::new(self.eval_expr(*left)?),
                Box::new(self.eval_expr(*right)?),
            )),
            Expr::Declaration((name, _), value) => {
                let value = self.eval_expr(*value)?;
                self.declare(&name, value);
                Ok(Value::None)
            }
            Expr::MultiDeclaration(names, value) => {
                let value = self.eval_expr(*value)?;
                match value {
                    Value::List(items) => {
                        if items.len() != names.len() {
                            Err(RuntimeError {
                                msg: format!(
                                    "Expected {} items in list, got {}",
                                    names.len(),
                                    items.len()
                                )
                                .red()
                                .to_string(),
                                span,
                                help: None,
                                color: None,
                            })?;
                        }

                        for ((name, _), value) in names.into_iter().zip(items) {
                            self.declare(&name, value);
                        }
                        Ok(Value::None)
                    }
                    Value::Pair(left, right) => {
                        if names.len() != 2 {
                            Err(RuntimeError {
                                msg: format!("Tried to unpack pair into {} variables", names.len())
                                    .red()
                                    .to_string(),
                                span,
                                help: None,
                                color: None,
                            })?;
                        }
                        self.declare(&names[0].0, *left);
                        self.declare(&names[1].0, *right);
                        Ok(Value::None)
                    }

                    _ => Err(RuntimeError {
                        msg: format!(
                            "Cannot unpack {} into {} variables",
                            value.ntype().cyan(),
                            names.len()
                        )
                        .red()
                        .to_string(),
                        span,
                        help: None,
                        color: Some(Color::Red),
                    })?,
                }
            }
            Expr::Assignment(name, value) => {
                let value = self.eval_expr(*value)?;
                let (name, span) = name;
                self.set(&name, value, span)?;
                Ok(Value::None)
            }
            Expr::Binary(op, lhs, rhs) => {
                let lhs = self.eval_expr(*lhs)?;
                let rhs = self.eval_expr(*rhs)?;
                self.eval_binary(op, lhs, rhs, span)
            }
            Expr::Unary(op, lhs) => {
                let lhs = self.eval_expr(*lhs)?;
                match &op {
                    Op::Not => match lhs {
                        Value::Bool(b) => Ok(Value::Bool(!b)),
                        _ => Err(RuntimeError {
                            msg: format!(
                                "Cannot perform logical NOT on type {}",
                                lhs.ntype().cyan()
                            )
                            .yellow()
                            .to_string(),
                            span,
                            help: None,
                            color: Some(Color::Yellow),
                        }),
                    },
                    Op::Neg => match lhs {
                        Value::Int(i) => Ok(Value::Int(-i)),
                        Value::Float(f) => Ok(Value::Float(-f)),
                        _ => Err(RuntimeError {
                            msg: format!("Cannot perform negation on type {}", lhs.ntype().cyan())
                                .yellow()
                                .to_string(),
                            span,
                            help: None,
                            color: Some(Color::Yellow),
                        }),
                    },
                    Op::Spread => match lhs {
                        Value::List(items) => Ok(Value::Spread(items)),
                        Value::Pair(left, right) => Ok(Value::Spread(vec![*left, *right])),
                        _ => Err(RuntimeError {
                            msg: format!(
                                "Cannot perform spread on non-list type {}",
                                lhs.ntype().cyan()
                            )
                            .yellow()
                            .to_string(),
                            span,
                            help: None,
                            color: Some(Color::Yellow),
                        }),
                    },
                    _ => Ok(lhs),
                }
            }
            Expr::Function(name, args, body) => {
                self.functions
                    .insert(name.0, Rc::new(Function { args, body }));
                Ok(Value::None)
            }
            Expr::Call(name, args) => self.eval_function(name, args),
            Expr::ContextWrapped((func, span), args, varname, body) => {
                if let Some(ctxfunc) = self.builtins.get(&func) {
                    match ctxfunc {
                        BuiltinFn::Fn(func) => {
                            let res = self.eval_builtin_inner((*func, span), args)?;

                            if let Some(varname) = varname {
                                self.declare(&varname.0, res);
                            }

                            let mut result = (Value::None, 0..0);
                            for expr in body {
                                result = (self.eval_expr(expr.clone())?, expr.1);
                            }

                            Ok(result.0)
                        }
                        BuiltinFn::Context(before, after) => {
                            let after = *after;

                            let res = self.eval_builtin_inner((*before, span.clone()), args)?;

                            if let Some(varname) = varname {
                                self.declare(&varname.0, res);
                            }

                            let mut result = (Value::None, 0..0);
                            for expr in body {
                                result = (self.eval_expr(expr.clone())?, expr.1);
                            }

                            let result = (Expr::Literal(result.0), result.1);

                            self.eval_builtin_inner((after, span), vec![result])
                        }
                    }
                } else {
                    Err(RuntimeError {
                        msg: format!("Unknown builtin function {}", func.cyan())
                            .red()
                            .to_string(),
                        span,
                        help: None,
                        color: Some(Color::Red),
                    })
                }
            }
            Expr::BuiltinCall(name, args) => self.eval_builtin(name, args),
            Expr::Conditional(cond, then, otherwise) => {
                let cond = self.eval_expr(*cond)?;

                let mut result = Value::None;
                match cond {
                    Value::Bool(true) => {
                        // self.frames.push(FxHashMap::default());

                        result = then
                            .iter()
                            .map(|expr| self.eval_expr(expr.clone()))
                            .last()
                            .unwrap_or(Ok(Value::None))?;

                        // self.frames.pop();
                    }
                    Value::Bool(false) => {
                        if let Some(otherwise) = otherwise {
                            // self.frames.push(FxHashMap::default());

                            result = otherwise
                                .iter()
                                .map(|expr| self.eval_expr(expr.clone()))
                                .last()
                                .unwrap_or(Ok(Value::None))?;

                            // self.frames.pop();
                        }
                    }
                    _ => {
                        return Err(RuntimeError {
                            msg: format!(
                                "type {} is not a boolean expression",
                                cond.ntype().cyan()
                            )
                            .yellow()
                            .to_string(),
                            span,
                            help: None,
                            color: Some(Color::Yellow),
                        })
                    }
                };

                Ok(result)
            }
            Expr::ForLoop(name, start, end, body) => {
                let start = self.eval_expr(*start)?;
                let end = self.eval_expr(*end)?;

                match (&start, &end) {
                    (Value::Int(start), Value::Int(end)) => {
                        self.eval_for_loop(name, *start, *end, body)
                    }
                    _ => Err(RuntimeError {
                        msg: format!(
                            "Expected integers for loop bounds, got {} and {}",
                            start.ntype().cyan(),
                            end.ntype().cyan()
                        )
                        .yellow()
                        .to_string(),
                        span,
                        help: None,
                        color: Some(Color::Yellow),
                    }),
                }
            }
        };

        self.depth -= 1;
        result
    }

    pub fn eval_for_loop(
        &mut self,
        (name, span): Spanned<String>,
        start: i32,
        end: i32,
        body: Vec<Spanned<Expr>>,
    ) -> Result<Value, RuntimeError> {
        let mut result = Value::None;

        for i in start.max(self.options.loop_min)..end.min(self.options.loop_max) {
            if self.loop_count >= self.options.max_loops {
                return Err(RuntimeError {
                    msg: format!(
                        "Loop limit of {} exceeded",
                        self.options.max_loops.to_string().cyan()
                    )
                    .red()
                    .to_string(),
                    span,
                    help: None,
                    color: Some(Color::Red),
                });
            }

            self.loop_count += 1;

            self.declare(&name, Value::Int(i));
            for expr in &body {
                result = self.eval_expr(expr.clone())?;
            }

            self.loop_count -= 1;
        }

        Ok(result)
    }

    pub fn eval_args(&mut self, args: Vec<Spanned<Expr>>) -> Result<Vec<Value>, RuntimeError> {
        let mut values = Vec::with_capacity(args.len());

        for arg in args {
            let value = self.eval_expr(arg)?;

            if let Value::Spread(vals) = value {
                values.extend(vals);
            } else {
                values.push(value);
            }
        }

        Ok(values)
    }

    pub fn eval_builtin_inner(
        &mut self,
        (func, span): Spanned<BuiltinFnReg<Data>>,
        args: Vec<Spanned<Expr>>,
    ) -> Result<Value, RuntimeError> {
        let (s, mut e) = (span.start, span.end);

        let mut values = Vec::with_capacity(args.len());

        for (arg, aspan) in args {
            e = e.max(aspan.end);
            let value = self.eval_expr((arg, aspan.clone()))?;

            if let Value::Spread(vals) = value {
                for val in vals {
                    values.push((val, aspan.clone()));
                }
            } else {
                values.push((value, aspan));
            }
        }

        let result = (func)(
            &mut self.data,
            &mut (values, s.saturating_sub(1)..e.saturating_add(1)),
        )?;

        Ok(result)
    }

    /// Evaluate a function call
    pub fn eval_function(
        &mut self,
        (name, span): Spanned<String>,
        values: Vec<Spanned<Expr>>,
    ) -> Result<Value, RuntimeError> {
        let values = self.eval_args(values)?;

        if let Some(func) = self.functions.get(&name) {
            if values.len() != func.args.len() {
                return Err(RuntimeError {
                    msg: format!(
                        "Expected {} arguments, got {}",
                        func.args.len().to_string().cyan(),
                        values.len().to_string().cyan()
                    )
                    .yellow()
                    .to_string(),
                    span,
                    help: None,
                    color: Some(Color::Yellow),
                });
            }

            let func = func.clone();
            self.push_frame(span)?;

            func.args
                .iter()
                .zip(values.into_iter())
                .for_each(|((name, _), value)| {
                    if let Some(result) = self.frames.last_mut() {
                        result.insert(name.to_string(), value);
                    }
                });

            let mut result = Value::None;

            for expr in &func.body {
                result = self.eval_expr(expr.clone())?;
            }

            self.pop_frame();

            Ok(result)
        } else {
            Err(RuntimeError {
                msg: format!("Function '{}' is not defined", name)
                    .red()
                    .to_string(),
                span,
                help: Some(
                    format!("define it with 'fn {}(...) {{ ... }}'", name)
                        .yellow()
                        .to_string(),
                ),
                color: None,
            })
        }
    }

    /// evals a builtin function call
    pub fn eval_builtin(
        &mut self,
        (name, span): Spanned<String>,
        values: Vec<Spanned<Expr>>,
    ) -> Result<Value, RuntimeError> {
        if let Some(func) = self.builtins.get(&name) {
            match func {
                BuiltinFn::Fn(func) => self.eval_builtin_inner((*func, span), values),
                BuiltinFn::Context(_, _) => {
                    return Err(RuntimeError {
                        msg: format!("'{}' can only called with 'with'", name)
                            .red()
                            .to_string(),
                        span,
                        help: Some("use 'with' to call this function".yellow().to_string()),
                        color: None,
                    })
                }
            }
        } else {
            Err(RuntimeError {
                msg: format!("Outbound '{}' is not defined", name)
                    .red()
                    .to_string(),
                span,
                help: None,
                color: None,
            })
        }
    }

    /// Evaluate a binary operation
    pub fn eval_binary(
        &mut self,
        op: Op,
        lhs: Value,
        rhs: Value,
        span: Span,
    ) -> Result<Value, RuntimeError> {
        match &op {
            Op::Add => match (&lhs, &rhs) {
                (Value::Str(lhs), Value::Str(rhs)) => Ok(Value::Str(format!("{}{}", lhs, rhs))),
                (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs + rhs)),
                (Value::Int(lhs), Value::Float(rhs)) => Ok(Value::Float(*lhs as f32 + rhs)),
                (Value::Float(lhs), Value::Int(rhs)) => Ok(Value::Float(lhs + *rhs as f32)),
                (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs + rhs)),
                (Value::Color(lhs), Value::Color(rhs)) => Ok(Value::Color([
                    lhs[0].saturating_add(rhs[0]),
                    lhs[1].saturating_add(rhs[1]),
                    lhs[2].saturating_add(rhs[2]),
                    lhs[3].saturating_add(rhs[3]),
                ])),
                _ => Err(RuntimeError {
                    msg: format!(
                        "Cannot add types {} and {}",
                        lhs.ntype().cyan(),
                        rhs.ntype().cyan()
                    )
                    .yellow()
                    .to_string(),
                    span,
                    help: None,
                    color: Some(Color::Yellow),
                }),
            },
            Op::Sub => match (&lhs, &rhs) {
                (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs - rhs)),
                (Value::Int(lhs), Value::Float(rhs)) => Ok(Value::Float(*lhs as f32 - rhs)),
                (Value::Float(lhs), Value::Int(rhs)) => Ok(Value::Float(lhs - *rhs as f32)),
                (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs - rhs)),
                (Value::Color(lhs), Value::Color(rhs)) => Ok(Value::Color([
                    lhs[0].saturating_sub(rhs[0]),
                    lhs[1].saturating_sub(rhs[1]),
                    lhs[2].saturating_sub(rhs[2]),
                    lhs[3].saturating_sub(rhs[3]),
                ])),
                _ => Err(RuntimeError {
                    msg: format!(
                        "Cannot subtract types {} and {}",
                        lhs.ntype().cyan(),
                        rhs.ntype().cyan()
                    )
                    .yellow()
                    .to_string(),
                    span,
                    help: None,
                    color: Some(Color::Yellow),
                }),
            },
            Op::Mul => match (&lhs, &rhs) {
                (Value::Str(lhs), Value::Int(rhs)) => {
                    Ok(Value::Str(lhs.repeat(*rhs.min(&64) as usize)))
                }
                (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs * rhs)),
                (Value::Int(lhs), Value::Float(rhs)) => Ok(Value::Float(*lhs as f32 * rhs)),
                (Value::Float(lhs), Value::Int(rhs)) => Ok(Value::Float(lhs * *rhs as f32)),
                (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs * rhs)),
                (Value::Color(lhs), Value::Color(rhs)) => Ok(Value::Color([
                    lhs[0].saturating_mul(rhs[0]),
                    lhs[1].saturating_mul(rhs[1]),
                    lhs[2].saturating_mul(rhs[2]),
                    lhs[3].saturating_mul(rhs[3]),
                ])),
                _ => Err(RuntimeError {
                    msg: format!(
                        "Cannot multiply types {} and {}",
                        lhs.ntype().cyan(),
                        rhs.ntype().cyan()
                    )
                    .yellow()
                    .to_string(),
                    span,
                    help: None,
                    color: Some(Color::Yellow),
                }),
            },
            Op::Div => {
                if rhs == Value::Int(0) || rhs == Value::Float(0.0) {
                    return Err(RuntimeError {
                        msg: "Cannot divide by zero".red().to_string(),
                        span,
                        help: None,
                        color: None,
                    });
                }
                match (&lhs, &rhs) {
                    (Value::Str(lhs), Value::Str(rhs)) => Ok(Value::Str(lhs.replace(rhs, ""))),
                    (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs / rhs)),
                    (Value::Int(lhs), Value::Float(rhs)) => Ok(Value::Float(*lhs as f32 / rhs)),
                    (Value::Float(lhs), Value::Int(rhs)) => Ok(Value::Float(lhs / *rhs as f32)),
                    (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs / rhs)),
                    (Value::Color(lhs), Value::Color(rhs)) => Ok(Value::Color([
                        lhs[0].saturating_div(rhs[0]),
                        lhs[1].saturating_div(rhs[1]),
                        lhs[2].saturating_div(rhs[2]),
                        lhs[3].saturating_div(rhs[3]),
                    ])),
                    _ => Err(RuntimeError {
                        msg: format!(
                            "Cannot divide types {} and {}",
                            lhs.ntype().cyan(),
                            rhs.ntype().cyan()
                        )
                        .yellow()
                        .to_string(),
                        span,
                        help: None,
                        color: Some(Color::Yellow),
                    }),
                }
            }
            Op::Mod => {
                if rhs == Value::Int(0) || rhs == Value::Float(0.0) {
                    return Err(RuntimeError {
                        msg: "Cannot modulo by zero".red().to_string(),
                        span,
                        help: None,
                        color: None,
                    });
                }

                match (&lhs, &rhs) {
                    (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs % rhs)),
                    (Value::Int(lhs), Value::Float(rhs)) => Ok(Value::Float(*lhs as f32 % rhs)),
                    (Value::Float(lhs), Value::Int(rhs)) => Ok(Value::Float(lhs % *rhs as f32)),
                    (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs % rhs)),
                    _ => Err(RuntimeError {
                        msg: format!(
                            "Cannot modulo types {} and {}",
                            lhs.ntype().cyan(),
                            rhs.ntype().cyan()
                        )
                        .yellow()
                        .to_string(),
                        span,
                        help: None,
                        color: Some(Color::Yellow),
                    }),
                }
            }
            Op::Pow => match (&lhs, &rhs) {
                (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs.pow(*rhs as u32))),
                (Value::Int(lhs), Value::Float(rhs)) => Ok(Value::Float((*lhs as f32).powf(*rhs))),
                (Value::Float(lhs), Value::Int(rhs)) => Ok(Value::Float(lhs.powf(*rhs as f32))),
                (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs.powf(*rhs))),
                _ => Err(RuntimeError {
                    msg: format!(
                        "Cannot exponentiate types {} and {}",
                        lhs.ntype().cyan(),
                        rhs.ntype().cyan()
                    )
                    .yellow()
                    .to_string(),
                    span,
                    help: None,
                    color: Some(Color::Yellow),
                }),
            },
            Op::Eq => Ok(Value::Bool(lhs == rhs)),
            Op::Neq => Ok(Value::Bool(lhs != rhs)),
            Op::Lt => match (&lhs, &rhs) {
                (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Bool(lhs < rhs)),
                (Value::Int(lhs), Value::Float(rhs)) => Ok(Value::Bool((*lhs as f32) < *rhs)),
                (Value::Float(lhs), Value::Int(rhs)) => Ok(Value::Bool(*lhs < *rhs as f32)),
                (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Bool(*lhs < *rhs)),
                (Value::Str(lhs), Value::Str(rhs)) => Ok(Value::Bool(lhs < rhs)),
                _ => Err(RuntimeError {
                    msg: format!(
                        "Cannot compare types {} and {}",
                        lhs.ntype().cyan(),
                        rhs.ntype().cyan()
                    )
                    .yellow()
                    .to_string(),
                    span,
                    help: None,
                    color: Some(Color::Yellow),
                }),
            },
            Op::Gt => match (&lhs, &rhs) {
                (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Bool(lhs > rhs)),
                (Value::Int(lhs), Value::Float(rhs)) => Ok(Value::Bool((*lhs as f32) > *rhs)),
                (Value::Float(lhs), Value::Int(rhs)) => Ok(Value::Bool(*lhs > *rhs as f32)),
                (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Bool(*lhs > *rhs)),
                (Value::Str(lhs), Value::Str(rhs)) => Ok(Value::Bool(lhs > rhs)),
                _ => Err(RuntimeError {
                    msg: format!(
                        "Cannot compare types {} and {}",
                        lhs.ntype().cyan(),
                        rhs.ntype().cyan()
                    )
                    .yellow()
                    .to_string(),
                    span,
                    help: None,
                    color: Some(Color::Yellow),
                }),
            },
            Op::Lte => match (&lhs, &rhs) {
                (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Bool(lhs <= rhs)),
                (Value::Int(lhs), Value::Float(rhs)) => Ok(Value::Bool((*lhs as f32) <= *rhs)),
                (Value::Float(lhs), Value::Int(rhs)) => Ok(Value::Bool(*lhs <= *rhs as f32)),
                (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Bool(*lhs <= *rhs)),
                (Value::Str(lhs), Value::Str(rhs)) => Ok(Value::Bool(lhs <= rhs)),
                _ => Err(RuntimeError {
                    msg: format!(
                        "Cannot compare types {} and {}",
                        lhs.ntype().cyan(),
                        rhs.ntype().cyan()
                    )
                    .yellow()
                    .to_string(),
                    span,
                    help: None,
                    color: Some(Color::Yellow),
                }),
            },
            Op::Gte => match (&lhs, &rhs) {
                (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Bool(lhs >= rhs)),
                (Value::Int(lhs), Value::Float(rhs)) => Ok(Value::Bool((*lhs as f32) >= *rhs)),
                (Value::Float(lhs), Value::Int(rhs)) => Ok(Value::Bool(*lhs >= *rhs as f32)),
                (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Bool(*lhs >= *rhs)),
                (Value::Str(lhs), Value::Str(rhs)) => Ok(Value::Bool(lhs >= rhs)),
                _ => Err(RuntimeError {
                    msg: format!(
                        "Cannot compare types {} and {}",
                        lhs.ntype().cyan(),
                        rhs.ntype().cyan()
                    )
                    .yellow()
                    .to_string(),
                    span,
                    help: None,
                    color: Some(Color::Yellow),
                }),
            },
            Op::And => match (&lhs, &rhs) {
                (Value::Bool(lhs), Value::Bool(rhs)) => Ok(Value::Bool(*lhs && *rhs)),
                _ => Err(RuntimeError {
                    msg: format!(
                        "Cannot perform logical AND on types {} and {}",
                        lhs.ntype().cyan(),
                        rhs.ntype().cyan()
                    )
                    .yellow()
                    .to_string(),
                    span,
                    help: None,
                    color: Some(Color::Yellow),
                }),
            },
            Op::Or => match (&lhs, &rhs) {
                (Value::Bool(lhs), Value::Bool(rhs)) => Ok(Value::Bool(*lhs || *rhs)),
                _ => Err(RuntimeError {
                    msg: format!(
                        "Cannot perform logical OR on types {} and {}",
                        lhs.ntype().cyan(),
                        rhs.ntype().cyan()
                    )
                    .yellow()
                    .to_string(),
                    span,
                    help: None,
                    color: Some(Color::Yellow),
                }),
            },
            Op::Join => match (&lhs, &rhs) {
                (Value::Str(lhs), Value::Str(rhs)) => Ok(Value::Str(lhs.clone() + rhs)),
                (Value::Str(lhs), Value::Int(rhs)) => {
                    Ok(Value::Str(lhs.clone() + &rhs.to_string()))
                }
                (Value::Str(lhs), Value::Float(rhs)) => {
                    Ok(Value::Str(lhs.clone() + &rhs.to_string()))
                }
                (Value::Str(lhs), Value::Bool(rhs)) => {
                    Ok(Value::Str(lhs.clone() + &rhs.to_string()))
                }
                (Value::Int(lhs), Value::Str(rhs)) => Ok(Value::Str(lhs.to_string() + rhs)),
                (Value::Float(lhs), Value::Str(rhs)) => Ok(Value::Str(lhs.to_string() + rhs)),
                (Value::Bool(lhs), Value::Str(rhs)) => Ok(Value::Str(lhs.to_string() + rhs)),
                _ => Err(RuntimeError {
                    msg: format!(
                        "Cannot join types {} and {}",
                        lhs.ntype().cyan(),
                        rhs.ntype().cyan()
                    )
                    .yellow()
                    .to_string(),
                    span,
                    help: None,
                    color: Some(Color::Yellow),
                }),
            },
            _ => Err(RuntimeError {
                msg: format!(
                    "Cannot perform operation on types {} and {}",
                    lhs.ntype().cyan(),
                    rhs.ntype().cyan()
                )
                .yellow()
                .to_string(),
                span,
                help: None,
                color: Some(Color::Yellow),
            }),
        }
    }
}
