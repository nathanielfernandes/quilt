use std::rc::Rc;

use crate::{
    backend::utils::pool::Pool,
    prelude::{ErrorS, Expr, Op, OverflowError, Span, Spanned},
};

use super::{
    bytecode::OpCode,
    chunk::Chunk,
    value::{Function, Value},
};

pub struct Compiler {
    level: Level,
    global_symbols: Pool<String, u16>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            level: Level {
                function: Function {
                    name: (String::from("@__main__"), Span::default()),
                    arity: 0,
                    upvalue_count: 0,
                    chunk: Chunk::new(),
                },
                locals: Vec::new(),
                upvalues: Vec::new(),
                enclosing: None,
                scope_depth: 0, // 0 is global scope
                symbol_pool: Pool::new(),
                constant_pool: Pool::new(),
            },
            global_symbols: Pool::new(),
        }
    }

    #[inline]
    fn is_global(&self) -> bool {
        self.level.scope_depth == 0
    }

    #[inline]
    fn write_op(&mut self, op: OpCode, span: Span) {
        self.level.function.chunk.write_op(op, span);
    }

    #[inline]
    fn write_u8(&mut self, byte: u8, span: Span) {
        self.level.function.chunk.write_u8(byte, span);
    }

    #[inline]
    fn write_u16(&mut self, byte: u16, span: Span) {
        self.level.function.chunk.write_u16(byte, span);
    }

    // fn patch_op(&mut self, offset: usize, op: OpCode) {
    //     self.level.function.chunk.patch_op(offset, op);
    // }

    // fn patch_u8(&mut self, offset: usize, byte: u8) {
    //     self.level.function.chunk.patch_u8(offset, byte);
    // }

    #[inline]
    fn patch_u16(&mut self, offset: usize, bytes: u16) {
        self.level.function.chunk.patch_u16(offset, bytes);
    }

    #[inline]
    fn write_op_u8(&mut self, op: OpCode, byte: u8, span: Span) {
        self.level.function.chunk.write_op(op, span);
        self.level.function.chunk.write_u8(byte, span);
    }

    #[inline]
    fn write_op_u16(&mut self, op: OpCode, byte: u16, span: Span) {
        self.level.function.chunk.write_op(op, span);
        self.level.function.chunk.write_u16(byte, span);
    }

    #[inline]
    fn write_jump(&mut self, op: OpCode, span: Span) -> usize {
        self.write_op(op, span);
        self.write_u16(0, span);
        self.level.function.chunk.len() - 2
    }

    fn patch_jump(&mut self, offset: usize, span: Span) -> Result<(), ErrorS> {
        let jump = self.level.function.chunk.ops.len() - 2 - offset;
        if jump > u16::MAX as usize {
            return Err((OverflowError::JumpTooLarge.into(), span));
        }
        self.patch_u16(offset, jump as u16);
        Ok(())
    }

    #[inline]
    fn add_constant(&mut self, value: Value) -> u16 {
        self.level.constant_pool.add(value)
    }

    #[inline]
    fn add_symbol(&mut self, symbol: String) -> u16 {
        self.level.symbol_pool.add(symbol)
    }

    #[inline]
    fn add_global_symbol(&mut self, symbol: String) -> u16 {
        self.global_symbols.add(symbol)
    }

    #[inline]
    fn add_symbol_ref(&mut self, symbol: &String) -> u16 {
        self.level.symbol_pool.add_ref(symbol)
    }

    #[inline]
    fn enter_level(&mut self, level: Level) {
        let level = std::mem::replace(&mut self.level, level);
        self.level.enclosing = Some(Box::new(level));
    }

    #[inline]
    fn exit_level(&mut self) -> (Function, Vec<Upvalue>) {
        let enclosing = self.level.enclosing.take().expect("no enclosing level");
        let level = std::mem::replace(&mut self.level, *enclosing);
        level.finish()
    }

    #[inline]
    fn enter_scope(&mut self) {
        self.level.scope_depth += 1;
    }

    fn exit_scope(&mut self, span: &Span) {
        self.level.scope_depth -= 1;

        while let Some(local) = self.level.locals.last() {
            if local.depth > self.level.scope_depth {
                if local.is_captured {
                    self.write_op(OpCode::CloseUpvalue, *span);
                } else {
                    self.write_op(OpCode::Pop, *span);
                };

                self.level.locals.pop();
            } else {
                break;
            }
        }
    }

    pub fn compile(mut self, ast: &Vec<Spanned<Expr>>) -> Result<VmEntry, ErrorS> {
        for expr in ast {
            // let pop = match expr.0 {
            //     Expr::Block(_) => false,
            //     Expr::Assignment(_, _) => false,
            //     Expr::Declaration(_, _) => false,
            //     Expr::MultiDeclaration(_, _) => false,
            //     Expr::MultiForLoop(_, _, _) => false,
            //     Expr::Function(_, _, _) => false,
            //     Expr::ForLoop(_, _, _) => false,
            //     Expr::Import(_) => false,
            //     Expr::Conditional(_, _, _) => false,

            //     _ => false,
            // };
            self.compile_expr(expr)?;
        }

        Ok(VmEntry {
            global_symbols: self.global_symbols.take(),
            function: self.level.finish().0,
        })
    }

    fn compile_block(&mut self, exprs: &Vec<Spanned<Expr>>, span: &Span) -> Result<(), ErrorS> {
        self.enter_scope();
        let mut span = span;
        for expr in exprs {
            self.compile_expr(expr)?;
            span = &expr.1;
        }

        // self.write_op(OpCode::Return, *span);
        self.exit_scope(span);
        Ok(())
    }

    fn compile_expr(&mut self, (expr, span): &Spanned<Expr>) -> Result<(), ErrorS> {
        match expr {
            Expr::Literal(value) => {
                let offset = self.add_constant(value.into());
                self.write_op_u16(OpCode::LoadConst, offset, *span);
            }

            Expr::Ident(name) => {
                self.get_variable(name, *span);
            }

            Expr::Block(exprs) => {
                self.compile_block(exprs, span)?;
            }

            Expr::Declaration((name, span), value) => {
                if self.is_global() {
                    self.compile_expr(value)?;

                    let offset = self.add_symbol_ref(name);
                    self.write_op_u16(OpCode::DefineGlobal, offset, *span);
                } else {
                    self.define_local(name, *span)?;
                    self.compile_expr(value)?;
                    self.set_variable(name, *span);
                }
            }

            Expr::Assignment((name, span), value) => {
                self.compile_expr(value)?;
                self.set_variable(name, *span);
            }

            Expr::Call((name, c_span), args) => {
                let arity: u8 = args
                    .len()
                    .try_into()
                    .map_err(|_| (OverflowError::TooManyArgs.into(), *span))?;

                self.get_variable(name, *c_span);

                for arg in args {
                    self.compile_expr(arg)?;
                }

                self.write_op_u8(OpCode::CallFunction, arity, *span);
            }

            Expr::Function((name, fn_span), arguments, body) => {
                let arity: u8 = arguments
                    .len()
                    .try_into()
                    .map_err(|_| (OverflowError::TooManyParams.into(), *span))?;

                let level = Level {
                    function: Function {
                        name: (name.clone(), *fn_span),
                        arity,
                        upvalue_count: 0,
                        chunk: Chunk::new(),
                    },
                    locals: Vec::new(),
                    upvalues: Vec::new(),
                    enclosing: None,
                    scope_depth: self.level.scope_depth + 1,
                    symbol_pool: Pool::new(),
                    constant_pool: Pool::new(),
                };

                self.enter_level(level);

                self.define_local(name, *fn_span)?;

                for (name, span) in arguments {
                    self.define_local(name, *span)?;
                }

                for expr in body {
                    self.compile_expr(expr)?;
                }

                // let last_span = self.level.function.chunk.last_span();
                // self.write_op(OpCode::Return, last_span);

                let (function, upvalues) = self.exit_level();
                let value = Value::Function(Rc::new(function));
                let offset = self.add_constant(value);
                if upvalues.len() > 0 {
                    self.write_op_u16(OpCode::CreateClosure, offset, *span);

                    for Upvalue(idx, is_local) in upvalues {
                        self.write_u8(is_local.into(), *span);
                        self.write_u16(idx, *span);
                    }
                } else {
                    self.write_op_u16(OpCode::CreateFunction, offset, *span);
                }

                // function compiled at this point
                if self.is_global() {
                    let offset = self.add_symbol_ref(name);
                    self.write_op_u16(OpCode::DefineGlobal, offset, *span);
                } else {
                    self.define_local(name, *span)?;
                    self.set_variable(name, *span);
                }
            }

            Expr::Conditional(condition, then, otherwise) => {
                self.compile_expr(condition)?;

                // if the condition is false, go to else
                let else_jump = self.write_jump(OpCode::JumpIfFalse, *span);
                // pop off condition
                self.write_op(OpCode::Pop, *span);

                self.compile_block(then, span)?;

                let end_jump = self.write_jump(OpCode::Jump, *span);

                // else
                self.patch_jump(else_jump, *span)?;
                // pop off condition
                self.write_op(OpCode::Pop, *span);

                if let Some(otherwise) = otherwise {
                    self.compile_block(otherwise, span)?;
                }

                self.patch_jump(end_jump, *span)?;
            }

            Expr::Binary(operand, lhs, rhs) => {
                self.compile_expr(lhs)?;
                self.compile_expr(rhs)?;

                let opcode = match operand {
                    Op::Add => OpCode::BinaryAdd,
                    Op::Sub => OpCode::BinarySubtract,
                    Op::Mul => OpCode::BinaryMultiply,
                    Op::Div => OpCode::BinaryDivide,
                    Op::Mod => OpCode::BinaryModulo,
                    Op::Pow => OpCode::BinaryPower,
                    Op::Eq => OpCode::BinaryEqual,
                    Op::Neq => OpCode::BinaryNotEqual,
                    Op::Lt => OpCode::BinaryLess,
                    Op::Gt => OpCode::BinaryGreater,
                    Op::Lte => OpCode::BinaryLessEqual,
                    Op::Gte => OpCode::BinaryGreaterEqual,
                    Op::And => OpCode::BinaryAnd,
                    Op::Or => OpCode::BinaryOr,
                    Op::Join => OpCode::BinaryJoin,

                    _ => unreachable!("unary op in binary expr"),
                };

                self.write_op(opcode, *span);
            }

            Expr::Unary(operand, rhs) => {
                self.compile_expr(rhs)?;

                let opcode = match operand {
                    Op::Not => OpCode::UnaryNot,
                    Op::Neg => OpCode::UnaryNegate,
                    Op::Spread => OpCode::UnarySpread,
                    _ => unreachable!("binary op in unary expr"),
                };

                self.write_op(opcode, *span);
            }

            _ => todo!(),
        }

        // if pop {
        //     self.write_op(OpCode::Pop, *span);
        // }

        Ok(())
    }

    fn get_variable(&mut self, name: &str, span: Span) {
        if let Some(local_idx) = self.level.resolve_local(name, false) {
            self.write_op_u16(OpCode::LoadLocal, local_idx, span);
        } else if let Some(upvalue_idx) = self.level.resolve_upvalue(name) {
            self.write_op_u16(OpCode::LoadUpvalue, upvalue_idx, span);
        } else {
            let offset = self.add_global_symbol(name.to_string());
            self.write_op_u16(OpCode::LoadGlobal, offset, span);
        }
    }

    fn set_variable(&mut self, name: &str, span: Span) {
        if let Some(local_idx) = self.level.resolve_local(name, false) {
            self.write_op_u16(OpCode::SetLocal, local_idx, span);
        } else if let Some(upvalue_idx) = self.level.resolve_upvalue(name) {
            self.write_op_u16(OpCode::SetUpvalue, upvalue_idx, span);
        } else {
            let offset = self.add_global_symbol(name.to_string());
            self.write_op_u16(OpCode::SetGlobal, offset, span);
        }
    }

    // todo: remove span and error
    fn define_local(&mut self, name: &str, _: Span) -> Result<(), ErrorS> {
        for i in (0..self.level.locals.len()).rev() {
            let local = &self.level.locals[i];
            if local.depth < self.level.scope_depth {
                break;
            }

            if local.name == name {
                self.level.symbol_pool.add_force(name.to_string());
                self.level.locals.push(Local {
                    name: name.to_string(),
                    depth: self.level.scope_depth,
                    is_captured: false,
                });

                // return Err((NameError::AlreadyDefined(name.to_string()).into(), span));

                return Ok(());
            }
        }

        self.add_symbol(name.to_string());
        self.level.locals.push(Local {
            name: name.to_string(),
            depth: self.level.scope_depth,
            is_captured: false,
        });

        Ok(())
    }
}

#[derive(Debug, Eq, PartialEq)]
struct Upvalue(u16, bool);

#[derive(Debug, Eq, PartialEq)]
struct Local {
    name: String,
    // scope depth (starts at 1)
    depth: u16,
    is_captured: bool,
}

pub struct Level {
    function: Function,
    locals: Vec<Local>,
    upvalues: Vec<Upvalue>,
    enclosing: Option<Box<Level>>,
    scope_depth: u16,

    symbol_pool: Pool<String, u16>,
    constant_pool: Pool<Value, u16>,
}

impl Level {
    fn finish(mut self) -> (Function, Vec<Upvalue>) {
        self.symbol_pool
            .take_inplace(&mut self.function.chunk.symbols);
        self.constant_pool
            .take_inplace(&mut self.function.chunk.constants);

        let last_span = self.function.chunk.last_span();
        self.function.chunk.write_op(OpCode::Return, last_span);

        (self.function, self.upvalues)
    }

    fn resolve_local(&mut self, name: &str, capture: bool) -> Option<u16> {
        for (i, local) in self.locals.iter_mut().enumerate().rev() {
            if local.name == name {
                if capture && !local.is_captured {
                    local.is_captured = true;
                }
                return Some(i as u16);
            }
        }

        None
    }

    fn resolve_upvalue(&mut self, name: &str) -> Option<u16> {
        let local_idx = match &mut self.enclosing {
            Some(parent) => parent.resolve_local(name, true),
            None => return None,
        };

        if let Some(local_idx) = local_idx {
            let upvalue_idx: u16 = self.add_upvalue(local_idx, true);
            return Some(upvalue_idx);
        };

        let upvalue_idx = match &mut self.enclosing {
            Some(parent) => parent.resolve_upvalue(name),
            None => return None,
        };

        if let Some(upvalue_idx) = upvalue_idx {
            let upvalue_idx = self.add_upvalue(upvalue_idx, false);
            return Some(upvalue_idx);
        };

        None
    }

    fn add_upvalue(&mut self, index: u16, is_local: bool) -> u16 {
        let upvalue = Upvalue(index, is_local);
        match self.upvalues.iter().position(|u| *u == upvalue) {
            Some(idx) => idx as u16,
            None => {
                self.upvalues.push(upvalue);

                let upvalues = self.upvalues.len() as u16;
                self.function.upvalue_count = upvalues;

                upvalues - 1
            }
        }
    }
}

pub struct VmEntry {
    pub global_symbols: Vec<String>,
    pub function: Function,
}
