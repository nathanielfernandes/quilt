use std::rc::Rc;

use crate::{
    backend::utils::pool::Pool,
    prelude::{
        BuiltinFnMap, BuiltinListFn, CompileError, ErrorS, Expr, ImportError, Op, OverflowError,
        Span, Spanned,
    },
};

use super::{
    bytecode::OpCode,
    chunk::Chunk,
    value::{Function, Value},
};

pub struct Compiler<Data = ()> {
    level: Level,
    global_symbols: Pool<String, u16>,
    builtins: BuiltinFnMap<Data>,
}

impl<Data> Compiler<Data> {
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
            builtins: BuiltinFnMap::default(),
        }
    }

    pub fn add_builtins(&mut self, builtins: BuiltinListFn<Data>) {
        for (name, func) in builtins() {
            let name = self.global_symbols.add(name);
            self.builtins.insert(name, func);
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
        self.write_op_u16(op, 0, span);
        self.level.function.chunk.len() - 2
    }

    #[inline]
    fn write_jump_backward(&mut self, start: usize, span: Span) -> Result<(), ErrorS> {
        let jump = self.level.function.chunk.ops.len() + 3 - start;
        if jump > u16::MAX as usize {
            return Err((OverflowError::JumpTooLarge.into(), span));
        }
        self.write_op_u16(OpCode::JumpBackward, jump as u16, span);
        Ok(())
    }

    #[inline]
    fn patch_jump(&mut self, offset: usize, span: Span) -> Result<(), ErrorS> {
        let jump = self.level.function.chunk.ops.len() - 2 - offset;
        if jump > u16::MAX as usize {
            return Err((OverflowError::JumpTooLarge.into(), span));
        }
        self.patch_u16(offset, jump as u16);
        Ok(())
    }

    #[inline]
    fn patch_jump_ex(&mut self, offset: usize, extra: u16, span: Span) -> Result<(), ErrorS> {
        let jump = self.level.function.chunk.ops.len() - 2 - offset;
        if jump > u16::MAX as usize {
            return Err((OverflowError::JumpTooLarge.into(), span));
        }
        self.patch_u16(offset, jump as u16 - extra);
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

        let mut ops = Vec::new();
        while let Some(local) = self.level.locals.last() {
            if local.cleanup && local.depth > self.level.scope_depth {
                if local.is_captured {
                    ops.push(OpCode::CloseUpvalue);
                } else {
                    ops.push(OpCode::Pop);
                };

                self.level.locals.pop();
            } else {
                break;
            }
        }

        if ops.len() > 0 {
            self.write_op(OpCode::BlockResult, *span);

            for op in ops.into_iter() {
                self.write_op(op, *span);
            }

            self.write_op(OpCode::BlockReturn, *span);
        }
    }

    pub fn compile(mut self, ast: &Vec<Spanned<Expr>>) -> Result<Script<Data>, ErrorS> {
        self.compile_stmnts(ast)?;

        Ok(Script {
            global_symbols: self.global_symbols.take(),
            function: self.level.finish().0,
            builtins: self.builtins,
        })
    }

    fn compile_stmnts(&mut self, exprs: &Vec<Spanned<Expr>>) -> Result<Span, ErrorS> {
        let mut span = Span::default();

        if exprs.len() == 0 {
            return Ok(span);
        }

        for i in 0..exprs.len() - 1 {
            let expr = &exprs[i];
            self.compile_stmnt(expr, true)?;

            span = expr.1;
        }

        if let Some(expr) = exprs.last() {
            self.compile_stmnt(expr, false)?;

            span = expr.1;
        }

        Ok(span)
    }

    fn compile_block(&mut self, exprs: &Vec<Spanned<Expr>>) -> Result<(), ErrorS> {
        self.enter_scope();
        let span = self.compile_stmnts(exprs)?;
        self.exit_scope(&span);
        Ok(())
    }

    fn compile_stmnt(&mut self, stmnt: &Spanned<Expr>, pop: bool) -> Result<(), ErrorS> {
        let span = stmnt.1;
        match &stmnt.0 {
            Expr::Function((name, fn_span), arguments, body) => {
                let arity: u8 = arguments
                    .len()
                    .try_into()
                    .map_err(|_| (OverflowError::TooManyParams.into(), span))?;

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
                    scope_depth: self.level.scope_depth,
                    symbol_pool: Pool::new(),
                    constant_pool: Pool::new(),
                };

                self.enter_level(level);
                self.enter_scope();

                self.define_local(&name, true);
                // self.set_variable(name, *span);

                for (name, _) in arguments {
                    self.define_local(&name, true);
                }

                self.compile_stmnts(&body)?;
                // self.exit_scope(&span);

                let (function, upvalues) = self.exit_level();

                let value = Value::Function(Rc::new(function));
                let offset = self.add_constant(value);
                if upvalues.len() > 0 {
                    self.write_op_u16(OpCode::CreateClosure, offset, span);
                    for Upvalue(idx, is_local) in upvalues {
                        self.write_u8(is_local.into(), span);
                        self.write_u16(idx, span);
                    }
                } else {
                    self.write_op_u16(OpCode::CreateFunction, offset, span);
                }

                // function compiled at this point
                if self.is_global() {
                    let offset = self.add_global_symbol(name.to_string());
                    self.write_op_u16(OpCode::DefineGlobal, offset, span);
                } else {
                    self.define_local(&name, true);
                    self.set_variable(&name, span);
                }
            }
            Expr::Declaration((name, span), value) => {
                self.compile_expr(&value)?;
                if self.is_global() {
                    let offset = self.add_global_symbol(name.to_string());
                    self.write_op_u16(OpCode::DefineGlobal, offset, *span);
                } else {
                    self.define_local(&name, true);
                    self.set_variable(&name, *span);
                }
            }
            Expr::MultiDeclaration(names, value) => {
                self.compile_expr(&value)?;

                let arity: u8 = names
                    .len()
                    .try_into()
                    .map_err(|_| (OverflowError::TooMuchToUnpack.into(), span))?;

                self.write_op_u8(OpCode::Unpack, arity, span);

                for (name, span) in names {
                    if self.is_global() {
                        let offset = self.add_global_symbol(name.to_string());
                        self.write_op_u16(OpCode::DefineGlobal, offset, *span);
                    } else {
                        self.define_local(&name, true);
                        self.set_variable(&name, *span);
                    }
                }
            }
            Expr::Import((name, span), body) => {
                if self.level.scope_depth > 0 {
                    return Err((ImportError::ImportNotTopLevel.into(), *span));
                }

                match body {
                    Some(body) => {
                        for stmnt in body {
                            self.compile_stmnt(stmnt, true)?;
                        }
                    }
                    None => {
                        return Err((
                            ImportError::UnresolvedImport(name.to_string()).into(),
                            *span,
                        ))
                    }
                }
            }

            // Expr::ForLoop(varname, iterable, body) => {
            //     self.enter_scope();

            //     let start = self.level.function.chunk.len();

            //     // push loop ctx ( used to keep track of loop state )
            //     self.write_op(OpCode::NewLoopCtx, span);

            //     // compile iterable
            //     self.compile_expr(&iterable)?;

            //     let exit = self.write_jump(OpCode::JumpIfFalse, span);

            //     // compile body
            //     self.compile_stmnts(&body)?;
            // }
            _ => {
                self.compile_expr(stmnt)?;
                if pop {
                    self.write_op(OpCode::Pop, stmnt.1);
                }
            } // Expr::MultiForLoop(_, _, _) => {}
        }

        Ok(())
    }

    fn compile_expr(&mut self, (expr, span): &Spanned<Expr>) -> Result<(), ErrorS> {
        match expr {
            Expr::Literal(value) => {
                match value {
                    crate::prelude::ParserValue::None => {
                        self.write_op(OpCode::LoadNone, *span);
                    }
                    _ => {
                        let offset = self.add_constant(value.into());
                        self.write_op_u16(OpCode::LoadConst, offset, *span);
                    }
                }
                // self.if_pop(pop, *span);
            }

            Expr::Pair(left, right) => {
                self.compile_expr(left)?;
                self.compile_expr(right)?;

                self.write_op(OpCode::CreatePair, *span);

                // self.if_pop(pop, *span);
            }

            Expr::Array(items) => {
                let count = items.len();

                if count > u8::MAX as usize {
                    return Err((OverflowError::TooManyItems.into(), *span));
                }

                for item in items.iter().rev() {
                    self.compile_expr(item)?;
                }

                self.write_op_u8(OpCode::CreateArray, count as u8, *span);
            }

            Expr::Range(start, end) => {
                self.compile_expr(start)?;
                self.compile_expr(end)?;
                self.write_op(OpCode::CreateRange, *span);
            }

            Expr::Ident(name) => {
                self.get_variable(name, *span);
                // self.if_pop(pop, *span);
            }

            Expr::Block(exprs) => {
                self.compile_block(exprs)?;
                // self.if_pop(pop, *span);
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
                // self.if_pop(pop, *span);
            }

            Expr::BuiltinCall((name, c_span), args) => {
                let arity: u8 = args
                    .len()
                    .try_into()
                    .map_err(|_| (OverflowError::TooManyArgs.into(), *span))?;

                for arg in args {
                    self.compile_expr(arg)?;
                }

                self.write_op(OpCode::CallBuiltin, *span);

                let name = self.add_global_symbol(name.to_string());
                self.write_u16(name, *span);
                self.write_u8(arity, *c_span);
                // self.if_pop(pop, *span);
            }

            Expr::ContextWrapped((name, c_span), args, varname, body) => {
                let arity: u8 = args
                    .len()
                    .try_into()
                    .map_err(|_| (OverflowError::TooManyArgs.into(), *span))?;

                for arg in args {
                    self.compile_expr(arg)?;
                }

                self.write_op(OpCode::EnterContext, *span);

                let name = self.add_global_symbol(name.to_string());
                self.write_u16(name, *span);
                self.write_u8(arity, *c_span);

                self.enter_scope();

                if let Some((varname, span)) = varname {
                    self.define_local(varname, true);
                    self.set_variable(varname, *span);
                }

                self.compile_stmnts(body)?;

                self.exit_scope(span);
                self.write_op(OpCode::ExitContext, *span);
                // self.if_pop(pop, *span);
            }

            Expr::Conditional(condition, then, otherwise) => {
                self.compile_expr(condition)?;

                // if the condition is false, go to else
                let else_jump = self.write_jump(OpCode::JumpIfFalse, *span);
                // pop off condition
                self.write_op(OpCode::Pop, *span);

                self.compile_block(then)?;

                let end_jump = self.write_jump(OpCode::JumpForward, *span);

                // else
                self.patch_jump(else_jump, *span)?;
                // pop off condition
                self.write_op(OpCode::Pop, *span);

                if let Some(otherwise) = otherwise {
                    self.compile_block(otherwise)?;
                }

                self.patch_jump(end_jump, *span)?;
                // self.if_pop(pop, *span);
            }

            Expr::WhileLoop(cond, body) => {
                let cspan = cond.1;

                // push none in case loop does not run at all (loops are expressions)
                self.write_op(OpCode::LoadNone, *span);

                let start = self.level.function.chunk.len();

                self.compile_expr(&cond)?;

                let exit = self.write_jump(OpCode::JumpIfFalse, *span);

                // pop the condition
                self.write_op(OpCode::Pop, cspan);

                // pop off the none or the previous iteration value
                self.write_op(OpCode::Pop, cspan);

                // compile body
                self.compile_block(&body)?;

                self.write_jump_backward(start, cspan)?;
                self.patch_jump(exit, *span)?;

                // pop the condition
                self.write_op(OpCode::Pop, cspan);
            }

            Expr::ForLoop((varname, nspan), iterable, body) => {
                self.enter_scope();

                // initial loop value
                self.write_op(OpCode::LoadNone, *span);
                self.define_local(varname, true);
                self.set_variable(varname, *nspan);

                // reserve two stack slots for the loop context and the iteration value
                self.reserve_temp_local_space();
                self.reserve_temp_local_space();

                // push a new loop context (keeps track of the current iteration)
                self.write_op(OpCode::NewLoopCtx, *span);
                self.compile_expr(&iterable)?;

                self.write_op(OpCode::LoadNone, *span);
                let start = self.level.function.chunk.len();

                // IterNext takes (end jump u16) and assumes the loop context is on top of the stack
                let data_offset = self.level.locals.len() - 2;
                let exit = self.write_jump(OpCode::IterNext, *span);
                self.write_u16(data_offset as u16, *span);
                self.set_variable(varname, *nspan);

                self.write_op(OpCode::Pop, *span);
                self.write_op(OpCode::Pop, *span);

                // compile body
                self.compile_block(&body)?;

                // pop off last value
                // self.write_op(OpCode::Pop, *span);

                self.write_jump_backward(start, *nspan)?;
                // account for extra 2 op slots for the iternext
                self.patch_jump_ex(exit, 2, *nspan)?;

                self.exit_scope(span);

                // pop none and return value
                // self.write_op(OpCode::Swap, *span);
                // self.write_op(OpCode::Pop, *span);
                // self.write_op(OpCode::Swap, *span);
                // self.write_op(OpCode::Pop, *span);
                // self.write_op(OpCode::Swap, *span);
                // self.write_op(OpCode::Pop, *span);
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
                // self.if_pop(pop, *span);
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
                // self.if_pop(pop, *span);
            }

            _ => Err((CompileError::StatementAsExpression.into(), *span))?,
        }

        // if pop
        //     && match expr {
        //         // Expr::Assignment(_, _) => false,
        //         Expr::Declaration(_, _) => false,
        //         Expr::MultiDeclaration(_, _) => false,
        //         Expr::MultiForLoop(_, _, _) => false,
        //         Expr::Function(_, _, _) => false,
        //         Expr::ForLoop(_, _, _) => false,
        //         Expr::Import(_, _) => false,
        //         Expr::Conditional(_, _, _) => false,
        //         Expr::ContextWrapped(_, _, _, _) => false,

        //         _ => true,
        //     }
        // {
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

    #[inline]
    fn reserve_temp_local_space(&mut self) {
        self.define_local("", true)
    }
    // todo: remove unused span and error
    fn define_local(&mut self, name: &str, cleanup: bool) {
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
                    cleanup,
                });

                return;
            }
        }

        self.add_symbol(name.to_string());
        self.level.locals.push(Local {
            name: name.to_string(),
            depth: self.level.scope_depth,
            is_captured: false,
            cleanup,
        });
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

    // whether to clean up the local variable after a scope ends
    cleanup: bool,
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

        // if let Some(last_op) = self.function.chunk.ops.last() {
        //     if *last_op == Pop {
        //         self.function.chunk.ops.pop();
        //     }
        // }

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
        match self.upvalues.iter().position(|u| u == &upvalue) {
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

pub struct Script<Data> {
    pub global_symbols: Vec<String>,
    pub function: Function,
    pub builtins: BuiltinFnMap<Data>,
}
