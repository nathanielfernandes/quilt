use std::rc::Rc;

use bytecode::{bytecode::*, chunk::Chunk};
use common::{
    error::*,
    pool::Pool,
    span::{Span, Spanned},
    vecc::Vecc,
};
use interpreter::{
    value::{Function, Value},
    Script,
};
use parser::{
    literal::Literal,
    node::{Node, Op},
};

pub struct Compiler {
    level: Level,
    global_symbols: Pool<String, u16>,
    // builtins: BuiltinFnMap<Data>,
}

impl Compiler {
    pub fn new(name: String) -> Self {
        Self {
            level: Level {
                function: Function {
                    name: (name, Span::default()),
                    arity: 0,
                    upvalue_count: 0,
                    chunk: Chunk::new(),
                },
                locals: Vec::new(),
                upvalues: Vec::new(),
                enclosing: None,
                scope_depth: 0, // 0 is global scope
                symbol_pool: Pool::new(),
                constant_pool: Pool::<Value, u16>::new(),
            },
            global_symbols: Pool::new(),
            // builtins: BuiltinFnMap::default(),
        }
    }

    // pub fn add_builtins(&mut self, builtins: BuiltinListFn<Data>) {
    //     for (name, func) in builtins() {
    //         let name = self.global_symbols.add(name);
    //         self.builtins.insert(name, func);
    //     }
    // }

    #[inline]
    fn is_global(&self) -> bool {
        self.level.scope_depth == 0
    }

    #[inline]
    fn write_op(&mut self, op: u8, span: Span) {
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

    // fn patch_op(&mut self, offset: usize, op: u8) {
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
    fn write_op_u8(&mut self, op: u8, byte: u8, span: Span) {
        self.level.function.chunk.write_op(op, span);
        self.level.function.chunk.write_u8(byte, span);
    }

    #[inline]
    fn write_op_u16(&mut self, op: u8, byte: u16, span: Span) {
        self.level.function.chunk.write_op(op, span);
        self.level.function.chunk.write_u16(byte, span);
    }

    #[inline]
    fn write_jump(&mut self, op: u8, span: Span) -> usize {
        self.write_op_u16(op, 0, span);
        self.level.function.chunk.len() - 2
    }

    #[inline]
    fn write_jump_backward(&mut self, start: usize, span: Span) -> Result<(), ErrorS> {
        let jump = self.level.function.chunk.ops.len() + 3 - start;
        if jump > u16::MAX as usize {
            return Err((OverflowError::JumpTooLarge.into(), span));
        }
        self.write_op_u16(JumpBackward, jump as u16, span);
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

    // #[inline]
    // fn patch_jump_ex(&mut self, offset: usize, extra: u16, span: Span) -> Result<(), ErrorS> {
    //     let jump = self.level.function.chunk.ops.len() - 2 - offset;
    //     if jump > u16::MAX as usize {
    //         return Err((OverflowError::JumpTooLarge.into(), span));
    //     }
    //     self.patch_u16(offset, jump as u16 - extra);
    //     Ok(())
    // }

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
                    ops.push(CloseUpvalue);
                } else {
                    ops.push(Pop);
                };

                self.level.locals.pop();
            } else {
                break;
            }
        }

        if ops.len() > 0 {
            self.write_op(BlockResult, *span);

            let mut pop_queue = 0_u16;

            for op in ops.into_iter() {
                if Pop == op {
                    pop_queue += 1;
                    continue;
                } else {
                    if pop_queue >= 2 {
                        self.write_op_u16(PopMany, pop_queue, *span)
                    } else if pop_queue == 1 {
                        self.write_op(Pop, *span);
                    }
                    pop_queue = 0;
                }

                self.write_op(op, *span);
            }

            if pop_queue >= 2 {
                self.write_op_u16(PopMany, pop_queue, *span)
            } else if pop_queue == 1 {
                self.write_op(Pop, *span);
            }

            self.write_op(BlockReturn, *span);
        }
    }

    pub fn compile(ast: &Vec<Spanned<Node>>) -> Result<Script, ErrorS> {
        let mut compiler = Compiler::new(String::from("@__main__"));
        compiler.set_main_span(ast);
        compiler.compile_stmnts(ast)?;
        Ok(compiler.take_as_script())
    }

    pub fn take_as_script(self) -> Script {
        Script {
            global_symbols: self.global_symbols,
            function: self.level.finish().0,
        }
    }

    pub fn clone_as_script(&self) -> Script {
        Script {
            global_symbols: self.global_symbols.clone(),
            function: self.level.clone().finish().0,
        }
    }

    pub fn reset_state(&mut self, name: String) {
        self.level = Level {
            function: Function {
                name: (name, Span::default()),
                arity: 0,
                upvalue_count: 0,
                chunk: Chunk::new(),
            },
            locals: Vec::new(),
            upvalues: Vec::new(),
            enclosing: None,
            scope_depth: 0, // 0 is global scope
            symbol_pool: Pool::new(),
            constant_pool: Pool::<Value, u16>::new(),
        };
    }

    pub fn set_main_span(&mut self, ast: &Vec<Spanned<Node>>) {
        if let Some((_, span)) = ast.first() {
            self.level.function.name.1 = *span;
        }
    }

    pub fn compile_stmnts(&mut self, exprs: &Vec<Spanned<Node>>) -> Result<Span, ErrorS> {
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

    fn compile_block(&mut self, exprs: &Vec<Spanned<Node>>) -> Result<(), ErrorS> {
        self.enter_scope();
        let span = self.compile_stmnts(exprs)?;
        self.exit_scope(&span);
        Ok(())
    }

    fn compile_stmnt(&mut self, stmnt: &Spanned<Node>, pop_last: bool) -> Result<(), ErrorS> {
        let span = stmnt.1;

        match &stmnt.0 {
            Node::Return(expr) => {
                if self.level.scope_depth == 0 {
                    return Err((SyntaxError::ReturnOutsideFunction.into(), span));
                }

                if let Some(expr) = expr {
                    self.compile_expr(expr)?;
                } else {
                    self.write_op(LoadNone, span);
                }

                self.write_op(Return, span);
            }
            Node::Function {
                name: (name, fn_span),
                args,
                body,
            } => {
                let arity: u8 = args
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

                for (name, _) in args {
                    self.define_local(&name, true);
                }

                self.compile_stmnts(&body)?;
                // self.exit_scope(&span);

                let (function, upvalues) = self.exit_level();

                let value = Value::Function(Rc::new(function));
                let offset = self.add_constant(value);
                if upvalues.len() > 0 {
                    self.write_op_u16(CreateClosure, offset, span);
                    for Upvalue(idx, is_local) in upvalues {
                        self.write_u8(is_local.into(), span);
                        self.write_u16(idx, span);
                    }
                } else {
                    self.write_op_u16(CreateFunction, offset, span);
                }

                // function compiled at this point
                if self.is_global() {
                    let offset = self.add_global_symbol(name.to_string());
                    self.write_op_u16(DefineGlobal, offset, span);
                } else {
                    self.define_local(&name, true);
                    self.set_variable(&name, span);
                }
            }

            Node::Declaration((name, span), value) => {
                self.compile_expr(&value)?;
                if self.is_global() {
                    let offset = self.add_global_symbol(name.to_string());
                    self.write_op_u16(DefineGlobal, offset, *span);
                } else {
                    self.define_local(&name, true);
                    self.set_variable(&name, *span);
                }
            }

            Node::MultiDeclaration(names, value) => {
                self.compile_expr(&value)?;

                let arity: u8 = names
                    .len()
                    .try_into()
                    .map_err(|_| (OverflowError::TooMuchToUnpack.into(), span))?;

                self.write_op_u8(Unpack, arity, span);

                if self.is_global() {
                    for (name, span) in names.iter() {
                        let offset = self.add_global_symbol(name.to_string());
                        self.write_op_u16(DefineGlobal, offset, *span);
                    }
                } else {
                    for (name, _) in names.iter().rev() {
                        self.define_local(&name, true);
                    }
                }
            }

            Node::Include((name, span), body) => {
                if self.level.scope_depth > 0 {
                    return Err((IncludeError::IncludeNotTopLevel.into(), *span));
                }

                match body {
                    Some(body) => {
                        for stmnt in body {
                            self.compile_stmnt(stmnt, true)?;
                        }
                    }
                    None => {
                        return Err((
                            IncludeError::UnresolvedInclude(name.to_string()).into(),
                            *span,
                        ))
                    }
                }
            }

            _ => {
                self.compile_expr(stmnt)?;
                if pop_last {
                    self.write_op(Pop, span);
                }
            }
        }

        Ok(())
    }

    fn compile_expr(&mut self, (expr, span): &Spanned<Node>) -> Result<(), ErrorS> {
        match expr {
            Node::Literal(value) => match value {
                Literal::None => {
                    self.write_op(LoadNone, *span);
                }
                _ => {
                    let offset = self.add_constant(literal_to_value(value));
                    self.write_op_u16(LoadConst, offset, *span);
                }
            },
            Node::Pair(left, right) => {
                self.compile_expr(left)?;
                self.compile_expr(right)?;

                self.write_op(CreatePair, *span);
            }

            Node::Array(items) => {
                let count = items.len();

                if count > u8::MAX as usize {
                    return Err((OverflowError::TooManyItems.into(), *span));
                }

                for item in items.iter().rev() {
                    self.compile_expr(item)?;
                }

                self.write_op_u8(CreateArray, count as u8, *span);
            }

            Node::Range(start, end) => {
                self.compile_expr(start)?;
                self.compile_expr(end)?;
                self.write_op(CreateRange, *span);
            }

            Node::Identifier(name) => {
                self.get_variable(name, *span);
            }

            Node::Block(exprs) => {
                self.compile_block(exprs)?;
            }

            Node::Assignment((name, span), value) => {
                self.compile_expr(value)?;
                self.set_variable(name, *span);
            }

            Node::Call(expr, args) => {
                let arity: u8 = args
                    .len()
                    .try_into()
                    .map_err(|_| (OverflowError::TooManyArgs.into(), *span))?;

                // self.get_variable(name, *c_span);

                self.compile_expr(expr)?;

                for arg in args {
                    self.compile_expr(arg)?;
                }

                self.write_op_u8(CallFunction, arity, *span);
                // self.if_pop(pop, *span);
            }

            Node::IndexGet(expr, idx) => {
                self.compile_expr(idx)?;
                self.compile_expr(expr)?;
                self.write_op(IndexGet, *span);
            }

            // Node::IndexSet(target, idx, value) => {
            //     self.compile_expr(value)?;
            //     self.compile_expr(idx)?;
            //     self.compile_expr(target)?;
            //     self.write_op(IndexSet, *span);
            // }
            Node::BuiltinCall((name, c_span), args) => {
                let arity: u8 = args
                    .len()
                    .try_into()
                    .map_err(|_| (OverflowError::TooManyArgs.into(), *span))?;

                for arg in args {
                    self.compile_expr(arg)?;
                }

                self.write_op(CallBuiltin, *span);

                let name = self.add_global_symbol(name.to_string());
                self.write_u16(name, *span);
                self.write_u8(arity, *c_span);
                // self.if_pop(pop, *span);
            }

            Node::ContextWrapped {
                name: (name, c_span),
                args,
                variable: varname,
                body,
            } => {
                let arity: u8 = args
                    .len()
                    .try_into()
                    .map_err(|_| (OverflowError::TooManyArgs.into(), *span))?;

                for arg in args {
                    self.compile_expr(arg)?;
                }

                self.write_op(EnterContext, *span);

                let name = self.add_global_symbol(name.to_string());
                self.write_u16(name, *span);
                self.write_u8(arity, *c_span);

                self.enter_scope();

                // if there is an 'as varname' part, define the variable
                // if not just pop if off
                if let Some((varname, span)) = varname {
                    self.define_local(varname, true);
                    self.set_variable(varname, *span);
                } else {
                    self.write_op(Pop, *span)
                }

                self.compile_stmnts(body)?;

                self.exit_scope(span);
                self.write_op(ExitContext, *span);
                // self.if_pop(pop, *span);
            }

            Node::Conditional {
                condition,
                then,
                otherwise,
            } => {
                // push none in case then oes not run at all
                self.write_op(LoadNone, *span);

                self.compile_expr(condition)?;

                // if the condition is false, go to else
                let else_jump = self.write_jump(JumpIfFalse, *span);

                // pop off condition and default none
                self.write_op_u16(PopMany, 2, *span);

                self.compile_block(then)?;

                let end_jump = self.write_jump(JumpForward, *span);

                // else
                self.patch_jump(else_jump, *span)?;

                if let Some(otherwise) = otherwise {
                    // pop off condition
                    // pop off default none
                    self.write_op_u16(PopMany, 2, *span);

                    self.compile_block(otherwise)?;
                } else {
                    // pop off condition
                    self.write_op(Pop, *span);
                }

                self.patch_jump(end_jump, *span)?;
            }

            Node::WhileLoop(cond, body) => {
                let cspan = cond.1;

                // push none in case loop does not run at all (loops are expressions)
                self.write_op(LoadNone, *span);

                let start = self.level.function.chunk.len();

                self.compile_expr(&cond)?;

                let exit = self.write_jump(JumpIfFalse, *span);

                // pop the condition
                // pop off the none or the previous iteration value
                self.write_op_u16(PopMany, 2, cspan);

                // compile body
                self.compile_block(&body)?;

                self.write_jump_backward(start, cspan)?;
                self.patch_jump(exit, *span)?;

                // pop the condition
                self.write_op(Pop, cspan);
            }

            Node::ForLoop {
                variable: (varname, nspan),
                iterable,
                body,
            } => {
                self.enter_scope();

                // reserve two stack slots for the loop context and the iteration value
                self.reserve_temp_local_space(2);

                // push a new loop context (keeps track of the current iteration)
                self.write_op(NewLoopCtx, *span);
                self.compile_expr(&iterable)?;

                self.enter_scope();

                self.write_op(LoadNone, *span);
                let start = self.level.function.chunk.len();

                // IterNext takes (end jump u16) and assumes the loop context is on top of the stack
                let exit = self.write_jump(IterNext, *span);

                // pop off default none
                self.write_op(SwapPop, *span);

                self.define_local(varname, true);
                // self.set_variable(varname, *nspan);

                // compile body
                self.compile_stmnts(&body)?;

                self.exit_scope(span);

                self.write_jump_backward(start, *nspan)?;
                self.patch_jump(exit, *nspan)?;

                self.exit_scope(span);
            }

            Node::MultiForLoop {
                variables: varnames,
                iterable,
                body,
            } => {
                self.enter_scope();

                let len = varnames.len();
                if len > u8::MAX as usize {
                    return Err((OverflowError::TooMuchToUnpack.into(), *span));
                }
                let arity: u8 = len as u8;

                // reserve two stack slots for the loop context and the iteration value
                self.reserve_temp_local_space(2);

                // push a new loop context (keeps track of the current iteration)
                self.write_op(NewLoopCtx, *span);
                self.compile_expr(&iterable)?;

                self.enter_scope();
                // push a default return value
                self.write_op(LoadNone, *span);

                let start = self.level.function.chunk.len();

                // IterNext takes (end jump u16) and assumes the loop context is on top of the stack
                let exit = self.write_jump(IterNext, *span);

                // pop default return
                self.write_op(SwapPop, *span);

                self.write_op_u8(Unpack, arity, iterable.1);
                for (varname, _) in varnames.iter().rev() {
                    self.define_local(varname, true);
                    // self.set_variable_ex(varname, i as u16, 0, *span);
                }

                // compile body
                self.compile_stmnts(&body)?;

                self.exit_scope(span);

                self.write_jump_backward(start, *span)?;
                self.patch_jump(exit, *span)?;

                // pop off the loop context
                // self.write_op_u16(PopMany, 2, *span);

                self.exit_scope(span);
            }

            Node::Lambda(args, body) => {
                let arity: u8 = args
                    .len()
                    .try_into()
                    .map_err(|_| (OverflowError::TooManyParams.into(), *span))?;

                let name = "@__lambda__";

                let level = Level {
                    function: Function {
                        name: (name.to_owned(), *span),
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

                for (name, _) in args {
                    self.define_local(&name, true);
                }

                self.compile_stmnts(&body)?;

                let (function, upvalues) = self.exit_level();

                let value = Value::Function(Rc::new(function));
                let offset = self.add_constant(value);
                if upvalues.len() > 0 {
                    self.write_op_u16(CreateClosure, offset, *span);
                    for Upvalue(idx, is_local) in upvalues {
                        self.write_u8(is_local.into(), *span);
                        self.write_u16(idx, *span);
                    }
                } else {
                    self.write_op_u16(CreateFunction, offset, *span);
                }
            }

            Node::Binary(operand, lhs, rhs) => {
                self.compile_expr(lhs)?;
                self.compile_expr(rhs)?;

                let opcode = match operand {
                    Op::Add => BinaryAdd,
                    Op::Sub => BinarySubtract,
                    Op::Mul => BinaryMultiply,
                    Op::Div => BinaryDivide,
                    Op::Mod => BinaryModulo,
                    Op::Pow => BinaryPower,
                    Op::Eq => BinaryEqual,
                    Op::Neq => BinaryNotEqual,
                    Op::Lt => BinaryLess,
                    Op::Gt => BinaryGreater,
                    Op::Lte => BinaryLessEqual,
                    Op::Gte => BinaryGreaterEqual,
                    Op::And => BinaryAnd,
                    Op::Or => BinaryOr,
                    Op::Join => BinaryJoin,

                    Op::BitwiseAnd => BitwiseAnd,
                    Op::BitwiseOr => BitwiseOr,
                    Op::BitwiseXor => BitwiseXor,
                    Op::BitwiseLeftShift => BitwiseLeftShift,
                    Op::BitwiseRightShift => BitwiseRightShift,

                    _ => unreachable!("unary op in binary expr"),
                };

                self.write_op(opcode, *span);
            }

            Node::Unary(operand, rhs) => {
                let opt = match (operand, &rhs.0) {
                    (Op::Neg, Node::Literal(lit)) => match lit {
                        Literal::Int(n) => Some(Literal::Int(-n)),
                        Literal::Float(n) => Some(Literal::Float(-n)),
                        _ => None,
                    },
                    (Op::Not, Node::Literal(lit)) => match lit {
                        Literal::Bool(b) => Some(Literal::Bool(!b)),
                        _ => None,
                    },
                    _ => None,
                };

                if let Some(lit) = opt {
                    let offset = self.add_constant(literal_to_value(&lit));
                    self.write_op_u16(LoadConst, offset, *span);
                } else {
                    self.compile_expr(rhs)?;

                    let opcode = match operand {
                        Op::Not => UnaryNot,
                        Op::Neg => UnaryNegate,
                        Op::BitwiseNot => BitwiseNot,
                        // Op::Spread => UnarySpread,
                        _ => unreachable!("binary op in unary expr"),
                    };

                    self.write_op(opcode, *span);
                }
            }

            _ => {
                return Err((SyntaxError::StatementAsExpression.into(), *span));
            }
        }

        Ok(())
    }

    fn get_variable(&mut self, name: &str, span: Span) {
        if let Some(local_idx) = self.level.resolve_local(name, false) {
            self.write_op_u16(LoadLocal, local_idx, span);
        } else if let Some(upvalue_idx) = self.level.resolve_upvalue(name) {
            self.write_op_u16(LoadUpvalue, upvalue_idx, span);
        } else {
            let offset = self.add_global_symbol(name.to_string());
            self.write_op_u16(LoadGlobal, offset, span);
        }
    }

    fn set_variable(&mut self, name: &str, span: Span) {
        if let Some(local_idx) = self.level.resolve_local(name, false) {
            self.write_op_u16(SetLocal, local_idx, span);
        } else if let Some(upvalue_idx) = self.level.resolve_upvalue(name) {
            self.write_op_u16(SetUpvalue, upvalue_idx, span);
        } else {
            let offset = self.add_global_symbol(name.to_string());
            self.write_op_u16(SetGlobal, offset, span);
        }
    }

    #[inline]
    fn reserve_temp_local_space(&mut self, space: usize) {
        for _ in 0..space {
            // cannot be accessed by name because of the '#' prefix
            let name = format!("#__temp_{}", self.level.locals.len());
            self.define_local(&name, true)
        }
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

#[derive(Debug, Clone, Eq, PartialEq)]
struct Upvalue(u16, bool);

#[derive(Debug, Eq, Clone, PartialEq)]
struct Local {
    name: String,
    // scope depth (starts at 1)
    depth: u16,
    is_captured: bool,

    // whether to clean up the local variable after a scope ends
    cleanup: bool,
}

#[derive(Clone)]
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

        if self.scope_depth == 0 {
            let last_span = self.function.chunk.last_span();
            self.function.chunk.write_op(Return, last_span);
        }

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

pub fn literal_to_value(literal: &Literal) -> Value {
    match literal {
        Literal::None => Value::None,
        Literal::Bool(b) => Value::Bool(*b),
        Literal::Int(n) => Value::Int(*n),
        Literal::Float(n) => Value::Float(*n),
        Literal::Color(c) => Value::Color(*c),
        Literal::String(s) => Value::String(Rc::new(s.clone())),
        Literal::Array(arr) => Value::Array(Rc::new(Vecc::force_new(
            arr.iter().map(literal_to_value).collect::<Vec<Value>>(),
        ))),
        Literal::Pair(l, r) => Value::Pair(Rc::new((literal_to_value(l), literal_to_value(r)))),
        Literal::Range(s, e) => Value::Range(*s, *e),
    }
}
