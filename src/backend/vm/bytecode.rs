#![allow(non_upper_case_globals)]

macro_rules! gen_bytecode {
    ($($name:ident),*) => {
        #[repr(u8)]
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub enum OpCode {
            $($name),*
        }

        $(
            pub const $name: u8 = OpCode::$name as u8;
        )*
    }
}

gen_bytecode!(
    Halt,
    Return,
    Pop,
    LoadConst, // u16
    LoadLocal, // u16
    SetLocal,  // u16
    DefineGlobal,
    LoadUpvalue, // u16
    SetUpvalue,  // u16
    LoadGlobal,  // u16
    SetGlobal,   // u16
    CloseUpvalue,
    CreateFunction,
    CreateClosure,
    CreatePair,
    CallFunction, // u8
    CallBuiltin,  // u16, u8
    EnterContext, // u16, u8
    ExitContext,
    BlockResult,
    BlockReturn,
    JumpIfFalse, // u16
    Jump,        // u16
    Unpack,      // u8
    UnaryNegate,
    UnaryNot,
    UnarySpread,
    BinaryAdd,
    BinarySubtract,
    BinaryMultiply,
    BinaryDivide,
    BinaryModulo,
    BinaryPower,
    BinaryEqual,
    BinaryNotEqual,
    BinaryGreater,
    BinaryGreaterEqual,
    BinaryLess,
    BinaryLessEqual,
    BinaryAnd,
    BinaryOr,
    BinaryJoin
);

impl From<OpCode> for u8 {
    fn from(op: OpCode) -> Self {
        op as u8
    }
}

impl From<u8> for OpCode {
    fn from(op: u8) -> Self {
        unsafe { std::mem::transmute(op) }
    }
}

pub trait ByteCode {
    fn insert_op(&mut self, idx: usize, op: OpCode);
    fn insert_u8(&mut self, idx: usize, value: u8);
    fn insert_u16(&mut self, idx: usize, value: u16);

    fn patch_op(&mut self, idx: usize, op: OpCode);
    fn patch_u8(&mut self, idx: usize, value: u8);
    fn patch_u16(&mut self, idx: usize, value: u16);

    fn write_op(&mut self, op: OpCode);
    fn write_u8(&mut self, value: u8);
    fn write_u16(&mut self, value: u16);

    fn read_op(&self, idx: usize) -> OpCode;
    fn read_u8(&self, idx: usize) -> u8;
    fn read_u16(&self, idx: usize) -> u16;
}

impl ByteCode for Vec<u8> {
    #[inline]
    fn insert_op(&mut self, idx: usize, op: OpCode) {
        self.insert(idx, op as u8);
    }

    #[inline]
    fn insert_u8(&mut self, idx: usize, value: u8) {
        self.insert(idx, value);
    }

    #[inline]
    fn insert_u16(&mut self, idx: usize, value: u16) {
        let [a, b] = value.to_le_bytes();
        self.insert(idx, b);
        self.insert(idx, a);
    }

    #[inline]
    fn patch_op(&mut self, idx: usize, op: OpCode) {
        self[idx] = op as u8;
    }

    #[inline]
    fn patch_u8(&mut self, idx: usize, value: u8) {
        self[idx] = value;
    }

    #[inline]
    fn patch_u16(&mut self, idx: usize, value: u16) {
        let [a, b] = value.to_le_bytes();
        self[idx] = a;
        self[idx + 1] = b;
    }

    #[inline]
    fn write_op(&mut self, op: OpCode) {
        self.push(op as u8);
    }

    #[inline]
    fn write_u8(&mut self, value: u8) {
        self.push(value);
    }

    #[inline]
    fn write_u16(&mut self, value: u16) {
        self.extend_from_slice(&value.to_le_bytes());
    }

    #[inline]
    fn read_op(&self, idx: usize) -> OpCode {
        OpCode::from(self[idx])
    }

    #[inline]
    fn read_u8(&self, idx: usize) -> u8 {
        self[idx]
    }

    #[inline]
    fn read_u16(&self, idx: usize) -> u16 {
        u16::from_le_bytes([self[idx], self[idx + 1]])
    }
}

#[test]
fn test_bytecode() {
    let mut chunk = Vec::new();
    chunk.write_op(OpCode::LoadConst);
    chunk.write_u16(400);
    chunk.write_op(OpCode::Return);

    assert_eq!(chunk.read_u8(0), OpCode::LoadConst as u8);
    assert_eq!(chunk.read_u16(1), 400);
    assert_eq!(chunk.read_u8(3), OpCode::Return as u8);

    // test from
    let op = OpCode::LoadConst;
    let op2 = u8::from(op);
    assert_eq!(op, OpCode::from(op2));
}
