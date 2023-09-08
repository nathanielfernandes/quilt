#![allow(non_upper_case_globals)]

pub trait ByteCodeName {
    fn name(&self) -> &'static str;
}

macro_rules! bytecode {
    {$first:ident, $($name:ident),*} => {
        iota::iota! {
            pub const $first: u8 = iota;
            $( ,$name )*
        }

        impl ByteCodeName for u8 {
            fn name(&self) -> &'static str {
                match *self {
                    $first => stringify!($first),
                    $( $name => stringify!($name), )*
                    _ => "Unknown",
                }
            }
        }
    };
}

bytecode! {
    Halt,
    Return,
    Pop,
    PopMany, // u16
    Swap,
    SwapPop,
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
    CreateArray, // u8
    CreateRange,
    CallFunction, // u8
    CallBuiltin,  // u16, u8
    EnterContext, // u16, u8
    ExitContext,
    BlockResult,
    BlockReturn,
    JumpIfFalse,  // u16
    JumpForward,  // u16
    JumpBackward, // u16
    Unpack,       // u8
    LoadNone,
    LoadNoneMany, // u8
    NewLoopCtx,
    // gets a value from an iterable and pushes it to the stack
    // if the iterable is exhausted, jumps to the end of the loop
    IterNext, // u16 (jump if end)
    IndexGet,
    IndexSet,
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
    BinaryJoin,

    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseNot,
    BitwiseLeftShift,
    BitwiseRightShift
}

pub trait ByteCode {
    fn insert_op(&mut self, idx: usize, op: u8);
    fn insert_u8(&mut self, idx: usize, value: u8);
    fn insert_u16(&mut self, idx: usize, value: u16);

    fn patch_op(&mut self, idx: usize, op: u8);
    fn patch_u8(&mut self, idx: usize, value: u8);
    fn patch_u16(&mut self, idx: usize, value: u16);

    fn write_op(&mut self, op: u8);
    fn write_u8(&mut self, value: u8);
    fn write_u16(&mut self, value: u16);

    fn read_op(&self, idx: usize) -> u8;
    fn read_u8(&self, idx: usize) -> u8;
    fn read_u16(&self, idx: usize) -> u16;
}

impl ByteCode for Vec<u8> {
    #[inline]
    fn insert_op(&mut self, idx: usize, op: u8) {
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
    fn patch_op(&mut self, idx: usize, op: u8) {
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
    fn write_op(&mut self, op: u8) {
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
    fn read_op(&self, idx: usize) -> u8 {
        self[idx]
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
    chunk.write_op(LoadConst);
    chunk.write_u16(400);
    chunk.write_op(Return);

    assert_eq!(chunk.read_u8(0), LoadConst as u8);
    assert_eq!(chunk.read_u16(1), 400);
    assert_eq!(chunk.read_u8(3), Return as u8);
}
