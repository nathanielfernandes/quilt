// bitwise tests

let x = 5 & 1
x = 5 | 20
x = 5 ^ 1

// left shift
x = 1 << 3

// right shift
x = 8 >> 1

@assert_eq(x, 4)

let b = 0b001 | 0b010

@assert_eq(b, 0b011)

// bitwise not
let c = ~0b001

@assert_eq(c, -2)
@assert_eq(0x100, 256)
@assert_eq(0o100, 64)
