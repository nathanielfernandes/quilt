<div align="center">

![quilt hero banner](assets/hero.png)

### A dynamically typed interpreted scripting language.

###### the name came from the language originally being created to edit images progamatically
---
    
</div>


## About
Quilt is a hobby project of mine. I designed it to be a simple, with a syntax similar to rust and python. I needed a language that I could use within my own projects that could be easily embedded and extended. I also wanted to learn more about how to build a programming language. 

[crafting interpreters](https://craftinginterpreters.com/a-bytecode-virtual-machine.html) was a great resource during the development of the backend.

## Features
- Dynamically typed
- Bytecode interpreter
- First class functions
- Closures
- Extendable native rust builtins
- Relatively fast (about `1.5x` - `2.5x` slower than python to calculate `fib(30)` & `fib(40)`)
- Python style dissasembler
<!-- - No garbage collection (yet..., for now all data is immutable, so it can be reference counted) -->

## Roadmap
- Spread operator
- Short circuiting
- `break` and `continue` statements
- `return` statements
- user created errors and error handling
- repl

## Examples
```rust
// includes 
include "std/math.ql"

// recursion
fn fib(n) {
    if n < 2 {
        n
    } else {
        fib(n - 1) + fib(n - 2)
    }
}

fib(30)


// loops
let i = 0
// assignements are expressions (as well as if statements and loops)
let res = while (i = i + 1) < 10 {
    i * 20
}

// native rust function
@print(res)

// for loops
for i in 0:10 {
    for j in [1, 2, 3, i] {
        @print(i, j)
    }
    @print(i)
}

// destructuring
let a, b = (1, 2)
for a, b, c in [[1, 2, 3], [4, 5, 6], [7, 8, 9]] {
    @print(a, b, c)
}

// closures
fn NewCounter() {
    let count = 0

    fn Increment() {
        count = count + 1
    }

    fn Decrement() {
        count = count - 1
    }

    fn Get() {
        count
    }

    [Increment, Decrement, Get]
}

let incr, decr, get = NewCounter()
```

## Disassembler Output
```rust
fn fib(n) {
    if n < 2 {
        n
    } else {
        fib(n - 1) + fib(n - 2)
    }
}

fib(30)
```
![disassembler](assets/fib_dis.png)


## Installation
coming soon...

## Benchmarks
coming soon...

## ByteCode
| Instructions `u8` | Args | Description |
| ----------- | ---- | ----------- |
| Halt | | Stops the program |
| Return | | Pops the top value from the stack, returns from the current function, and pushes the value back to the stack |
| Pop | | Pops the top value from the stack |
| PopMany | `n: u16` | Pops the top `n` values from the stack |
| Swap | | Swaps the top two values on the stack |
| SwapPop | | Swaps the top two values on the stack, then pops the top value |
| LoadConst | `idx: u16` | Pushes the constant at `idx` (in the constant pool) to the stack |
| LoadLocal | `offset: u16` | Pushes the local variable at `offset` (on the stack) to the stack |
| SetLocal | `offset: u16` | Peeks the top value from the stack, and sets the local variable at `offset` to the value |
| DefineGlobal | `id: u16` | Pops the top value from the stack, and defines a global variable with the value |
| LoadUpvalue | `offset: u16` | Pushes the upvalue at `offset` to the stack |
| SetUpvalue | `offset: u16` | Peeks the top value from the stack, and sets the upvalue at `offset` to the value |
| LoadGlobal | `id: u16` | Pushes the global variable with the id `id` to the stack |
| SetGlobal | `id: u16` | Peeks the top value from the stack, and sets the global variable with the id `id` to the value |
| CloseUpvalue | `offset: u16` | Closes the upvalue at `offset` |
| CreateFunction | `idx: u16` | Creates a function from the constant at `idx` and pushes it to the stack |
| CreateClosure | `idx: u16`, repeats(`offset: u16`, `is_local: u8`) for `n` captured variables | Creates a closure from the function at `idx` and pushes it to the stack |
| CreatePair | | Pops the top two values from the stack, and pushes a pair of the values to the stack |
| CreateArray | `n: u8` | Pops the top `n` values from the stack, and pushes an array of the values to the stack |
| CreateRange | | Pops the top two values from the stack, and pushes a range of the values to the stack |
| CallFunction | `n: u8` | Pops the top `n` values from the stack, and calls the function at the top of the stack with the values |
| CallBuiltin | `id: u16`, `n: u8` | Pops the top `n` values from the stack, and calls the builtin function with the id `id` with the values |
| EnterContext | `idx: u16`, `n_args: u8` | Creates a new context with builtin `idx`, and `n_args` arguments, and pushes it to the stack |
| ExitContext | | Pops the top context from the stack |
| BlockResult | | Pops the top value from the stack, and pushes it to the parent context |
| BlockReturn | | Pops the return value from the parent context, and pushes it to the stack |
| JumpIfFalse | `offset: u16` | Pops the top value from the stack, and jumps to `offset` if the value is `false` |
| JumpForward | `offset: u16` | Jumps forward `offset` instructions |
| JumpBackward | `offset: u16` | Jumps backward `offset` instructions |
| Unpack | `n: u8` | Pops the top value from the stack, and unpacks it into `n` values, pushing them to the stack |
| LoadNone | | Pushes `None` to the stack |
| LoadNoneMany | `n: u8` | Pushes `None` to the stack `n` times |
| NewLoopCtx | | Creates a new loop context, and pushes it to the stack |
| IterNext | `end: u16`, `loopctx: u16` | uses the iterable at the top of the stack, and pushes the next value to the stack, or jumps to `end` if the iterable is exhausted |
| UnaryNegate | | Pops the top value from the stack, and pushes the negated value to the stack |
| UnaryNot | | Pops the top value from the stack, and pushes the boolean negated value to the stack |
| UnarySpread | | Pops the top value from the stack, and pushes the spread value to the stack |
| BinaryAdd | | Pops the top two values from the stack, and pushes the sum to the stack |
| BinarySubtract | | Pops the top two values from the stack, and pushes the difference to the stack |
| BinaryMultiply | | Pops the top two values from the stack, and pushes the product to the stack |
| BinaryDivide | | Pops the top two values from the stack, and pushes the quotient to the stack |
| BinaryModulo | | Pops the top two values from the stack, and pushes the remainder to the stack |
| BinaryPower | | Pops the top two values from the stack, and pushes the power to the stack |
| BinaryEqual | | Pops the top two values from the stack, and pushes the equality to the stack |
| BinaryNotEqual | | Pops the top two values from the stack, and pushes the inequality to the stack |
| BinaryGreater | | Pops the top two values from the stack, and pushes the greater than to the stack |
| BinaryGreaterEqual | | Pops the top two values from the stack, and pushes the greater than or equal to to the stack |
| BinaryLess | | Pops the top two values from the stack, and pushes the less than to the stack |
| BinaryLessEqual | | Pops the top two values from the stack, and pushes the less than or equal to to the stack |
| BinaryAnd | | Pops the top two values from the stack, and pushes the boolean and to the stack |
| BinaryOr | | Pops the top two values from the stack, and pushes the boolean or to the stack |
| BinaryJoin | | Pops the top two values from the stack, and pushes the joined value to the stack |