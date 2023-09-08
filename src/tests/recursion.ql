fn fib(n) {
    if n < 2 {
        n
    } else {
        fib(n - 1) + fib(n - 2)
    }
}

@assert_eq(fib(20), 6765)