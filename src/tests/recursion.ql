fn fib(n) {
    if n < 2 {
        n
    } else {
        fib(n - 1) + fib(n - 2)
    }
}

if fib(20) != 6765 {
    @err("fib(20) != 6765")
}
