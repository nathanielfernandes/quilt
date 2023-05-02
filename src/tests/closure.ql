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

    return [Increment, Decrement, Get]
}

let incr, decr, get = NewCounter()


incr()
incr()
incr()

decr()


if (get() != 2) {
    @err("Expected 2, got " .. get())
}


{
    {
        let incr, decr, get = NewCounter()

        decr()

        incr()
        incr()
        incr()
        incr()
        incr()

        if (get() != 4) {
            @err("Expected 4, got " .. get())
        }
    }
}


if (get() != 2) {
    @err("Expected 2, got " .. get())
}

