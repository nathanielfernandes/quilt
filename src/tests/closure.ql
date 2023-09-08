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

@assert_eq(get(), 2)

{
    {
        let incr, decr, get = NewCounter()

        decr()

        incr()
        incr()
        incr()
        incr()
        incr()

        @assert_eq(get(), 4)
    }
}


@assert_eq(get(), 2)

fn SOUP() {
    let a = 0

    let b = [
        fn() {
            a = a + 1
        },
        fn() {
            a = a - 1
        },
        fn() {
            a
        }
    ]
}

fn GOOP() {
    let a = 0

    let b = [
        fn() {
            a = a + 1
        },
        fn() {
            a = a - 1
        },
        fn() {
            a
        }
    ]

    return b
}

let a = SOUP()
let a, b, c = GOOP()