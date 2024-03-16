fn fizzbuzz(n) {
    for i in 0 : n + 1{
        let fizz = i % 3 == 0
        let buzz = i % 5 == 0

        if fizz && buzz {
            @println("fizzbuzz")
        } else if fizz {
            @println("fizz")
        } else if buzz {
            @println("buzz")
        } else {
            @println(i)
        }
    }
}


fizzbuzz(20)



let i = 0
let ans = 0
while (i = i + 1) < 20 {
    if i % 3 == 0 {
        ans = ans + i
    }
}

@print(ans)




for a, b, c in [[1, 2, 3], [4, 5, 6], [7, 8, 9]] {
    @print(a, b, c)
}


let r, g, b, a = @rgba(1, 2, 3, 4)

@assert_eq(r, 1)
@assert_eq(g, 2)
@assert_eq(b, 3)
@assert_eq(a, 4)


let r, g, b = @rgba(1, 2, 3, 4)

@assert_eq(r, 1)
@assert_eq(g, 2)
@assert_eq(b, 3)


let r, = @rgba(1, 2, 3, 4)

@assert_eq(r, 1)
