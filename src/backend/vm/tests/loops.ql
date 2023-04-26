fn fizzbuzz(n) {
    for i in 0 : n + 1{
        let fizz = i % 3 == 0
        let buzz = i % 5 == 0

        if fizz && buzz {
            @print("fizzbuzz")
        } else if fizz {
            @print("fizz")
        } else if buzz {
            @print("buzz")
        } else {
            @print(i)
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