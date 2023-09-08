// conditional tests

let a = 1
let b = 20


let ans = if (a > b) {
    "true"
} else {
    "false"
}

@assert_eq(ans, "false")

let ans2 = 0
for i in 1:10 {
    if (i % 2 == 0) {
        ans2 = ans2 + i
    }
}

@assert_eq(ans2, 20)

let bruh = if 1 < 0 {
    "bruh"
}

@assert_eq(bruh, none)

let bruh2 =  if 1 > 0 {

} else {
    "bruh"
}

@assert_eq(bruh2, none)
