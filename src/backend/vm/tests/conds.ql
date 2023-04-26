// conditional tests

let a = 1
let b = 20


let ans = if (a > b) {
    "true"
} else {
    "false"
}

if ans != "false" {
    @err("ans is not false")
}


let ans2 = 0
for i in 1:10 {
    if (i % 2 == 0) {
        ans2 = ans2 + i
    }
}


if ans2 != 20 {
    @err("ans2 is not 20")
}

let bruh = if 1 < 0 {
    "bruh"
}

if bruh != none {
    @err("bruh is not none")
}

let bruh2 =  if 1 > 0 {

} else {
    "bruh"
}


if bruh2 != none {
    @err("bruh2 is not bruh")
}
