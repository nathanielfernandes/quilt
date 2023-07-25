// arithmetic tests

let x = 5
x = x + 1
x = x * 20 - (5 / 2) ** (2)
x / 2

if (x != 116) {
    @err("x should be 116, but is: " .. x)
}