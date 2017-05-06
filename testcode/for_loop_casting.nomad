#ret:15
fn main() -> int:
    let array = [1, 2, 3, 4, 5]
    var sum = 0u
    for a in array:
        sum = sum + a as uint
    sum as int
