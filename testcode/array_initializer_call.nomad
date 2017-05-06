#ret:15
fn baz() -> int:
    5

fn main() -> int:
    let array = [1, 2, 3, 4, baz()]
    var sum = 0
    for a in array:
        sum = sum + a
    sum
