#ret:15
fn apply(x: int, f: fn(int) -> int) -> int:
    f(x)

fn main() -> int:
    let triple = fn(x) -> x * 3 in apply(5, triple)