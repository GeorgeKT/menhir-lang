#ret:11

fn add(x: $a, y: $a) -> $a: x + y
fn mul(x: $a, y: $a) -> $a: x * y
fn combine(x: $a, y: $a) -> $a: add(x, y) + mul(x, y)
fn main() -> int: combine(3, 2)