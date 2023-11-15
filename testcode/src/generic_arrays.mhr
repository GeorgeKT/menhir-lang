#ret:22

fn fold(v: $a[], accu: $b, f: fn($b, $a) -> $b) -> $b:
    match v:
        [] => accu
        [hd | tail] => fold(tail, f(accu, hd), f)

fn sum(v: int[]) -> int:
    fold(v, 0, fn(s, el) -> s + el)

fn main() -> int:
    sum([4, 5, 6, 7])