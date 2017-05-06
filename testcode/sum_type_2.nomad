#ret:12
enum PairOrSingle:
    Pair{first: int, second: int}
    Single{first: int}

fn pair_or_single(a: int, b: int) -> PairOrSingle:
    if a != b:
        Pair{a, b}
    else
        Single{a}

fn value_of(v: PairOrSingle) -> int:
    match v:
        Pair{a, b} => a + b
        Single{a} => a

fn main() -> int:
    value_of(pair_or_single(4, 5)) + value_of(pair_or_single(3, 3))