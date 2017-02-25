#ret:6
enum Option:
    Some{v: int}
    None

fn unwrap_or(opt: Option, default: int) -> int:
    match opt:
        Some{i} => i
        None => default

fn main() -> int:
    unwrap_or(Some{5}, 1) + unwrap_or(None, 1)