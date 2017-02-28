#ret:16
enum Option:
    Some{value: $a}
    None

fn unwrap_or(opt: Option<$a>, def: $a) -> $a:
    match opt:
        Some{i} => i
        None => def

fn main() -> int:
    unwrap_or(None, 9) + unwrap_or(Some{7}, 9)