pub struct Test
{
    pub name: &'static str,
    pub ret: i64,
    pub debug: bool,
    pub code: &'static str,
}


pub const ALL_TESTS: [Test; 36] = [
    Test{
        name: "optional",
        ret: 42,
        debug: false,
        code: r#"
            unwrap_or(opt: ?int, default: int) -> int =
                opt || default

            main() -> int =
                unwrap_or(41, 1) + unwrap_or(nil, 1)
        "#
    },

    Test{
        name: "optional match",
        ret: 42,
        debug: false,
        code: r#"
            unwrap_or(opt: ?int, default: int) -> int {
                match opt:
                    ?value => value,
                    nil => default
            }

            main() -> int {
                unwrap_or(41, 1) + unwrap_or(nil, 1)
            }
        "#
    },

    Test{
        name: "optional if",
        ret: 42,
        debug: true,
        code: r#"
            foo() -> ?int = 77

            main() -> int {
                let a = foo();
                if a != nil: 42 else 0
            }
        "#
    },

    Test{
        name: "while",
        ret: 20,
        debug: false,
        code: r#"
            main() -> int {
                let x = 0;
                let y = 0;
                while x < 10 {
                    y = y + 2;
                    x = x + 1;
                }
                y
            }
        "#
    },

    Test{
        name: "assignment",
        ret: 77,
        debug: false,
        code: r#"
            main() -> int {
                let x = 7;
                x = x * 11;
                x
            }
        "#
    },

    Test{
        name: "member functions",
        ret: 11,
        debug: false,
        code: r#"
            type Foo {
                bar: int
            }

            Foo.add(self, num: int) -> int {
                self.bar + num
            }

            main() -> int =
                let f = Foo{7} in f.add(4)
        "#
    },

    Test{
        name: "char",
        ret: 8,
        debug: false,
        code: r#"
            foo(c: char) -> int =
                match c:
                    'b' => 5,
                    '\n' => 7,
                    _ => 3

            main() -> int = foo('b') + foo('c')
        "#
    },

    Test{
        name: "block",
        ret: 77,
        debug: false,
        code: r#"
            max(a: int, b: int) -> int =
                if a > b: a
                else b

            main() -> int {
                let x = max(10, 11);
                7 * x
            }
        "#
    },

    Test{
        name: "if test",
        ret: 100,
        debug: false,
        code: r#"
            max(a: int, b: int) -> int =
                if a > b: a
                else b

            main() -> int =
                max(100, 10)
        "#
    },

    Test{
        name: "string length",
        ret: 5,
        debug: false,
        code: r#"
            main() -> int =
                let x = "Hello" in x.len
        "#
    },

    Test{
        name: "number",
        ret: 5,
        debug: false,
        code: "main() -> int = 5"
    },

    Test{
        name: "unary sub",
        ret: -5,
        debug: false,
        code: "main() -> int = -5"
    },

    Test{
        name: "unary not",
        ret: 8,
        debug: false,
        code: "main() -> int = if !true: 7 else 8"
    },

    Test{
        name: "arithmethic operators",
        ret: 4 + 35 - 3 + 1,
        debug: false,
        code: "main() -> int = 4 + 5 * 7 - 9 / 3 + 5 % 4",
    },

    Test{
        name: "boolean operators",
        ret: 1,
        debug: false,
        code: "main() -> int = if 4 < 5 * 7 && 9 / 3 > 5 % 4: 1 else 0"
    },

    Test{
        name: "call",
        ret: 13,
        debug: false,
        code: r#"
            add(a: int, b: int) -> int = a + b
            main() -> int = add(6, 7)
        "#
    },

    Test{
        name: "match int",
        ret: 299,
        debug: false,
        code: r#"
            foo(a: int) -> int =
                match a:
                    0 => 100,
                    1 => 299,
                    _ => 0

            main() -> int = foo(1)
        "#
    },

    Test{
        name: "match bool",
        ret: 100,
        debug: false,
        code: r#"
            foo(a: bool) -> int =
                match a:
                    true => 100,
                    false => 299

            main() -> int = foo(true)
        "#
    },

    Test{
        name: "let",
        ret: 18,
        debug: false,
        code: r#"foo(a: int, b: int, c: int) -> int =
            let x = a * b, y = b * c in
                x + y

            main() -> int = foo(2, 3, 4)
        "#
    },

    Test{
        name: "array",
        ret: 5,
        debug: false,
        code: r#"
            main() -> int =
                let x = [2, 3, 4] in 5
        "#
    },

    Test{
        name: "array recursive iteration",
        ret: 9,
        debug: false,
        code: r#"
            sum(v: int[]) -> int =
                match v:
                    [] => 0,
                    [head | tail] => head + sum(tail)

            main() -> int =
                let x = [2, 3, 4] in sum(x)
        "#
    },

    Test{
        name: "array recursive iteration 2",
        ret: 49,
        debug: false,
        code: r#"
            sum(v: int[]) -> int =
                match v:
                    [] => 0,
                    [head | tail] => head + sum(tail)

            main() -> int =
                let y = 7, x = [y; 7] in sum(x)
        "#
    },

    Test{
        name: "generics",
        ret: 11,
        debug: false,
        code: r#"
            add(x: $a, y: $a) -> $a = x + y
            mul(x: $a, y: $a) -> $a = x * y
            combine(x: $a, y: $a) -> $a = add(x, y) + mul(x, y)
            main() -> int = combine(3, 2)
        "#
    },

    Test{
        name: "lambda",
        ret: 10,
        debug: false,
        code: r#"
            apply(x: int, fn: (int) -> int) -> int =
                fn(x)

            main() -> int = apply(5, @(x) -> x * 2)
        "#
    },

    Test{
        name: "lambda var",
        ret: 15,
        debug: false,
        code: r#"
            apply(x: int, fn: (int) -> int) -> int =
                fn(x)

            main() -> int =
                let triple = @(x) -> x * 3 in apply(5, triple)
        "#
    },

    Test{
        name: "func var",
        ret: 15,
        debug: false,
        code: r#"
            triple(x: int) -> int = x * 3

            main() -> int =
                let f = triple in f(5)
        "#
    },

    Test{
        name: "generic arrays",
        ret: 22,
        debug: false,
        code: r#"
            fold(v: $a[], accu: $b, fn: ($b, $a) -> $b) -> $b =
                match v:
                    [] => accu,
                    [hd | tail] => fold(tail, fn(accu, hd), fn)

            sum(v: int[]) -> int =
                fold(v, 0, @(s, el) -> s + el)

            main() -> int =
                sum([4, 5, 6, 7])
        "#
    },

    Test{
        name: "structs",
        ret: 50,
        debug: false,
        code: r#"
            type Vec2D = {x: int, y: int}

            dot(a: Vec2D, b: Vec2D) -> int = a.x * b.x + a.y * b.y

            main() -> int = dot(Vec2D{4, 5}, Vec2D{5, 6})
        "#
    },

    Test{
        name: "complex return types",
        ret: 20,
        debug: false,
        code: r#"
            type Vec2D = {x: int, y: int}

            add(a: Vec2D, b: Vec2D) -> Vec2D = Vec2D{a.x + b.x, a.y + b.y}

            main() -> int =
                let v = add(Vec2D{4, 5}, Vec2D{5, 6}) in v.x + v.y
        "#
    },

    Test{
        name: "anonymous structs",
        ret: 9,
        debug: false,
        code: r#"
            make_pair(a: int, b: int) -> {int, int} = ${a, b}

            main() -> int =
                let {left, right} = make_pair(4, 5) in left + right
        "#
    },

    Test{
        name: "sum types",
        ret: 6,
        debug: false,
        code: r#"
            type Option = Some{int} | None

            unwrap_or(opt: Option, default: int) -> int =
                match opt:
                    Some{i} => i,
                    None => default

            main() -> int =
                unwrap_or(Some{5}, 1) + unwrap_or(None, 1)
        "#
    },

    Test{
        name: "sum type return types",
        ret: 12,
        debug: false,
        code: r#"
            type PairOrSingle = Pair{int, int} | Single{int}

            pair_or_single(a: int, b: int) -> PairOrSingle =
                if a != b: Pair{a, b} else Single{a}

            value_of(v: PairOrSingle) -> int =
                match v:
                    Pair{a, b} => a + b,
                    Single{a} => a

            main() -> int =
                value_of(pair_or_single(4, 5)) + value_of(pair_or_single(3, 3))
        "#
    },

    Test{
        name: "enum types",
        ret: 34,
        debug: false,
        code: r#"
            type Animal = Dog | Cat | Bird | Fish

            number(a: Animal) -> int =
                match a:
                    Dog => 7,
                    Cat => 8,
                    Fish => 9,
                    _ => 10

            main() -> int =
                number(Dog) + number(Fish) + number(Cat) + number(Bird)
        "#
    },

    Test{
        name: "multiple sum types",
        ret: 53,
        debug: false,
        code: r#"
            type Animal = Dog | Cat | Bird | Fish
            type Option = Some{int} | None

            number(a: Animal) -> int =
                match a:
                    Dog => 7,
                    Cat => 8,
                    Bird => 22,
                    Fish => 9

            unwrap_or(opt: Option, def: int) -> int =
                match opt:
                    Some{i} => i,
                    None => def

            main() -> int =
                number(Dog) + number(Fish) + number(Cat) + number(Bird) + unwrap_or(Some{7}, 9)
        "#
    },

    Test{
        name: "generic sum types",
        ret: 16,
        debug: false,
        code: r#"
            type Option = Some{$a} | None

            unwrap_or(opt: Option<$a>, def: $a) -> $a =
                match opt:
                    Some{i} => i,
                    None => def

            main() -> int =
                unwrap_or(None, 9) + unwrap_or(Some{7}, 9)
        "#
    },

    Test{
        name: "generic struct types",
        ret: 18,
        debug: false,
        code: r#"
            type Pair = {first: $a, second: $b}

            add(p: Pair<$a, $a>) -> $a  =
                p.first + p.second

            main() -> int = add(Pair{4, 14})
        "#
    }
];
