#ret:15
interface Sum:
    fn sum(self) -> int

struct Point:
    x: int
    y: int

fn Point.sum(self) -> int:
    self.x + self.y

fn foo(x: $Sum) -> int:
    x.sum()

fn main() -> int:
    let p = Point{7, 8};
    foo(p)