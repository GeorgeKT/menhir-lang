#ret:71
interface Product:
    fn product(self) -> int

interface Sum:
    fn sum(self) -> int

struct Point:
    x: int
    y: int

fn Point.sum(self) -> int:
    self.x + self.y

fn Point.product(self) -> int:
    self.x * self.y

fn foo(x: $(Product + Sum)) -> int:
    x.product() + x.sum()

fn main() -> int:
    let p = Point{7, 8}
    foo(p)