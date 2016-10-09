// 4.15. メソッド構文

struct Circle {
    x: f64,
    y: f64,
    radius: f64,
}

impl Circle {
    // &selfでレシーバ（c）を受け取ってる
    fn area(&self) -> f64 {
        std::f64::consts::PI * (self.radius * self.radius)
    }

    fn grow(&self, increment: f64) -> Circle {
        Circle { x: self.x, y: self.y, radius: self.radius + increment }
    }
}

impl Circle {
    // 借用
    fn reference(&self) {
        println!("taking self by reference!");
    }
    // Mutableな借用
    fn mutable_reference(&mut self) {
        println!("taking self by mutable reference!");
    }
    // 所有権をもらう
    fn takes_ownership(self) {
        println!("taking ownership of self!");
    }
}

impl Circle {

    // 関連関数（他の言語で言う「静的メソッド」）
    fn new(x: f64, y: f64, radius: f64) -> Circle {
        Circle {
            x: x,
            y: y,
            radius: radius,
        }
    }
}

// Builder パターン
struct CircleBuilder {
    x: f64,
    y: f64,
    radius: f64,
}

impl CircleBuilder {
    fn new() -> CircleBuilder {
        CircleBuilder { x: 0.0, y: 0.0, radius: 1.0 }
    }

    fn x(&mut self, coordinate: f64) -> &mut CircleBuilder {
        self.x = coordinate;
        self
    }

    fn y(&mut self, coordinate: f64) -> &mut CircleBuilder {
        self.y = coordinate;
        self
    }

    fn radius(&mut self, radius: f64) -> &mut CircleBuilder {
        self.radius = radius;
        self
    }

    fn finalize(&self) -> Circle {
        Circle { x: self.x, y: self.y, radius: self.radius }
    }
}

fn main() {
    let c = Circle { x: 0.0, y: 0.0, radius: 2.0 };
    println!("{}", c.area()); // メソッドのように呼び出せる

    // メソッドチェーン（.grow が Circle を返すので連鎖できる）
    let d = c.grow(2.0).area();
    println!("{}", d);

    let c = Circle::new(0.0, 0.0, 2.0);

    // Builderパターンによる struct の生成
    let c = CircleBuilder::new()
                .x(1.0)
                .y(2.0)
                .radius(2.0)
                .finalize();
    println!("area: {}", c.area());
    println!("x: {}", c.x);
    println!("y: {}", c.y);
}
