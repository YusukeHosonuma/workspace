use std::fmt;

// アトリビュートで fmt::Debug を自動導出
#[derive(Debug)]
struct MinMax(i64, i64);

// fmt::Display を実装
impl fmt::Display for MinMax {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}, {})", self.0, self.1)
    }
}

#[derive(Debug)]
struct Point2 {
    x: f64,
    y: f64,
}

impl fmt::Display for Point2 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "x: {}, y: {}", self.x, self.y)
    }
}

#[derive(Debug)]
struct Complex {
    real: f64,
    imag: f64,
}

impl fmt::Display for Complex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} + {}i", self.real, self.imag)
    }
}

fn main() {

    let minmax = MinMax(0, 14);

    // `{}`で、fmt::Display の実装が呼ばれ、
    // `{:?}`で、fmt::Debug の実装（今回は`#[derive(Debug)]`による導出）が呼ばれる。

    println!("Compare structures:");
    println!("Display: {}", minmax); // => Display: (0, 14)
    println!("Debug: {:?}", minmax); // => Debug: MinMax(0, 14)

    let big_range = MinMax(-300, 300);
    let small_range = MinMax(-3, 3);

    // 名前付きのプレースホルダ（文字列インターポレーション）が利用できる。
    // （たぶん）マクロなのでパフォーマンスは全く落ちない。
    println!("The big range is {big:?} and the small is {small}",
             small = small_range,
             big = big_range);
    // => The big range is MinMax(-300, 300) and the small is (-3, 3)

    let point = Point2 { x: 3.3, y: 7.2 };

    println!("Compare points:");
    println!("Display: {}", point); // => Display: x: 3.3, y: 7.2
    println!("Debug: {:?}", point); // => Debug: Point2 { x: 3.3, y: 7.2 }

    let complex = Complex { real: 3.3, imag: 7.2 };

    println!("Compare complex:");
    println!("Display: {}", complex);
    println!("Debug: {:?}", complex);
}
