// 4.32. 演算子とオーバーロード


fn main() {

    // 演算子とオーバーロード
    {
        use std::ops::Add;

        #[derive(Debug)]
        struct Point {
            x: i32,
            y: i32,
        }

        impl Add for Point {
            type Output = Point;
            fn add(self, other: Point) -> Point {
                Point {
                    x: self.x + other.x,
                    y: self.y + other.y,
                }
            }
        }

        pub trait AddX<RHS = Self> {
            type Output;
            fn add(self, rhs: RHS) -> Self::Output;
        }

        impl AddX<i32> for Point {
            type Output = f64;
            fn add(self, rhs: i32) -> f64 {
                (self.x + self.y + rhs) as f64
            }
        }

        let p1 = Point { x: 1, y: 0 };
        let p2 = Point { x: 2, y: 3 };
        let p3 = p1 + p2;
        println!("{:?}", p3);

        // let p: Point = Point { x: 1, y: 2 };
        // let x: f64 = p + 2i32;
    }

    // オペレータトレイトをジェネリック構造体で使う
    {
        use std::ops::Mul;

        // 以前は f64 の型に特化していた。

        trait HasArea<T> {
            fn area(&self) -> T;
        }

        struct Square<T> {
            x: T,
            y: T,
            side: T,
        }

        impl<T> HasArea<T> for Square<T>
                where T: Mul<Output=T> + Copy { // 所有権がムーブしないようにCopyが必要
            fn area(&self) -> T {
                self.side * self.side // Mulを実装しているので使える
            }
        }

        let s = Square {
            x: 0.0f64,
            y: 0.0f64,
            side: 12.0f64,
        };
        println!("Area of s: {}", s.area());
    }
}
