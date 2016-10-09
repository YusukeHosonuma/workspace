
// トレイトの宣言（型シグネチャだけ定義）
trait HasArea {
    fn area(&self) -> f64;
}

struct Circle {
    x: f64,
    y: f64,
    radius: f64,
}

// トレイトを実装
// `impl {トレイト} for 型 {`
impl HasArea for Circle {
    fn area(&self) -> f64 {
        std::f64::consts::PI * (self.radius * self.radius)
    }
}

struct Square {
    x: f64,
    y: f64,
    side: f64,
}

impl HasArea for Square {
    fn area(&self) -> f64 {
        self.side * self.side
    }
}

fn print_area<T: HasArea>(shape: T) {
    println!("This shape has an area of {}", shape.area());
}

fn main() {

    // トレイト
    {
        let c = Circle {
            x: 0.0f64,
            y: 0.0f64,
            radius: 1.0f64,
        };

        let s = Square {
            x: 0.0f64,
            y: 0.0f64,
            side: 1.0f64,
        };

        // Circle、Square ともに、 HasArea トレイトを実装しているので呼び出せる
        print_area(c);
        print_area(s);

        // Error: 型があっていなければ当然エラー
        // print_area(5);
    }

    // トレイト with ジェネリックス構造体
    {
        struct Rectangle<T> {
            x: T,
            y: T,
            width: T,
            height: T,
        }

        // 両辺の型は、 core::cmp::PartialEq を実装しなければならない
        impl<T: PartialEq> Rectangle<T> {
            fn is_square(&self) -> bool {
                self.width == self.height
            }
        }

        let mut r = Rectangle {
            x: 0,
            y: 0,
            width: 47,
            height: 47,
        };

        assert!(r.is_square());

        r.height = 42;
        assert!(!r.is_square());
    }

    // トレイト実装のルール
    //
    // 以下の2つの制限がある。
    //
    // 1. トレイトを use しない限りは、そのトレイトが定義されたメソッドは利用できない（コンパイルエラー）
    // 2. トレイトの定義と、impl の対象となる型は、両方共自分たちで実装しなければならない。
    //    つまり、Haskellのように既存の型に対して、拡張とかはできないということ（たぶん）
    //
    // 2.について、例えば Rust で提供されている ToString トレイトを実装しようとしても出来ない。
    {
        // 実装上はこういう意味のないことも出来る。
        // ソース上で、HasArea トレイト を use しない限りは影響しない。
        impl HasArea for i32 {
            fn area(&self) -> f64 {
                println!("this is silly");
                *self as f64
            }
        }

        // こうやって Writeトレイトを use しない限り、write() は呼べない
        use std::io::Write;

        // let mut f = std::fs::File::open("foo.txt").expect("Couldn't open foo.txt");
        // let buf = b"whatever";
        // let result = f.write(buf);
    }

    // 複数のトレイト境界
    {
        use std::fmt::Debug;

        // 1つ以上の境界を与えたい場合、 + を利用する
        fn foo<T: Clone + Debug>(x: T) {
            x.clone();
            println!("{:?}", x);
        }
    }

    // Where 節
    {
        use std::fmt::Debug;

        // こうやって書くと、型パラメータと引数が遠くなって読みづらくなる
        fn foo<T: Clone, K: Clone + Debug>(x: T, y: K) {
            x.clone();
            y.clone();
            println!("{:?}", y);
        }

        // where を使うことで以下のように書ける
        fn bar<T, K>(x: T, y: K) where T: Clone, K: Clone + Debug {
            x.clone();
            y.clone();
            println!("{:?}", y);
        }

        foo("Hello", "world");
        bar("Hello", "world");

        // 空白や改行も入れられる
        fn bar2<T, K>(x: T, y: K)
            where T: Clone,
                  K: Clone + Debug {
        }

        trait ConvertTo<Output> {
            fn convert(&self) -> Output;
        }
        impl ConvertTo<i64> for i32 {
            fn convert(&self) -> i64 { *self as i64 }
        }
        // T == i32 の時に呼び出せる
        fn normal<T: ConvertTo<i64>>(x: &T) -> i64 {
            x.convert()
        }
        // T == i64 の時に呼び出せる
        fn inverse<T>() -> T
                where i32: ConvertTo<T> {
            42.convert()
        }
    }

    // デフォルトメソッド
    {
        trait Foo {
            fn is_valid(&self) -> bool;

            // 以下のようにデフォルト実装（is_valid の反対）を用意できる
            fn is_invalid(&self) -> bool { !self.is_valid() }
        }

        // デフォルト実装をそのまま使った場合
        struct UseDefault;
        impl Foo for UseDefault {
            fn is_valid(&self) -> bool {
                println!("Called UseDefault.is_valid.");
                true
            }
        }

        // デフォルト実装をオーバーライド
        struct OverrideDefault;
        impl Foo for OverrideDefault {
            fn is_valid(&self) -> bool {
                println!("Called OverrideDefault.is_valid.");
                true
            }
            fn is_invalid(&self) -> bool {
                println!("Called OverrideDefault.is_invalid!");
                true
            }
        }

        let default = UseDefault;
        assert!(!default.is_invalid());

        let over = OverrideDefault;
        assert!(over.is_invalid());
    }

    // 継承
    {
        trait Foo {
            fn foo(&self);
        }

        // Foo を継承したトレイト
        trait FooBar : Foo {
            fn foobar(&self);
        }

        struct Baz;

        // 以下を実装していないとコンパイルエラーとなる。
        // => error: the trait bound `main::Baz: main::Foo` is not satisfied
        impl Foo for Baz {
            fn foo(&self) { println!("foo"); }
        }
        // FooBar を実装
        impl FooBar for Baz {
            fn foobar(&self) { println!("foobar"); }
        }
    }

    // Derive
    //
    // アトリビュートという、自動的にトレイトを実装する仕組み
    // （Haskell の deriving と同じ）
    //
    // 以下がサポートされてる。
    // - Clone
    // - Copy
    // - Debug
    // - Default
    // - Eq
    // - Hash
    // - Ord
    // - PartialEq
    // - PartialOrd
    //
    {
        #[derive(Debug)]
        struct Foo;
        println!("{:?}", Foo);
    }
}
