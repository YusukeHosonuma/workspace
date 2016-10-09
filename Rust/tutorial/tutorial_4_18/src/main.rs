// 4.18. ジェネリクス

enum Option2<T> {
    Some(T),
    None,
}

// 慣習としては第１ジェネリックパラメータは T であるべき。（だが、ルールではない）
enum Result2<T, E> {
    Ok(T),
    Err(E),
}

fn main() {

    // ジェネリック型
    {
        // <型>でジェネリクスの方を指定
        let x: Option<i32> = Some(5);

        // Error: 型が不一致であれば当然エラー
        // => error: mismatched types
        // let x: Option<f64> = Some(5);

        // 型があっていれば当然OK
        let x: Option<i32> = Some(5);
        let y: Option<f64> = Some(5.0f64);
    }

    // ジェネリック関数
    {
        // 1. <T> により、この関数は1つの型、T に対してジェネリックである
        // 2. x: T により「 x は T型 である」
        fn takes_anything<T>(x: T) {
        }

        // 複数の引数が同じジェネリック型を持つことも出来る
        fn takes_two_of_the_same_things<T>(x: T, y: T) {
        }

        // もちろん複数の型をとることも出来る
        fn takes_two_things<T, U>(x: T, y: U) {
        }
    }

    // ジェネリック構造体
    {
        // struct 内にジェネリックな値を保持することも出来る
        struct Point<T> {
            x: T,
            y: T,
        }
        let int_origin = Point { x: 0, y: 0 };
        let float_origin = Point { x: 0.0, y: 0.0 };

        // ジェネリックな struct に実装を追加する場合
        impl<T> Point<T> {
            fn swap(&mut self) {
                std::mem::swap(&mut self.x, &mut self.y);
            }
        }
    }
}
