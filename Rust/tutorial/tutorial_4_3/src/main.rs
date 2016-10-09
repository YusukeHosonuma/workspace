fn main() {

    // boolean
    let x = true;
    let y: bool = false;

    // char（ユニコードのスカラ値＝4byte）
    let x = 'x';
    let two_hearts = '💕';

    // 数値型
    {
        // 数値型（デフォルトでは以下の型が利用される）
        let x = 42; // x: i32
        let y = 1.0; // y: f64

        // 符号あり/符号なし
        let x: i8 = 1; // integer（符号あり）
        let x: u8 = 1; // unsigned（符号なし）

        // 指定できるビット長は、8, 16, 32, 64
        let x: i8 = 1;
        let x: i16 = 1;
        let x: i32 = 1;
        let x: i64 = 1;

        // 可変長型（マシンのポインタサイズに依存する型）
        let x: isize;
        let x: usize;

        // 浮動小数点型（IEEE-754）
        let x: f32;
        let x: f64;
    }

    // 配列（デフォルトでイミュータブル）
    {
        // [T; N]という型を持つ： T=要素、N=配列の長さ（コンパイル時の定数）
        let a = [1, 2, 3];     // [i32; 3]
        let mut m = [1, 2, 3]; // [i32; 3]

        // すべて同じ値で初期化する省略表現（0で埋める）
        let a = [0; 20]; // [i32; 20]

        // 要素数を取得：.len()
        let a = [1, 2, 3];
        println!("a has {} elements", a.len());

        // 要素へアクセス
        let names = ["Graydon", "Brian", "Niko"]; // names: [&str; 3]
        println!("The second name is: {}", names[1]);

        // Runtime error: 配列の範囲外アクセスは実行時にチェックされる（releaseビルドだと無視されるみたい）
        // => thread 'main' panicked at 'index out of bounds: the len is 3 but the index is 3',
        // println!("The second name is: {}", names[3]);
    }

    // スライス：他のデータ構造への参照（View）
    // 型：&[T]
    {
        let a = [0, 1, 2, 3, 4];
        let complete = &a[..]; // 全体を持つスライス
        let middle = &a[1..4]; // 1, 2, 3のみを要素に持つスライス
    }

    // タプル
    {
        let x = (1, "hello");
        let x: (i32, &str) = (1, "hello");

        // アリティ（引数とかの数）が一緒であれば他のタプルに割当可能
        let mut x = (1, 2);
        let y = (2, 3);
        x = y;

        // 分配束縛let（パターンマッチ）
        let (x, y, z) = (1, 2, 3);
        println!("x is {}", x);

        // 1要素のタプルも記述できる
        (0,); // 1要素のタプル
        (0);  // 丸括弧に囲まれたゼロ

        // タプル内の要素へのアクセス（.N）
        let tuple = (1, 2, 3);
        let x = tuple.0;
        let y = tuple.1;
        let z = tuple.2;
        println!("x is {}", x);
    }

    // 関数
    fn foo(x: i32) -> i32 { x }
    let x: fn(i32) -> i32 = foo;
}
