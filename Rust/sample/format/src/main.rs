use std::fmt;

struct Point {
    x: i32,
    y: i32,
}

impl fmt::Display for Point {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
       write!(f, "({}, {})", self.x, self.y)
    }
}

impl fmt::Debug for Point {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Point(x: {}, y: {})", self.x, self.y)
    }
}

fn main() {

    // `println!`マクロで標準出力
    println!("Hello world!");
    println!("Hello {}!", "world");

    // 文字列が欲しい場合は`format!`マクロを使う
    let s = format!("Hello, {} !!", "Rust"); // s = "Hello, Rust !!"
    assert_eq!(s, "Hello, Rust !!");

    // {} がプレースホルダ（引数が順に埋められる）
    // {n} で n番目（0始まり）の引数で置き換える
    // {name} で `name="value"` と指定した引数で置き換えられる
    println!("{} | {} | {1} | {three} | {four}",
             "One",
             "Two",
             three="three",
             four=4);
    // => One | Two | Two | three | 4

    // `:`の後にフォーマット条件を指定
    // <n が n桁で左寄せ
    // >n が n桁で右寄せ
    // ^n が n桁で中央寄せ
    // >0n または <0n で「0埋め」
    println!("{0: <8} | {1: >8} | {2: ^8} | {3: <08} | {4: >08} | {hundred: >08}",
             "Left",
             "Right",
             "Center",
             42,
             999,
             hundred=100);
    // => Left     |    Right |  Center  | 42000000 | 00000999 | 00000100

    // バイナリ表現
    println!("{:b}", 1234); // => 10011010010

    // 8進数
    println!("{:o}", 1234); // => 2322

    // 16進数（`X`だと大文字で出力）
    println!("{:x}", 1234); // => 4d2
    println!("{:X}", 1234); // => 4D2

    // 浮動小数点数の指数表現
    println!("{:e}", 12.34); // => 1.234e1
    println!("{:E}", 12.34); // => 1.234E1

    // {} では fmt::Display の実装が使われ、
    // {:?} では fmt::Debug の実装が使われる

    let p = Point { x: 1, y: 2 };
    println!("Point(fmt::Display) : {}",   p);
    // => Point(fmt::Display) : (1, 2)

    println!("Point(fmt::Debug)   : {:?} ", p);
    // => Point(fmt::Debug)   : Point(x: 1, y: 2)
}
