#![allow(dead_code)]

// 0始まりのi32型で表現される
enum Number {
    Zero, // 0
    One,  // 1
    Two,  // 2
}

// 自分で値を設定することも出来る
enum Color {
    Red   = 0xff0000,
    Green = 0x00ff00,
    Blue  = 0x0000ff,
}

fn main() {

    use Number::*;
    use Color::*;

    println!("zero is {}", Zero as i32);
    println!("one is {}", One as i32);

    // `0`: 0埋め
    // `6`: 6桁
    // `x`: 16進数リテラルとして出力
    println!("roses are #{:06x}", Red as i32);
    println!("violets are #{:06x}", Blue as i32);
}
