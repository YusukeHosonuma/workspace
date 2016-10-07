// 4.28. `type`エイリアス

use std::result;

enum ConcreteError {
    Foo,
    Bar,
}

type Result<T> = result::Result<T, ConcreteError>; // 特化したバージョンのエイリアスが定義できる

type Name = String;
type Num = i32;

fn main() {

    let x: Name = "Hello".to_string();

    let x: i32 = 5;
    let y: Num = 5;
    if x == y {
        println!("x == y");
    }
}
