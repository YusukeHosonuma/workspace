#![allow(dead_code)]

enum Person {
    Engineer, // like Unit構造体
    Scientist,
    Height(i32), // like タプル構造体
    Weight(i32),
    Info { name: String, height: i32 } // like 構造体
}

fn inspect(p: Person) {

    // 網羅しないとコンパイルエラー（Swiftの`switch`と同じ）
    match p {
        Person::Engineer  => println!("Is an engineer!"),
        Person::Scientist => println!("Is a scientist!"),
        Person::Height(i) => println!("Has a height of {}.", i),
        Person::Weight(i) => println!("Has a weight of {}.", i),
        Person::Info { name, height } => { // パターンマッチで分解（`Destructure`と表現する模様）
            println!("{} is {} tall!", name, height);
        },
    }
}

fn main() {
    let person  = Person::Height(18);
    let amira   = Person::Weight(10);
    // `.to_owned()`で、文字列参照（`&str`）から`String`に変換している
    let dave    = Person::Info { name: "Dave".to_owned(), height: 72 };
    let rebecca = Person::Scientist;
    let rohan   = Person::Engineer;

    inspect(person);
    inspect(amira);
    inspect(dave);
    inspect(rebecca);
    inspect(rohan);
}
