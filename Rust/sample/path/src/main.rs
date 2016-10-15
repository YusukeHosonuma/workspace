use std::path::Path;

fn main() {

    let path = Path::new(".");

    // `display()`すると println! で表示できるようになる
    let display = path.display();
    println!("path: {}", display);

    // OSに準拠したセパレータでパスを繋げる
    let new_path = path.join("a").join("b");

    // `Path`は UTF-8シーケンスではなく、`Vec<u8>`のバイトシーケンスで表現されているので失敗することがある
    match new_path.to_str() {
        None => panic!("new path is not a valid UTF-8 sequence"),
        Some(s) => println!("new path is {}", s),
    }
}
