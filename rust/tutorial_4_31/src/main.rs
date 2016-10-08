// 4.31. サイズ不定型

struct Foo<T: ?Sized> { // 「Tは Sized かもしれない」と読む
    f: T,
}

fn main() {
    println!("Hello, world!");
}
