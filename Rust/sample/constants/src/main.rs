// 3.3 constants

// 文字列リテラルは &'static str として扱われる
static LANGUAGE: &'static str = "Rust";

// const は不変
const  THRESHOLD: i32 = 10;

fn is_big(n: i32) -> bool {
    n > THRESHOLD
}

fn main() {
    let n = 16;

    println!("This is {}", LANGUAGE);
    println!("The threshold is {}", THRESHOLD);
    println!("{} is {}", n, if is_big(n) { "big" } else { "small" });

    // 変更不可
    // THRESHOLD = 5;
}
