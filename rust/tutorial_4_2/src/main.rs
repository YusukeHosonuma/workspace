fn main() {
    print_number(5);
    print_sum(5, 6);
    let x = add_one(5);
    println!("add_one(5) is: {}", x);

    // ダイバージング関数は任意の型として使える（まだ良くわかってない）
    // let x: i32 = diverges();
    // let x: String = diverges();

    fn plus_one(i: i32) -> i32 {
        i + 1
    }

    // 関数ポインタ： \i32 -> i32
    let f: fn(i32) -> i32 = plus_one;
    let f = plus_one; // 推論できる

    let six = f(5);
    println!("six is: {}", six);
}

fn print_number(x: i32) {
    println!("x is: {}", x)
}

// 関数の引数の型は省略できない（設計上の決断）
fn print_sum(x: i32, y: i32) {
    println!("sum is: {}", x + y);
}

fn add_one(x: i32) -> i32 {
    // return は不要（使っても良いけれど、良くないスタイルと言われる）
    x + 1 // ❗ ; をつける（文だと値を返さないので）とコンパイルエラー
}

fn foo(x: i32) -> i32 {
    return x; // 早期Return
    x + 1
}

// ダイバージング関数（リターンしない関数のこと）
fn diverges() -> ! {
    // panic!は現在のスレッドを与えられたメッセージとともにクラッシュさせる
    // RUST_BACKTRACE=1 cargo run でスタックトレースが見られる
    panic!("This function never returns!");
}
