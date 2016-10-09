fn main() {

    let x = 5;

    // 通常のコントロールフローとしての if else-if else
    if x == 5 {
        println!("x is 5!");
    } else if x == 6 {
        println!("x is 6!");
    } else {
        println!("x is not 5 or 6 :(")
    }

    let x = 5;

    // 式として評価できる
    let y = if x == 5 {
        10
    } else {
        15
    };

    // 1行で書いたもの（これが推奨されるらしい）
    let y = if x == 5 { 10 } else { 15 };
}
