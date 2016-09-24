// 4.13. マッチ

fn main() {

    let x = 5;

    match x {
        1 => println!("one"),
        2 => println!("two"),
        3 => println!("three"),
        4 => println!("four"),
        5 => println!("five"),
        // 網羅性が検査されるので、以下がなければコンパイルエラーとなる
        // => error: non-exhaustive patterns: `_` not covered
        _ => println!("something else"),
    }

    // match は 式でもある
    let x = 5;

    let number = match x {
        1 => "one",
        2 => "two",
        3 => "three",
        4 => "four",
        5 => "five",
        _ => "something else",
    };

    // 列挙型に対するマッチ
    enum Message {
        Quit,
        ChangeColor(i32, i32, i32),
        Move { x: i32, y: i32 },
        Write(String),
    }

    fn quit() {}
    fn change_color(r: i32, g: i32, b: i32) {}
    fn move_cursor(x: i32, y: i32) {}

    fn process_message(msg: Message) {

        // if は列挙型のバリアントには利用できない（`if let` ならOK）
        // `if let` は match の短縮形とみなせる。
        match msg {
            Message::Quit => quit(),
            Message::ChangeColor(r, g, b) => change_color(r, g, b),
            Message::Move { x: x, y: y } => move_cursor(x, y),
            Message::Write(s) => println!("{}", s),
        }
    }
}
