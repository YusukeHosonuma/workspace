// 4.12. 列挙型

// いくつかのヴァリアントのうちからどれか一つを取るデータ型（直和型）
enum Message {
    Quit,
    ChangeColor(i32, i32, i32), // 関連するデータも持てる
    Move { x: i32, y: i32 },
    Write(String),
}

fn main() {

    // :: 構文でヴァリアントを使う
    let x: Message = Message::Move { x: 3, y: 4 };

    enum BoardGameTurn {
        Move { squares: i32 },
        Pass,
    }

    // 列挙型名でスコープ化されているので、Move という名前がかぶっても大丈夫
    let y: BoardGameTurn = BoardGameTurn::Move { squares: 1 };

    // データは、それ自信がどの型なのかを示す「タグ：を持っていて、
    // コンパイラはこの情報を用いて、列挙型内のデータへ安全にアクセスすることを強制する

    fn process_color_change(msg: Message) {
        // Error: どれか1つの値であるとみなすことは出来ない
        // => error: refutable pattern in local binding: `Quit` not covered
        // let Message::ChangeColor(r, g, b) = msg;
    }

    // 関数としてのコンストラクタ
    let m = Message::Write("Hello, world".to_string());

    // ↑と同じ
    fn foo(x: String) -> Message {
        Message::Write(x)
    }
    let x = foo("Hello, world".to_string());

    // Stringのベクタからの変換
    let v = vec!["Hello".to_string(), "World".to_string()];
    let v1: Vec<Message> = v.into_iter().map(Message::Write).collect();
}
