extern crate rand; // これでクレートを使うと宣言している。

use std::io;
use std::cmp::Ordering;
use rand::Rng;

// Note:
//
// Rustがデフォルトで読み込むAPI群は「プレリュード」と呼ぶ。（Haskellと同じ）
//
// 乱数生成機能は標準ライブラリには含まれていないため「randクレート」を使う。
// 「クレート」はRustコードのパッケージ。
//
// 実行可能なものは「バイナリクレート」で、ライブラリは「ライブラリクレート」と呼ぶ。
//
// Cargoはバージョン記述の標準「セマンティックバージョン」を理解する。
//
// rand="0.3.0"  => "^0.3.0"の略記で「0.3.0と互換性のあるもの」
// rand="=0.3.0" => 正確にバージョン0.3.0
// rand="*"      => 最新バージョン
//
// carthage と同じように .lock ファイルを生成するため、
// 明示的にアップデードしない限りは、そのバージョンを利用し続ける。
//
// アップデートしたい場合は、`cargo update` を実行する。
//
// その場合、0.3.0より大きくて、0.4.0より小さいバージョンを探しに行くので、
// 0.4.xより大きいバージョンを使いたい場合は、Cargo.toml を修正する必要がある。
//

fn main() { // 返り値が指定されていない場合は、空のタプル()として扱われる

    // これはマクロ
    println!("Guess the number!");

    // rand::thread_rng() で、現在のスレッドにローカルな乱数生成器のコピーを取得。
    // gen_range() メソッドで、1から100までの間の数を生成する。
    let secret_number = rand::thread_rng().gen_range(1, 101);

    loop {
        println!("Please input your guess.");

        // 変数束縛。
        //
        // 可変な束縛にするために`mut`を指定している。
        // 代入の左辺には名前ではなく「パターン」を受け取っている。
        //
        // Stringは伸長可能で UTF-8 エンコードされたテキスト片。
        // 「関連関数」（スタティックメソッド）を呼ぶために、`::xxx()` としている。
        //
        // `new()`はただの関数。
        let mut guess = String::new();

        // io の関連関数 `stdin()` を呼び出している。
        // useしていなかった場合は `std::io::stdin()` と書く必要がある。
        // この関数は、ターミナルの標準入力へのハンドルを返す。
        //
        // `.read_line(&mut guess)` でハンドルを使ってユーザからの入力を取得。
        // &mut guess としているので、ミュータブルな参照を渡している。
        // 参照もデフォルトではイミュータブルなので `&mut` をつける必要がある。
        //
        // ユーザの入力が`&mut String`に入るが、`read_line()`は`io::Result`を値として返す。
        // io::Result の `expect()`メソッドは、失敗であった場合にメッセージと共に`panic!`させる。
        io::stdin().read_line(&mut guess)
            .expect("Failed to read line");

        // シャドーイングにより同じ名前「guess」を宣言できている。
        // guess_str、guess などと別々の名前をつけなくても良くなっている。
        //
        // trim() メソッドは文字列の最初と最後にある空白（改行も）を取り除く。
        // u32 と型を指定していることで、u32用の parse() が選ばれる。
        let guess: u32 = match guess.trim().parse() {
            Ok(num) => num,
            Err(_) => continue,
        };

        // {} がプレースホルダ
        println!("You guessed: {}", guess);

        match guess.cmp(&secret_number) {
            Ordering::Less    => println!("Too small!"),
            Ordering::Greater => println!("Too big!"),
            Ordering::Equal   => {
                println!("You win!");
                break;
            }
        }
    }
}
