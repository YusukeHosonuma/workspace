// 4.17. 文字列

// Rustの文字列
//
// 「文字列」は、UTF-8のバイトストリームとしてエンコードされたスカラー値のシーケンス
// null終端ではなく、nullバイトを含めることが可能
//
// Rustは主に2種類の文字列型を持っている。
//
// 1. &str
// => 「文字列のスライス」。固定サイズを持ち、変更は不可。UTF-8バイトシーケンスへの参照。
//
// 2. String
// => ヒープにアロケートされる文字列形式。変更可能。文字列スライスのメソッド `to_string` を用いて変換することが多い
//

fn main() {

    // &'static str
    //
    // 静的にアロケートされた文字列のスライスで、コンパイルされたプログラム中に保存される。
    // 文字列スライスを引数として期待している関数には、文字列リテラルを渡せる。
    {
        let greeting = "Hello there."; // greeting: &'static str

        // 複数行の文字列リテラル（改行と空白を含む）
        let s = "foo
            bar";
        assert_eq!("foo\n            bar", s);

        // \で空白と改行を削れる
        let s = "foo\
            bar";
        assert_eq!("foobar", s);
    }

    // String
    {
        // to_string() で String に変換
        let mut s = "Hello".to_string(); // mut s: String
        println!("{}", s);

        // 連結できる
        s.push_str(", world.");
        println!("{}", s);

        // & によって、String は &str に型強制される
        fn takes_slice(slice: &str) {
            println!("Got: {}", slice);
        }
        let s = "Hello".to_string();
        takes_slice(&s);

        // （&str ではなく）&str のトレイトを期待している関数では自動的に変換されない
        {
            use std::net::TcpStream;

            // 引数として &str を渡す
            TcpStream::connect("192.168.0.1:3000");

            // addr_string を `&*` で、&str に変換して渡す（自動的に変換はされない）
            let addr_string = "192.168.0.1:3000".to_string();
            TcpStream::connect(&*addr_string);
        }
    }

    // インデクシング
    {
        // 文字列はインデクシングをサポートしていない
        // 1文字が複数バイトであることが可能なため、n番目の文字列を探すのは高コストな処理
        let s = "hello";

        // Error:
        // => the trait bound `str: std::ops::Index<_>` is not satisfied
        // println!("The first letter of s is {}", s[0]);

        let hachiko = "忠犬ハチ公";

        // バイト列として走査：
        // => 229, 191, 160, 231, 138, 172, 227, 131, 143, 227, 131, 129, 229, 133, 172,
        for b in hachiko.as_bytes() {
            print!("{}, ", b);
        }
        println!("");

        // 文字列として走査：
        // => 忠, 犬, ハ, チ, 公,
        for c in hachiko.chars() {
            print!("{}, ", c);
        }
        println!("");

        // chars()で変換してから、`nth(n)`でインデクシングのようなことが出来る
        let dog = hachiko.chars().nth(1); // like `hachiko[1]`
    }

    // スライシング
    {
        // スライス構文を用いて、文字列スライスが取得できる
        let dog = "hachiko";
        let hachi = &dog[0..5];
        println!("hachi is: {}", hachi);

        // バイトのオフセットなので、正しく文字に分割できない場合は実行時エラーとなる。
        let dog = "忠犬ハチ公";
        let hachi = &dog[0..3];
        // Runtime error:
        // => thread 'main' panicked at 'index 0 and/or 2 in `忠犬ハチ公` do not lie on character boundary'
        // let hachi = &dog[0..2];
        println!("hachi is: {}", hachi);
    }

    // 結合
    {
        // String の末尾に `+` で &str を結合できる
        let hello = "Hello ".to_string();
        let world = "world!";
        let hello_world = hello + world;

        // 2つの String を結合するときは、`&` が必要になる。
        let hello = "Hello ".to_string();
        let world = "world!".to_string();
        let hello_world = hello + &world;
    }
}
