// 4.22. トレイトオブジェクト

// Rust は静的ディスパッチを指示する一方で、
// 「トレイトオブジェクト」というメカニズムで動的ディスパッチもサポートしている。

trait Foo {
    fn method(&self) -> String;
}

impl Foo for u8 {
    fn method(&self) -> String { format!("u8: {}", *self) }
}

impl Foo for String {
    fn method(&self) -> String { format!("string: {}", *self) }
}

fn main() {

    // 静的ディスパッチ
    {
        fn do_something<T: Foo>(x: T) {
            x.method();
        }

        let x = 5u8;
        let y = "Hello".to_string();

        // Rust が、それぞれの型用の関数を呼び出すように書き換えるので静的ディスパッチになる。
        // （コンパイル時に呼び出される関数は分かっているので、インライン化も出来る）
        //
        // 以下のようなイメージ。
        //
        // fn do_something_u8(x: u8) {
        //     x.method();
        // }
        //
        // fn do_something_string(x: String) {
        //     x.method();
        // }
        // do_something_u8(x);
        // do_something_string(y);
        //
        do_something(x);
        do_something(y);

        // Note:
        // 極端にインライン化された関数は、CPUの命令キャッシュを膨張させてしまう。
        // その結果として、動的ディスパッチのほうが早くなることが稀にある。
    }

    // 動的ディスパッチ
    //
    // トレイトオブジェクトとは、
    // トレイトを実装する「あらゆる型」の値を保持する値。
    {
        // 実行時に仮想関数により実際の関数にディスパッチされる
        fn do_something(x: &Foo) {
            x.method();
        }

        // キャスト
        let x = 5u8;
        do_something(&x as &Foo);

        // 型強制
        let x = "Hello".to_string();
        do_something(&x);
    }
}
