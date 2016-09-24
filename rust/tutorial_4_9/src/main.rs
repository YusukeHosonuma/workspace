// 4.9. ライフタイム

fn main() {

    // 暗黙的
    fn foo(x: &i32) {}

    // 明示的
    //
    // <>内はジェネリクス
    fn bar<'a>(x: &'a i32) {} // 'aは「ライフタイム a」と読む

    // struct もライフタイムを持つことが出来る
    struct Foo<'a> {
        x: &'a i32, // Fooへの参照が、これより長い期間になると不正なアクセスとなるので、その保証が必要
    }
    let y = &5; // `let _y = 5; let y = &_y;` と同じ
    let f = Foo { x: y };
    println!("{}", f.x);

    // impl ブロック
    impl<'a> Foo<'a> {
        fn x(&self) -> &'a i32 { self.x }
    }
    let y = &5; // `let _y = 5; let y = &_y;` と同じ
    let f = Foo { x: y };
    println!("{}", f.x());

    // 複数のライフタイム
    {
        // すべてが同じライフタイムを持つ
        fn x_or_y<'a>(x: &'a str, y: &'a str) -> &'a str { "" }

        // x と y に違うライフタイムを持たせる例
        fn x_or_y2<'a, 'b>(x: &'a str, y: &'b str) -> &'a str { "" }
    }

    // スコープの考え方
    {
        let y = &5;     // -+ yがスコープに入る
                        //  |
        // stuff        //  |
                        //  |
    }                   // -+ yがスコープから出る
    {
        let y = &5;           // -+ yがスコープに入る
        let f = Foo { x: y }; // -+ fがスコープに入る
        // stuff              //  |
                              //  |
    }                         // -+ fとyがスコープから出る
    {
        let x: i32;               // -+ xがスコープに入る
                                  //  |
        {                         //  |
            let y = &5;           // ---+ yがスコープに入る
            let f = Foo { x: y }; // ---+ fがスコープに入る
            //x = &f.x;           //  | | ここでエラーが起きる
        }                         // ---+ fとyがスコープから出る
                                  //  |
        //println!("{}", x);      //  |
    }

    // 'static
    //
    // プログラム全体に渡るタイムラインを持つことを表現する
    {
        let x: &'static str = "Hello, world."; // バイナリのデータセグメントに焼き付けられる

        static FOO: i32 = 5;
        let x: &'static i32 = &FOO;
    }

    // ライフタイムの省略
    //
    // 以下の3つのルールにより省略（推論）出来る
    //
    // 1. 関数の引数の中の省略された各ライフタイムは互いに異なるライフタイムパラメータになる
    //
    // 2. もし入力ライフタイムが1つだけならば、省略されたかどうかにかかわらず、
    //    そのライフタイムはその関数の戻り値の中の省略されたライフタイムすべてに割り当てられる
    //
    // 3. もし入力ライフタイムが複数あるが、その1つが &self または &mut self であれば、
    //    selfのライフタイムは省略された出力ライフタイムすべてに割り当てられる
    //
    // そうでないときは、出力ライフタイムの省略はエラーとなる。
    {
        // 入力ライフタイムを持つ関数
        fn foo1<'a>(bar: &'a str) {}

        // 出力ライフタイムを持つ関数
        fn foo2<'a>() -> &'a str { "" }

        // 両方の位置のライフタイムを持つ関数
        fn foo3<'a>(bar: &'a str) -> &'a str { "" }
    }

    // 例
    // {
    //     fn print(s: &str); // 省略された形
    //     fn print<'a>(s: &'a str); // 展開した形
    //
    //     fn debug(lvl: u32, s: &str); // 省略された形
    //     fn debug<'a>(lvl: u32, s: &'a str); // 展開された形
    //
    //     // 前述の例では`lvl`はライフタイムを必要としません。なぜなら、それは参照（`&`）
    //     // ではないからです。（参照を含む`struct`のような）参照に関係するものだけがライ
    //     // フタイムを必要とします。
    //
    //     fn substr(s: &str, until: u32) -> &str; // 省略された形
    //     fn substr<'a>(s: &'a str, until: u32) -> &'a str; // 展開された形
    //
    //     fn get_str() -> &str; // 不正。入力がない
    //
    //     fn frob(s: &str, t: &str) -> &str; // 不正。入力が2つある
    //     fn frob<'a, 'b>(s: &'a str, t: &'b str) -> &str; // 展開された形。出力ライフタイムが決まらない
    //
    //     fn get_mut(&mut self) -> &mut T; // 省略された形
    //     fn get_mut<'a>(&'a mut self) -> &'a mut T; // 展開された形
    //
    //     fn args<T:ToCStr>(&mut self, args: &[T]) -> &mut Command; // 省略された形
    //     fn args<'a, 'b, T:ToCStr>(&'a mut self, args: &'b [T]) -> &'a mut Command; // 展開された形
    //
    //     fn new(buf: &mut [u8]) -> BufWriter; // 省略された形
    //     fn new<'a>(buf: &'a mut [u8]) -> BufWriter<'a>; // 展開された形
    // }
}
