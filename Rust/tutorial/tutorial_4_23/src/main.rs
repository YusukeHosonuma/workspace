// 4.23. クロージャ

fn main() {

    // 構文
    {
        // 1行で書いた場合
        let plus_one = |x: i32| x + 1;
        assert_eq!(2, plus_one(1));

        // 複数行で書いた場合
        let plus_two = |x| {
            let mut result: i32 = x;
            result += 1;
            result += 1;
            result
        };
        assert_eq!(4, plus_two(2));

        // 関数（fn）とクロージャの構文の違い
        //
        // 1. クロージャでは型を明示せずとも良い（関数は必須）
        // 2. 構文がちょっと異なる。
        //
        fn  plus_one_v1   (x: i32) -> i32 { x + 1 }
        let plus_one_v2 = |x: i32| -> i32 { x + 1 };
        let plus_one_v3 = |x: i32|          x + 1  ;
    }

    // クロージャとクロージャの環境
    {
        let num = 5;
        let plus_num = |x: i32| x + num; // num を借用している

        assert_eq!(10, plus_num(5));

        // Error: 借用ルールに則っていない
        // => error: cannot borrow immutable local variable `num` as mutable
        // let y = &mut num;

        let mut num = 5;
        {
            let plus_num = |x: i32| x + num;
        } // ここで num の借用が終わる

        let y = &mut num;

        let nums = vec![1, 2, 3];

        let takes_num = || nums; // ここで所有権をムーブしているので、以下はError
        // println!("{:?}", nums);
    }

    // moveクロージャ
    {
        // 通常のクロージャ（moveなし）
        let mut num = 5;
        {
            let mut add_num = |x: i32| num += x; // 環境を変更するため`mut`で宣言する必要あり
            add_num(5); // 変更可能な参照を取得しているので、`num`が書き換わる
        }
        assert_eq!(10, num);

        // moveクロージャ
        let mut num = 5;
        {
            let mut add_num = move |x: i32| num += x; // `num`のコピーの所有権を得る
            add_num(5); // なので元々の`num`は更新されない
        }
        assert_eq!(5, num);
    }

    // クロージャの実装
    //
    // Rustにおけるクロージャはトレイトへの糖衣構文。
    //
    // 関数呼び出しの`()`は、Rustにおいてはオーバーロード可能な演算子。
    // Rustではトレイトを演算子のオーバーロードに利用している。
    //
    // クロージャの構文 `||`、`{}` はトレイトへの糖衣構文。
    //

    // クロージャを引数に取る
    {
        fn call_with_one<F>(some_closure: F) -> i32
            where F : Fn(i32) -> i32 { // Fn はトレイト

            some_closure(1)
        }

        // 以下の呼び出しは静的ディスパッチとなる。
        // （他の多くの言語では、常にヒープにアロケートされて、常に動的ディスパッチとなる）
        let answer = call_with_one(|x| x + 2);
        assert_eq!(3, answer);

        // 動的ディスパッチにしたい場合は以下のようにする。
        fn call_with_one2(some_closure: &Fn(i32) -> i32) -> i32 {
            some_closure(1)
        }
        let answer = call_with_one2(&|x| x + 2); // 参照を渡すために `&||` という構文を使っている
        assert_eq!(3, answer);
    }

    // 関数ポインタとクロージャ
    //
    // 関数ポインタは環境を持たないクロージャとみなせるので、
    // クロージャを引数として期待している関数に関数ポインタを渡せる。
    {
        fn call_with_one(some_closure: &Fn(i32) -> i32) -> i32 {
            some_closure(1)
        }

        fn add_one(i: i32) -> i32 {
            i + 1
        }

        let f = add_one;

        let answer = call_with_one(&f); // 関数ポインタ f を渡している
        assert_eq!(2, answer);

        // 直接指定する場合は以下のようにする。
        let answer = call_with_one(&add_one);
        assert_eq!(2, answer);
    }

    // クロージャを返す
    {
        // 関数から何かを返すためには、Rustは返り値の型のサイズを知る必要があるが、
        // `Fn`はトレイトで、サイズや種類は多岐に渡るので以下はNG。
        //
        // fn factory() -> (Fn(i32) -> i32) {
        //     let num = 5;
        //     |x| x + num
        // }

        // ライフタイムの指定がないためNG
        //
        // fn factory() -> &(Fn(i32) -> i32) {
        //     let num = 5;
        //     |x| x + num
        // }

        // 返り値の型が参照であることを期待しているが、参照を返していないのでNG。
        //
        // fn factory() -> &'static (Fn(i32) -> i32) {
        //     let num = 5;
        //     |x| x + num
        // }

        // あとから解放済みのメモリを参照してしまうのでNG
        //
        // fn factory() -> Box<Fn(i32) -> i32> {
        //     let num = 5;
        //     Box::new(|x| x + num) // 借用された`num`はスタックフレームと同じライフタイムを持つ
        //                           // その為、あとから呼び出された場合、解放済みのメモリを参照することになる
        // }

        // これでOK
        fn factory() -> Box<Fn(i32) -> i32> {
            let num = 5;
            Box::new(move |x| x + num) // move Fn にすることで、クロージャのための新しいスタックフレームが生成される
        }

        let f = factory();
        let answer = f(1);
        assert_eq!(6, answer);
    }
}
