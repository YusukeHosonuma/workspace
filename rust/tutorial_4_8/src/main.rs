fn main() {

    // 借用を使わないコード
    {
        fn foo(v1: Vec<i32>, v2: Vec<i32>) -> (Vec<i32>, Vec<i32>, i32) {
            // v1 と v2 について作業を行う

            // 所有権と関数の結果を返す
            (v1, v2, 42)
        }

        let v1 = vec![1, 2, 3];
        let v2 = vec![1, 2, 3];

        let (v1, v2, answer) = foo(v1, v2);
    }

    // 借用を使ったコード
    //
    // &T型は「参照」と呼ばれ、リソースを所有するのではなく借用する。
    // 借用した場合は、スコープを外れた時にリソースを開放しない。
    {
        fn foo(v1: &Vec<i32>, v2: &Vec<i32>) -> i32 {
            // v1 と v2 についての作業を行う
            42 // 答えを返す
        }
        let v1 = vec![1, 2, 3];
        let v2 = vec![1, 2, 3];
        let answer = foo(&v1, &v2);
    }

    // 参照はイミュータブルなので変更できない
    {
        fn foo(v: &Vec<i32>) {
            // Error:
            // => error: cannot borrow immutable borrowed content `*v` as mutable
            // v.push(5);
        }
        let v = vec![];
        foo(&v);
    }

    // &mut参照
    //
    // &mutによってミュータブルな参照にでき、`*`を頭につけることによって参照先の値を操作できる
    {
        let mut x = 5; // こちらもミュータブルである必要がある
        // このスコープは必要！
        {
            let y = &mut x; // ミュータブルな参照を取得
            *y += 1; // 参照先を操作
        }
        println!("{}", x);
    }

    // ルール
    //
    // 借用はすべて所有者のスコープより長く存続してはいけない。
    //
    // 次の2種類の借用のうちどちらか1つを持てるが、両方を同時に持つことは出来ない。
    //
    // 1. リソースに対する１つ以上の参照（ &T ）
    // 2. ただ1つのミュータブルな参照（ &mut T ）
    //
    // これがRustがデータ競合をコンパイル時に回避する仕組み。（書き込み可能な &mut が1つしか存在できない）

    // NGな例：
    // {
    //     let mut x = 5;
    //
    //     let y = &mut x;    // -+ xの&mut借用がここから始まる
    //                        //  |
    //     *y += 1;           //  |
    //                        //  |
    //     println!("{}", x); // -+ - ここでxを借用しようとする（が、yによって&mut借用されてるのでNG）
    //                        // -+ xの&mut借用がここで終わる
    // }

    // OKな例：
    {
        let mut x = 5;

        {
           let y = &mut x; // -+ &mut借用がここから始まる
           *y += 1;        //  |
        }                  // -+ ... そしてここで終わる

        println!("{}", x); // <- ここでxを借用しようとする
    }

    // イテレータの無効（繰り返しを行っている間のコレクションの変更はコンパイルエラー）
    {
        // OK:
        let mut v = vec![1, 2, 3];
        for i in &v {
            println!("{}", i);
        }

        // NG:
        let mut v = vec![1, 2, 3];
        for i in &v {
            println!("{}", i);

            // Error: v はループによって借用されるので、それを変更できない
            // => error: cannot borrow `v` as mutable because it is also borrowed as immutable
            // v.push(34);
        }
    }

    // 解放後の使用
    //
    // 参照はリソースよりも長く生存できないことが、コンパイラによりチェックされる。
    {
        let y: &i32;
        {
            let x = 5;

            // Error: x より長い生存期間である y には借用できない
            // => error: `x` does not live long enough
            // y = &x;
        }
        //println!("{}", y);
    }
    {
        let y: &i32;
        let x = 5;

        // Error: 同一スコープで宣言されたものは、宣言と逆順で開放されるため、x -> y の順で開放となりNG。
        // => error: `x` does not live long enough
        // y = &x;
        // println!("{}", y);
    }
}
