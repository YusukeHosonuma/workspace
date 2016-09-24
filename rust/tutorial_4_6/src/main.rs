fn main() {

    // 無限ループ
    // loop {
    //     println!("Loop forever!");
    // }


    // whileループ
    //
    // 無限ループのときは`while true`ではなく、単に`loop`としたほうが良い。
    // （コンパイラがより最適なコードを出力できる）
    let mut x = 5;
    let mut done = false;

    while !done {
        x += x - 3;

        println!("{}", x);

        if x % 5 == 0 {
            done = true;
        }
    }

    // forループ
    //
    // Cスタイルのforループは持たない。（間違いやすいため）
    for x in 0..10 { // 10は含まない
        println!("{}", x);
    }

    // 列挙
    for (i, j) in (5..10).enumerate() { // ループ回数が知りたい場合には`.enumerate`する
        println!("i = {} and j = {}", i, j);
    }

    let mut x = 5;

    // break（returnでもループの早期終了となる）
    loop {
        x += x - 3;

        println!("{}", x);

        if x % 5 == 0 { break; }
    }

    // continue
    for x in 0..10 {
        if x % 2 == 0 { continue; }
        println!("{}", x);
    }

    // ループラベル
    //
    // デフォルトの`break`、`continue`は最内ループに適用される。
    'outer: for x in 0..10 {
        'inner: for y in 0..10 {
            if x % 2 == 0 { continue 'outer; } // x のループを継続
            if y % 2 == 0 { continue 'inner; } // y のループを継続
            println!("x: {}, y: {}", x, y);
        }
    }
}
