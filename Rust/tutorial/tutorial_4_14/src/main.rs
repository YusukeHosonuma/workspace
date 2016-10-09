// 4.14. パターン

fn main() {

    // パターン
    {
        let x = 1;

        match x {
            1 => println!("one"),
            2 => println!("two"),
            3 => println!("three"),
            _ => println!("anything"),
        }

        let x = 'x';
        let c = 'c';

        match c {
            x => println!("x: {}, c: {}", x, c), // x はシャドーイングされることに注意！
        }
        println!("x: {}", x);
    }

    // 複式パターン
    {
        let x = 1;

        match x {
            1 | 2 => println!("one or two"),
            3 => println!("three"),
            _ => println!("anything"),
        }
    }

    // デストラクチャリング
    {
        struct Point {
            x: i32,
            y: i32,
        }

        let origin = Point { x: 0, y: 1 };

        // 構造体に対するパターンマッチ（コンストラクトの逆計算）
        match origin {
            Point { x, y } => println!("({}, {})", x, y),
        }

        // 値に別の名前をつけたい場合は、 : を使う
        match origin {
            Point { x: x1, y: y1 } => println!("({}, {})", x1, y1),
        }

        // 値の一部だけを使いたい場合
        match origin {
            Point { x, .. } => println!("x is {}", x),
        }

        // 最初の要素じゃなくても同じようにパターンマッチ可能
        match origin {
            Point { y, .. } => println!("y is {}", y),
        }
    }

    // 束縛の無視
    {
        let some_value: Result<bool, String> = Result::Ok(true);

        // _ でマッチした値を無視（捨てている）
        match some_value {
            Ok(value) => println!("got a value: {}", value),
            Err(_) => println!("an error occurred"),
        }

        fn coordinate() -> (i32, i32, i32) {
            (1, 2, 3)
        }

        // タプルのパターンマッチ
        let (x, _, z) = coordinate();

        enum OptionalTuple {
            Value(i32, i32, i32),
            Missing,
        }

        let x = OptionalTuple::Value(5, -2, 3);

        // .. でパターン内の複数の値を無視できる
        match x {
            OptionalTuple::Value(..) => println!("Got a tuple!"),
            OptionalTuple::Missing => println!("No such luck."),
        }
    }

    // リファレンスの取得
    {
        let x = 5;

        // ref でリファレンス（この場合は &i32型）を取得
        match x {
            ref r => println!("Got a reference to {}", r),
        }

        let mut x = 5;

        // ref mut
        match x {
            ref mut mr => println!("Got a mutable reference to {}", mr),
        }
    }

    // レンジ
    {
        let x = 1;

        // ... で値のレンジのマッチ
        match x {
            1 ... 5 => println!("one through five"),
            _ => println!("anything"),
        }

        // レンジは大体、整数か char型 で使う
        let x = '💅';

        match x {
            'a' ... 'j' => println!("early letter"),
            'k' ... 'z' => println!("late letter"),
            _ => println!("something else"),
        }
    }

    // 束縛
    {
        let x = 1;

        // @ でマッチした値を参照できる
        match x {
            e @ 1 ... 5 => println!("got a range element {}", e),
            _ => println!("anything"),
        }

        #[derive(Debug)]
        struct Person {
            name: Option<String>,
        }

        let name = "Steve".to_string();
        let mut x: Option<Person> = Some(Person { name: Some(name) });

        // データ構造の一部に対するマッチングなどに使える
        match x {
            Some(Person { name: ref a @ Some(_), .. }) => println!("{:?}", a),
            _ => {},
        }

        let x = 5;

        // | で @ を使う場合
        match x {
            e @ 1 ... 5 | e @ 8 ... 10 => println!("got a range element {}", e),
            _ => println!("anything"),
        }
    }

    // ガード
    {
        enum OptionalInt {
            Value(i32),
            Missing,
        }

        let x = OptionalInt::Value(5);

        // マッチガードによって、値の属性によって振り分けられる
        match x {
            OptionalInt::Value(i) if i > 5 => println!("Got an int bigger than five!"),
            OptionalInt::Value(..) => println!("Got an int!"),
            OptionalInt::Missing => println!("No such luck."),
        }

        // 複式パターンで if を使うと、両方に適用される
        let x = 4;
        let y = false;

        match x {
            4 | 5 if y => println!("yes"),
            _ => println!("no"),
        }
    }


}
