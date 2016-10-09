// 4.34. マクロ

fn main() {

    macro_rules! foo {
        (x => $e:expr) => (println!("mode X: {}", $e));
        (y => $e:expr) => (println!("mode Y: {}", $e));
    }

    foo!(y => 3);

    // Error: パターンにマッチしないのでコンパイルエラー
    // foo!(z => 3);

    // 繰り返し
    {
        macro_rules! o_O {
            (
                $(
                    $x:expr; [ $( $y:expr ),* ]
                );*
            ) => {
                &[ $($( $x + $y ),*),* ]
            }
        }

        let a: &[i32]
            = o_O!(10; [1, 2, 3];
                   20; [4, 5, 6]);
        assert_eq!(a, [11, 12, 13, 24, 25, 26]);
    }

    // 健全性
    {
        macro_rules! five_times {
            ($x:expr) => (5 * $x);
        }
        // メタ変数 $x は、1つの式の頂点としてパースされるので、C言語マクロのように結果が13にはならない
        assert_eq!(25, five_times!(2 + 3));

        fn get_log_state() -> i32 { 10 }
        macro_rules! log {
            ($msg:expr) => {{
                let state: i32 = get_log_state();
                if state > 0 {
                    println!("log({}): {}", state, $msg);
                }
            }}
        }
        // マクロ中でも state を利用しているが、コンフリクトすることはない。
        let state: &str = "reticulating splines";
        log!(state);

        // macro_rules! foo {
        //     () => (let x = 3);
        // }
        // foo!();
        // println!("{}", x);

        macro_rules! foo {
            ($v:ident) => (let $v = 3);
        }
        foo!(x);
        println!("{}", x);

        macro_rules! bar {
            () => (fn y() { });
        }
        bar!();
        y();
    }

    // 再帰的マクロ
    {
        macro_rules! write_html {
            ($w:expr, ) => (());

            ($w:expr, $e:tt) => (write!($w, "{}", $e));

            ($w:expr, $tag:ident [ $($inner:tt)* ] $($rest:tt)*) => {{
                write!($w, "<{}>", stringify!($tag));
                write_html!($w, $($inner)*);
                write!($w, "</{}>", stringify!($tag));
                write_html!($w, $($rest)*);
            }};
        }

        use std::fmt::Write;
        let mut out = String::new();

        write_html!(&mut out,
            html[
                head[title["Macros guide"]]
                body[h1["Macros are the best!"]]
            ]);

        assert_eq!(out,
            "<html><head><title>Macros guide</title></head>\
             <body><h1>Macros are the best!</h1></body></html>");
    }

    // サンプル
    {
        let condition = false;
        if !condition {
            println!("False!!");
        }

        macro_rules! unless {
            ($cond:expr, $then:stmt) => {{
                if ! $cond {
                    $then
                }
            }};
        }

        unless!(condition, {
            println!("False!! with unless.");
        })
    }
}
