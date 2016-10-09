use std::thread;
use std::time::Duration;
use std::sync::{Mutex, Arc};

struct Philosopher {
    name: String,
    left: usize,  // Tableが保持する forks のインデックス値
    right: usize, // 〃
}

impl Philosopher {

    // こちらはselfを取らないので「関連関数」
    fn new(name: &str, left: usize, right: usize) -> Philosopher {
        // 最後の式が自動的に戻り値となる
        Philosopher {
            name: name.to_string(), // .to_string() によりコピーが作られる
            left: left,
            right: right,
        }
    }

    // selfパラメータを取るので「メソッド」
    fn eat(&self, table: &Table) {

        // lock() を呼び出すことで、別スレッドから並行アクセスされていた場合は、有効になるまでブロックされる。
        //
        // lock() の呼び出しは失敗することがあるが、ロック保持中のスレッドがパニックした場合にしか発生しない。
        // その為、ここでは単純に unwrap() を使っている。
        //
        // 変数束縛の前に「_」をつけることで、Rustに使用しないことを明示し、警告を回避している。
        //
        // ロックの開放は、_left、_right がスコープから抜ける時に自動的に解放される。
        let _left = table.forks[self.left].lock().unwrap();
        thread::sleep(Duration::from_millis(150));
        let _right = table.forks[self.right].lock().unwrap();

        println!("{} is eating.", self.name);

        thread::sleep(Duration::from_millis(1000));

        println!("{} is done eating.", self.name);
    }
}

struct Table {
    // Mutex は並行処理を制御するための機構で、同時にアクセスできるのが1スレッドに限定される。
    forks: Vec<Mutex<()>>,
}

fn main() {

    // Arc<T> はアトミック参照カウント。共有するときは参照カウントを増やし、スレッド終了時にはカウントを減らす。
    let table = Arc::new(Table { forks: vec![
        Mutex::new(()),
        Mutex::new(()),
        Mutex::new(()),
        Mutex::new(()),
        Mutex::new(()),
    ]});

    // Vec<T>（ベクタ）は可変長の配列型。
    let philosopher = vec![
        Philosopher::new("Judith Butler", 0, 1),
        Philosopher::new("Karl Marx", 2, 3),
        Philosopher::new("Gilles Deleuze", 1, 2),
        Philosopher::new("Emma Goldman", 3, 4),
        Philosopher::new("Michel Foucault", 0, 4), // 4, 0 ではなく、こうすることでデッドロックを防ぐ
    ];

    // スレッドを制御するハンドルを束縛。
    // _ は型プレースホルダであり、型をRustが解決するように指示している。
    //
    // into_iter() は所有権を持つイテレータを生成する。
    // map() で引数として要素ごとに順番に呼ばれるクロージャを渡す。
    //
    // collect() で結果をまとめ上げてコレクションにする。（各スレッドへのハンドルのベクタ）
    let handles: Vec<_> = philosopher.into_iter().map(|p| {

        // clone() により参照カウントが増える。
        // スレッドを跨いで、table への参照が何個あるかを知るのに必要。
        let table = table.clone();

        // thread::spawn 関数は、クロージャを1つ引数にとり、新しいスレッド上でそのクロージャを実行する。
        // move を指定することで、キャプチャする値「p」の所有権がクロージャ内に移動する。
        thread::spawn(move || {
            p.eat(&table);
        })
    }).collect();

    // join() を呼び出すことで、各スレッド実行が完了するまで実行をブロックする。
    for h in handles {
        h.join().unwrap();
    }
}
