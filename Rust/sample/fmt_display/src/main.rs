// 1.2.2.1 Testcase: List

use std::fmt;

// Vecを内包した構造体
struct List(Vec<i32>);

impl fmt::Display for List {

    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {

        // パターンマッチで分解。`self`は借用なので`ref`で中身の`vec`を取り出す
        let List(ref vec) = *self;

        // write! は fmt::Result を返すため結果を無視すると未使用警告がされる。
        // try! マクロを使用することで、エラーが発生しない場合は継続する。
        try!(write!(f, "["));

        for (count, v) in vec.iter().enumerate() {
            if count != 0 { try!(write!(f, ", ")); }
            try!(write!(f, "{}", v));
        }
        write!(f, "]")
    }
}

fn main() {
    let v = List(vec![1, 2, 3]);
    println!("{}", v);
}
