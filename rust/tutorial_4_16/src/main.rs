// 4.16. ベクタ

fn main() {

    // ベクタ：Vec<T>
    //
    // 動的な、または「拡張可能」配列。
    {
        // vec! マクロで作成する。
        let v = vec![1, 2, 3, 4, 5]; // v: Vec<i32>

        // マクロでは[]で書いても、()で書いても一緒
        let v = vec!(1, 2, 3, 4, 5);

        // 初期値（0）の繰り返し
        let v = vec![0; 10];
    }

    // 要素へのアクセス
    {
        let v = vec![1, 2, 3, 4, 5];

        // []で要素へアクセス
        println!("The third element of v is {}", v[2]);

        let i: usize = 0;
        let j: i32 = 0;
        v[i];
        // Error: 要素へのアクセスは usize 型でなければならない。
        // => he trait bound `std::vec::Vec<_>: std::ops::Index<i32>` is not satisfied
        // v[j];
    }

    // イテレーティング
    //
    // ベクタである値に対して、以下の3つの方法でイテレートできる。
    {
        let mut v = vec![1, 2, 3, 4, 5];

        // 参照
        for i in &v {
            println!("A reference to {}", i);
        }

        // ミュータブル参照
        for i in &mut v {
            println!("A mutable reference to {}", i);
        }

        // 所有権を渡す
        for i in v {
            println!("Take ownership of the vector and its element {}", i);
        }
    }
}
