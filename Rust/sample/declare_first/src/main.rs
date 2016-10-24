fn main() {

    // 宣言だけ先に書いて
    let a_binding;
    {
        let x = 2;

        // あとから初期化（バインド）することが出来る
        a_binding = x * x;
    }

    println!("a binding: {}", a_binding);

    let another_binding;

    // NG: 未初期化の値を使おうとした場合、コンパイルエラー（未定義動作となってしまうため禁止）
    // println!("another binding: {}", another_binding);

    another_binding = 1;
    println!("another binding: {}", another_binding);
}
