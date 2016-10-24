fn main() {

    let x = 1;

    {
        let y = 2;
        println!("inner y: {}", y);

        // 外のスコープの`x`をシャドウイングする
        let x = 5_f32;
        println!("inner x: {}", x);
    }

    // NG: スコープ外の束縛は参照できない
    // println!("outer y: {}", y);

    println!("outer x: {}", x);

    // 同じスコープの`x`をシャドウィングする
    // `x = 'a';`は再代入になるのでNG（`let mut`で宣言しても型が違うのでコンパイルエラー）
    let x = 'a';
    println!("outer x: {}", x);
}
