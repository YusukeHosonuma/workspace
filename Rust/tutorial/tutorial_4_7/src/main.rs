fn main() {

    // 所有権（束縛されているものの「所有権を持つ」）
    fn foo() {
        let v = vec![1, 2, 3]; // vはスコープを抜けると解放される
    }

    // ムーブセマンティクス
    //
    // Rustは与えられたリソースに対する束縛が1つだけであることを保証する。
    // 言い換えると、あるオブジェクトへの参照は必ず1つということ。
    //
    // 所有権を何か別のものに転送するとき、参照するものを「ムーブした」と言う。

    let v = vec![1, 2, 3];
    let v2 = v;

    // Error: vの所有権はv2に移っているので、コンパイルエラーとなる
    // => error: use of moved value: `v`
    // println!("v[0] is: {}", v[0]);

    fn take(v: Vec<i32>) {
    }
    let v = vec![1, 2, 3];
    take(v);

    // Error: 関数の引数に渡した場合も同様に所有権が移るので、コンパイルエラーとなる
    // => error: use of moved value: `v`
    // println!("v[0] is: {}", v[0]);

    // Copy型
    //
    // Copyトレイトを実装したものは、完全なコピーが作成される（Value semantics）
    // 言い換えると所有権ルールによる「ムーブ」は発生しない
    let v = 1; // v: i32
    let v2 = v;
    println!("v is: {}", v);

    let a = 5;
    let _y = double(a);
    println!("{}", a);

    let a = true;
    let _y = change_truth(a);
    println!("{}", a);
}

fn double(x: i32) -> i32 {
    x * 2
}

fn change_truth(x: bool) -> bool {
    !x
}
