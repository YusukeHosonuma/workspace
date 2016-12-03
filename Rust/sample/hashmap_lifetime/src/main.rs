use std::collections::HashMap;

fn main() {

    let mut fib: HashMap<i32, i32> = HashMap::new();
    fib.insert(1, 1);
    fib.insert(2, 1);
    fib.insert(3, 2);
    fib.insert(4, 3);
    fib.insert(5, 5);

    // Note:
    // fib.get()の結果`Option<&K>`の`&K`は`fib`と同じライフタイムを持つ。
    // そのため、取得された`fib4`は`fib`への非mut参照として扱われる。
    // 
    // let fib4: &i32 = fib.get(&4).unwrap();
    // let fib5: &i32 = fib.get(&5).unwrap();

    // 1. 参照外し（つまり非mut参照を持たない）
    // （デメリットとしてコピーが発生する）
    // let fib4: i32 = *fib.get(&4).unwrap();
    // let fib5: i32 = *fib.get(&5).unwrap();
    // fib.insert(6, fib4 + fib5);

    // 2. 取得した参照が破棄された上で、`fib6`を利用する方法
    // （`fib6`を利用して`insert`するときには、`fib`への非mut参照は存在しない）
    let fib6: i32 = {
        let fib4: &i32 = fib.get(&4).unwrap();
        let fib5: &i32 = fib.get(&5).unwrap();
        fib4 + fib5
    };

    // ここでmut参照しようとしている為、非mut参照が既にあった場合はコンパイルエラー
    fib.insert(6, fib6);
}
