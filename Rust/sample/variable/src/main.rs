// 4. Variable Bindings

fn main() {

    // Rustは静的な型付けによる安全さを提供するが、殆どのケースでは文脈から型を推論できる

    let an_integer = 1u32;
    let a_boolean = true;
    let unit = ();

    let copied_integer = an_integer; // コピーされる

    println!("An integer: {:?}", copied_integer);
    println!("A boolean: {:?}", a_boolean);
    println!("Meet the unit value: {:?}", unit);

    // 変数名の先頭に`_`をつければ、未使用でもコンパイラに警告されなくなる
    let _unused_variable = 3u32;
}
