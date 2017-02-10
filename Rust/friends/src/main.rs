use std::env;

// けものフレンズ
fn main() {

    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        panic!("引数が違うよ！");
    }

    let command = &args[1];

    match command.as_str() {
        "wai"      => wai(),
        "sugoi"    => sugoi(),
        "tanoshii" => tanoshii(),
        s => skill(s),
    }
}

fn wai() {
    println!("わーい");
}

fn sugoi() {
    println!("すごーい");
}

fn tanoshii() {
    println!("たのしー");
}

fn skill(skill: &str) {
    println!("君は{}がとくいな、フレンズなんだね!!", skill);
}
