use std::error::Error;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

fn main() {
    let path = Path::new("./resource/hello.txt");
    let display = path.display();

    // ファイルオープン
    let mut file = match File::open(&path) {
        Err(why) => panic!("couldn't open {}: {}", display,
                                                   why.description()),
        Ok(file) => file,
    };

    // ファイルの内容を読み込み
    let mut s = String::new();
    match file.read_to_string(&mut s) {
        Err(why) => panic!("couldn't read {}: {}", display,
                                                   why.description()),
        Ok(_) => println!("{} contains:\n{}", display, s),
    }

    // `file`はスコープが外れたタイミングで、`Drop`によって勝手にクローズされる
}
