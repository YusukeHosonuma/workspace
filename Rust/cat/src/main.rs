use std::env;
use std::error::Error;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

fn main() {

    let args: Vec<String> = env::args().collect();

    if args.len() == 1 {
        panic!("no args.");
    }

    let start_arg;
    let show_num;

    if args[1] == "-n" {
        start_arg = 2;
        show_num = true;
    } else {
        start_arg = 1;
        show_num = false;
    }

    for file in &args[start_arg..] {

        let path = Path::new(file);
        let display = path.display();

        let mut file = match File::open(&path) {
            Err(why) => panic!("couldn't open {}: {}", display, why.description()),
            Ok(file) => file,
        };

        let mut text = String::new();
        match file.read_to_string(&mut text) {
            Err(why) => panic!("couldn't open {}: {}", display, why.description()),
            Ok(_) => {
                if show_num {
                    output_with_num(&text);
                } else {
                    output_with_nonum(&text);
                }
            },
        }
    }
}

/// 行番号なしで出力
fn output_with_nonum(text: &String) {
    print!("{}", text);
}

/// 行番号ありで出力
fn output_with_num(text: &String) {
    for (count, line) in text.lines().enumerate() {
        println!("{:6}  {}", count + 1, line);
    }
}
