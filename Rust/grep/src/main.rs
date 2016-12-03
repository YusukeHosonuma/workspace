extern crate clap;
extern crate regex;

use clap::{Arg, App, AppSettings};

fn main() {

    let matches = App::new("my_grep")
        .global_settings(&[AppSettings::ColoredHelp])
        .version("0.1")
        .author("Yusuke Hosonuma <tobi462@gmail.com>")
        .about("grep by self")
        .arg(Arg::with_name("target")
            .required(true)
            .takes_value(true)
            .help("Target file"))
        .arg(Arg::with_name("pattern")
            .required(true)
            .takes_value(true)
            .help("Regex pattern"))
        .get_matches();

    let target = matches.value_of("target").unwrap();
    let pattern = matches.value_of("pattern").unwrap();

    // "Buf.*;"
    // /Users/yusuke/Desktop/workspace/Rust/grep/src/main.rs
    let hits = grep(pattern, target);

    let linesep = str_repeat("-", 80);

    println!("{}", linesep);
    for line in hits.iter() {
        println!("{}", line);
    }
    println!("{}", linesep);
}

fn str_repeat(s: &str, n: usize) -> String {
    std::iter::repeat(s).take(n).collect::<Vec<_>>().join("")
}

// TODO: 戻り値を`Vec<&str>`にしようと思ったけれど、ライフタイムを解決できなかったorz
fn grep(pattern: &str, filepath: &str) -> Vec<String> {

    use std::fs::File;
    use std::io::BufReader;
    use std::io::BufRead;
    use regex::Regex;

    let mut v = vec![];

    let file = match File::open(filepath) {
        Ok(val) => val,
        Err(_) => panic!("file can't open"),
    };

    let re = Regex::new(pattern).unwrap();

    let reader = BufReader::new(file);
    for line in reader.lines().filter_map(|result| result.ok()) {
        if re.is_match(line.as_str()) {
            v.push(line);
        }
    }

    v
}
