extern crate rust_cat;

use std::env;

use rust_cat::cat::Config;
use rust_cat::cat::Catenete;

//
// Rust で `cat` コマンド
//
fn main() {
    let config = Config::from(env::args());
    Catenete::execute(config);
}
