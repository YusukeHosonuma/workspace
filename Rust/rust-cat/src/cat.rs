use std::env;
use std::fs;
use std::fs::File;
use std::io::prelude::*;
use std::io::Error;
use std::io::ErrorKind;

pub struct Config {
    paths: env::Args
}

impl Config {
    pub fn from(mut args: env::Args) -> Config {
        args.next();
        Config { paths: args }
    }
}

pub struct Catenete {}

impl Catenete {

    pub fn execute(config: Config) {

        let results = config.paths.map(|path| {
            match read_textfile(path.as_str()) {
                Ok(s) => s,
                Err(err) => {
                    let s = match err {
                        NotFound   => format!("rust-cat: {}: No such file or directory", path),
                        ReadFailed => format!("rust-cat: {}: Can't read", path),
                        Directory  => format!("rust-cat: {}: Is a directory", path),
                    };
                    String::from(s) + "\n"
                }
            }
        });

        for result in results {
            print!("{}", result);
        }
    }
}

enum ReadTextfileErr {
    NotFound,
    ReadFailed,
    Directory,
}

use self::ReadTextfileErr::{NotFound, ReadFailed, Directory};

impl From<Error> for ReadTextfileErr {
    fn from(err: Error) -> ReadTextfileErr {
        match err.kind() {
            ErrorKind::NotFound => NotFound,
            _ => ReadFailed,
        }
    }
}

fn read_textfile(path: &str) -> Result<String, ReadTextfileErr> {

    if let Ok(m) = fs::metadata(path) {
        if m.is_dir() {
            return Err(Directory)
        }
    };

    let mut content = String::new();
    File::open(path)?.read_to_string(&mut content)?;

    Ok(content)
}