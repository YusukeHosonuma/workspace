extern crate rustserver;

use std::collections::HashMap;
use std::env;
use std::error::Error;
use std::fs::File;
use std::net::{TcpListener, TcpStream};
use std::io::prelude::*;
use std::path::Path;
use std::str::FromStr;
use std::str;

use rustserver::http::{Method, HttpVersion, Request, Server, HttpResult, ContentType};

fn main() {

    let args: Vec<String> = env::args().collect();
    // if args.len() == 1 {
    //     println!("Can't start server.\n");
    //     println!("Usage:");
    //     println!("$ rustserver 5000");
    //     return
    // }

    // Note: 面倒なので引数なしの場合は5000ポートで起動するように
    let port = if args.len() == 1 { "5000" } else { args[1].as_str() };
    let address = format!("localhost:{}", port);

    let listener = match TcpListener::bind(address.as_str()) {
        Err(e) => {
            println!("Can't start server: {}", e);
            return
        },
        Ok(v) => v,
    };
    println!("Startup server: {}", port);

    for stream in listener.incoming() {
        match stream {
            Err(e) => println!("Connection failed: {:?}", e),
            Ok(stream) => {
                handle_client(stream);
            },
        };
    }

    // TODO: need drop() ?
}

fn handle_client(mut stream: TcpStream) {

    // TODO: あとで良いインターフェースにする。

    let mut server = Server { routes: HashMap::new() };

    server.get("*", |request| {

        // remoev first char
        let mut path = request.path.to_string();
        path.remove(0);

        let path = Path::new(path.as_str());

        let mut file = match File::open(&path) {
            Err(_) => return HttpResult::not_found("404 Not found".to_string()),
            Ok(v) => v,
        };

        let mut s = String::new();
        match file.read_to_string(&mut s) {
            Err(why) => panic!("{}", why.description()),
            Ok(v) => v,
        };

        match path.extension() {
            None => HttpResult::ok_html(s),
            Some(ext) => match ext.to_str().unwrap() {
                "html" => HttpResult::ok(ContentType::HTML, s),
                "css"  => HttpResult::ok(ContentType::CSS, s),
                _ => HttpResult::ok(ContentType::HTML, s),
            },
        }
    });

    // index.html を読み込むサンプル
    server.get("/", |_| {

        let path = Path::new("html/index.html");

        let mut file = match File::open(&path) {
            Err(why) => panic!("{}", why.description()),
            Ok(v) => v,
        };

        let mut s = String::new();
        match file.read_to_string(&mut s) {
            Err(why) => panic!("{}", why.description()),
            Ok(v) => v,
        };

        HttpResult::ok_html(s)
    });

    // hello
    server.get("/hello", |_| {
        HttpResult::ok_html("Hello, rust-server".to_string())
    });

    // reply
    server.get("/reply", |request| {
        HttpResult::ok_html(request.raw.to_string())
    });

    // 404 not found
    server.get("/404", |_| {
        HttpResult::not_found("Not found ...".to_string())
    });

    let body = read_request(&stream);
    let mut lines = body.lines();

    let req1 = lines.next().unwrap();
    let v: Vec<&str> = req1.split(' ').collect();

    // TODO: safety
    let method  = v[0];
    let path    = v[1];
    let version = v[2];

    let mut request = Request {
        method: Method::from_str(method),
        path: path,
        version: HttpVersion::from_str(version), 
        raw: body.as_str(),
    };

    server.process(&mut request, &mut stream);
}

fn read_request(mut stream: &TcpStream) -> String {

    let mut buf = [0u8; 4096]; // TODO: なんで4096かは分からない。

    //　リクエストヘッダをそのままレスポンスにするよ
    println!("[start] stream read");
    let mut body = String::new();
    loop {
        let size = stream.read(&mut buf).unwrap();
        let s = match str::from_utf8(&buf[0 .. size]) {
            Ok(v) => v,
            Err(e) => panic!("invalid!: {}", e),
        };
        body.push_str(s);
        if size < 4096 { // TODO: これダメかも？
            break;
        }
    }
    println!("[end  ] stream read");

    body
}
