use std::collections::HashMap;
use std::net::{TcpStream};
use std::io::prelude::*;
use std::str::FromStr;

// HTTP method
#[derive(PartialEq)]
pub enum Method {
    Get,
    Post,
    Put,
    Delete,
}

impl Method {
    pub fn from_str(method: &str) -> Method {
        use self::Method::*;
        match method {
            "GET"    => Get,
            "POST"   => Post,
            "PUT"    => Put,
            "DELETE" => Delete,
            _ => panic!("not support http method: {}", method),
        }
    }
}
    
#[test]
fn from_str_test() {
    use self::Method::*;
    assert!(Method::from_str("GET")    == Get);
    assert!(Method::from_str("POST")   == Post);
    assert!(Method::from_str("PUT")    == Put);
    assert!(Method::from_str("DELETE") == Delete);
} 

// HTTP version
#[derive(PartialEq)]
pub enum HttpVersion {
    Version1_1,
    Version2,
}

impl HttpVersion {
    pub fn from_str(version: &str) -> HttpVersion {
        use self::HttpVersion::*;
        match version {
            "HTTP/1.1" => Version1_1,
            "HTTP/2"   => Version2,
            _ => panic!("not support http version: {}", version),
        }
    }
}

// Wrap HTTP Request message.
pub struct Request<'a> {
    pub method: Method,
    pub path: &'a str,
    pub version: HttpVersion,
    pub raw: &'a str,
}

enum HttpStatus {
    Ok,
    NotFound,
}

pub enum ContentType {
    HTML,
    CSS,
}

pub struct HttpResult {
    status: HttpStatus,
    content_type: ContentType,
    message: String, // TODO: &str と String の使い分けが難しい・・・借用システム周りで躓く
}

impl HttpResult {

    pub fn ok_html(body: String) -> HttpResult {
        HttpResult::ok(ContentType::HTML, body)
    }

    pub fn ok(c_type: ContentType, body: String) -> HttpResult {
        HttpResult {
            status: HttpStatus::Ok,
            content_type: c_type,
            message: body,
        }
    }

    pub fn not_found(body: String) -> HttpResult {
        HttpResult {
            status: HttpStatus::NotFound,
            content_type: ContentType::HTML,
            message: body,
        }        
    }
}

// Rooting
pub struct Server<'a> {
    pub routes: HashMap<String, Box<FnMut(&Request) -> HttpResult + 'a>>, // TODO: `+ 'a` ってのは後で調べる
}

impl<'a> Server<'a> {

    pub fn get<F: FnMut(&Request) -> HttpResult + 'a>(&mut self, path: &str, handler: F) {
        self.routes.insert(path.to_string(), Box::new(handler));
    }

    pub fn process(&mut self, request: &Request, mut stream: &TcpStream) {

        let path = if self.routes.contains_key("*") { "*" } else { request.path };

        self.routes.get_mut(path).map({ |f| {

            let result = f(request);
            match result.status {

                // 200
                HttpStatus::Ok => {
                    let body = result.message;

                    let c_type = match result.content_type {
                        ContentType::HTML => "text/html",
                        ContentType::CSS => "text/css",
                    };

                    let mut msg = String::new();
                    msg.push_str("HTTP/1.1 200 OK\r\n");
                    msg.push_str(format!("Content-Type: {}; charset=UTF-8\r\n", c_type).as_str());
                    msg.push_str("Content-Length: ");
                    msg.push_str(body.len().to_string().as_str());
                    msg.push_str("\r\n");
                    msg.push_str("Connection: Close\r\n");
                    msg.push_str("\r\n");
                    msg.push_str(body.as_str());

                    stream.write(msg.as_bytes()).unwrap();
                },

                // 404
                HttpStatus::NotFound => {

                    let body = result.message;
                    let mut msg = String::from_str(concat!(
                        "HTTP/1.1 404 NotFound\r\n",
                        "Content-Type: text/html; charset=UTF-8\r\n",
                        "Content-Length: "
                    )).unwrap();

                    msg.push_str(body.len().to_string().as_str());
                    msg.push_str("\r\n");
                    msg.push_str("Connection: Close\r\n");
                    msg.push_str("\r\n");
                    msg.push_str(body.as_str());

                    stream.write(msg.as_bytes()).unwrap();
                },
            }
        }});
    }
}
