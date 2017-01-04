#![feature(plugin)]
#![plugin(rocket_codegen)]

extern crate rocket;

#[get("/world")]
fn index() -> &'static str {
    "Hello, Rocket!!"
}

fn main() {
    let x = "att";
    rocket::ignite().mount("/hello", routes![index]).launch();
}
