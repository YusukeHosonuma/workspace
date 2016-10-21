#![allow(dead_code)]

enum Status {
    Rich,
    Poor,
}

enum Work {
    Civilian,
    Soldier,
}

fn main() {

    use Status::{Poor, Rich};
    use Work::*;

    // `use`しているおかげで、`Status::Poor`と書かなくて済む
    let status = Poor;
    let work = Civilian;

    // match式においても同様
    match status {
        Rich => println!("The rich have lots of money!"),
        Poor => println!("The poor have no money..."),
    }

    match work {
        Civilian => println!("Civilians work!"),
        Soldier  => println!("Soldiers fight!"),
    }
}
