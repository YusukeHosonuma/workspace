// FizzBazz

fn sample<F: Fn()>(title: &str, execute: F) {
    println!("--- {} ---", title);
    execute();
    println!("");
}

fn main() {

    sample("Plain Old FizzBazz", ||
        for n in 1..20 {
            if n % 15 == 0 {
                println!("FizzBazz");
            } else if n % 3 == 0 {
                println!("Fizz");
            } else if n % 5 == 0 {
                println!("Bazz");
            } else {
                println!("{}", n);
            };
        }
    );

    sample("for を式として利用", ||
        for n in 1..20 {
            let s: String = if n % 15 == 0 {
                "FizzBazz".to_string()
            } else if n % 3 == 0 {
                "Fizz".to_string()
            } else if n % 5 == 0 {
                "Bazz".to_string()
            } else {
                n.to_string()
            };
            println!("{}", s);
        }
    );

    enum FBType {
        Fizz,
        Bazz,
        FizzBazz,
        Other(i32),
    }

    sample("enum / match を利用 (1)", ||
        for n in 1..20 {
            let r: FBType = if n % 15 == 0 {
                FBType::FizzBazz
            } else if n % 3 == 0 {
                FBType::Fizz
            } else if n % 5 == 0 {
                FBType::Bazz
            } else {
                FBType::Other(n)
            };

            match r {
                FBType::FizzBazz => println!("FizzBazz"),
                FBType::Fizz     => println!("Fizz"),
                FBType::Bazz     => println!("Bazz"),
                FBType::Other(n) => println!("{}", n),
            }
        }
    );

    sample("enum / match を利用 (2) - match を式として利用", ||
        for n in 1..20 {
            let r: FBType = if n % 15 == 0 {
                FBType::FizzBazz
            } else if n % 3 == 0 {
                FBType::Fizz
            } else if n % 5 == 0 {
                FBType::Bazz
            } else {
                FBType::Other(n)
            };

            let r = match r {
                FBType::FizzBazz => "FizzBazz".to_string(),
                FBType::Fizz     => "Fizz".to_string(),
                FBType::Bazz     => "Bazz".to_string(),
                FBType::Other(n) => n.to_string(),
            };
            println!("{}", r);
        }
    );

    impl FBType {
        fn to_string(&self) -> String {
            match *self {
                FBType::FizzBazz => "FizzBazz".to_string(),
                FBType::Fizz     => "Fizz".to_string(),
                FBType::Bazz     => "Bazz".to_string(),
                FBType::Other(n) => n.to_string(),
            }
        }
    }

    sample("enum に実装したメソッドを利用", ||
        for n in 1..20 {
            let r: FBType = if n % 15 == 0 {
                FBType::FizzBazz
            } else if n % 3 == 0 {
                FBType::Fizz
            } else if n % 5 == 0 {
                FBType::Bazz
            } else {
                FBType::Other(n)
            };
            println!("{}", r.to_string());
        }
    );

    trait FizzBazz {
        fn fizzBazz(self) -> FBType;
    }

    impl FizzBazz for i32 {
        fn fizzBazz(self) -> FBType {
            if self % 15 == 0 {
                FBType::FizzBazz
            } else if self % 3 == 0 {
                FBType::Fizz
            } else if self % 5 == 0 {
                FBType::Bazz
            } else {
                FBType::Other(self)
            }
        }
    }

    sample("トレイトを利用", ||
        for n in 1..20 {
            println!("{}", n.fizzBazz().to_string());
        }
    );
}
