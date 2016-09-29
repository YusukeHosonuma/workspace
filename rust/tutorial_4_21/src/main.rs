// 4.21. if let

fn main() {

    // if let
    {
        let option: Option<i32> = Some(42);

        // 通常のパターンマッチ
        match option {
            Some(x) => { println!("{}", x); },
            None => {},
        }

        // 明示的なアンラップ
        if option.is_some() {
            let x = option.unwrap();
            println!("{}", x);
        }

        // if let
        if let Some(x) = option {
            println!("{}", x);
        } else {
            println!("None.");
        }
    }

    // while let
    {
        // loop して、値がなくなったら break
        let mut v = vec![1, 3, 5, 7, 11];
        loop {
            match v.pop() {
                Some(x) => println!("{}", x),
                None => break,
            }
        }

        // while let を使ったバージョン
        let mut v = vec![1, 3, 5, 7, 11];
        while let Some(x) = v.pop() {
            println!("{}", x);
        }
    }
}
