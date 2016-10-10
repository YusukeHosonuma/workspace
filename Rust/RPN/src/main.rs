fn main() {

    let question = "1 5 + 2 3 + *"; // Expected: (1 + 5) * (2 + 3) => 30

    let mut stack: Vec<i32> = Vec::new();

    for s in question.split(" ") {
        if s == "+" {
            let (n1, n2) = pop2(&mut stack);
            stack.push(n1 + n2);
        } else if s == "*" {
            let (n1, n2) = pop2(&mut stack);
            stack.push(n1 * n2);
        } else {
            match s.parse::<i32>() {
                Ok(n)  => stack.push(n),
                Err(_) => panic!("parse error."),
            }
        }
    }

    match stack.pop() {
        None => panic!("parse error"),
        Some(result) => println!("{} => {}", question, result),
    }
}

fn pop2(stack: &mut Vec<i32>) -> (i32, i32) {
    match stack.pop() {
        None => panic!("parse error."),
        Some(n1) => match stack.pop() {
            None => panic!("parse error."),
            Some(n2) => (n1, n2),
        },
    }
}
