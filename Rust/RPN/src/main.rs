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
        Some(result) => println!("{} => {}", question, result),
        None => panic!("parse error"),
    }
}

fn pop2(stack: &mut Vec<i32>) -> (i32, i32) {
    if let (Some(n1), Some(n2)) = (stack.pop(), stack.pop()) {
        return (n1, n2)
    }
    panic!("parse error.");
}
