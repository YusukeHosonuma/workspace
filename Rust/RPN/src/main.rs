fn main() {

    let question = "1 5 + 2 3 + *"; // Expected: (1 + 5) * (2 + 3) => 30

    let mut stack: Vec<i32> = Vec::new();

    for s in question.split(" ") {
        if s == "+" {
            pop2_apply(&mut stack, |a, b| a + b);
        } else if s == "*" {
            pop2_apply(&mut stack, |a, b| a * b);
        } else {
            match s.parse::<i32>() {
                Ok(n)  => stack.push(n),
                Err(_) => panic!("parse error."),
            }
        }
    }

    match stack.pop() {
        Some(result) => {
            println!("{} => {}", question, result);
            assert_eq!(result, 30);
        },
        None => panic!("parse error"),
    }
}

fn pop2_apply<F: >(stack: &mut Vec<i32>, apply: F) where F: Fn(i32, i32) -> i32 {
    let (a, b) = pop2(stack);
    stack.push(apply(a, b));
}

fn pop2(stack: &mut Vec<i32>) -> (i32, i32) {
    if let (Some(a), Some(b)) = (stack.pop(), stack.pop()) {
        return (a, b)
    }
    panic!("parse error.");
}
