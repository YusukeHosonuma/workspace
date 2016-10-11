// 逆ポーランド電卓

fn main() {

    // Valid expr
    assert_eq!(resolve("1 5 + 2 3 + *"), Some(30)); // from Wikipedia.

    // Invalid expr
    assert_eq!(resolve("5 + 2 3 + *"),     None); // (+) 処理時にスタックに数字が1つ
    assert_eq!(resolve("1 5 + 2x 3 + *"),  None); // 数値へのパースに失敗
    assert_eq!(resolve("9 1 5 + 2 3 + *"), None); // スタックの数字を使い切らない
}

/// RPN式を解く
///
/// # Arguments
///
/// * `expr` - 対象の式
///
/// # Example
///
/// ```
/// resolve("1 5 + 2 3 + *"); // => Some(30)
/// ```
fn resolve(expr: &str) -> Option<i32> {

    let mut stack: Vec<i32> = Vec::new();

    for s in expr.split(" ") {
        if s == "+" {
            if !pop2_apply(&mut stack, |a, b| a + b) { return None };
        } else if s == "*" {
            if !pop2_apply(&mut stack, |a, b| a * b) { return None };
        } else {
            match s.parse::<i32>() {
                Ok(n)  => stack.push(n),
                Err(_) => return None,
            }
        }
    }

    if stack.len() == 1 { stack.pop() } else { None }
}

fn pop2_apply<F: >(stack: &mut Vec<i32>, apply: F) -> bool where F: Fn(i32, i32) -> i32 {
    if let Some((a, b)) = pop2(stack) {
        stack.push(apply(a, b));
        true
    } else {
        false
    }
}

fn pop2(stack: &mut Vec<i32>) -> Option<(i32, i32)> {
    if let (Some(a), Some(b)) = (stack.pop(), stack.pop()) {
        Some((a, b))
    } else {
        None
    }
}
