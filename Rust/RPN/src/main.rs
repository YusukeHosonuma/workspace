// 逆ポーランド電卓

fn main() {

    // Valid expr
    assert_eq!(resolve("1 5 + 2 3 + *"), Some(30)); // from Wikipedia.

    // Invalid expr
    assert_eq!(resolve("5 + 2 3 + *"),     None); // (+) 処理時にスタックに数字が1つ
    assert_eq!(resolve("1 5 + 2x 3 + *"),  None); // 数値へのパースに失敗
    assert_eq!(resolve("9 1 5 + 2 3 + *"), None); // スタックの数字を使い切らない
}

#[derive(Debug, PartialEq)]
enum Expr {
    Operator(OperatorType),
    Num(i32),
    Unknown,
}

#[derive(Debug, PartialEq)]
enum OperatorType {
    Sum,
    Product,
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

    // トークン解析
    let expr: Vec<Expr> = expr.split(" ").map(|s| {
        if s == "+" {
            Expr::Operator(OperatorType::Sum)
        } else if s == "*" {
            Expr::Operator(OperatorType::Product)
        } else {
            match s.parse::<i32>() {
                Ok(n)  => Expr::Num(n),
                Err(_) => Expr::Unknown,
            }
        }
    }).collect();

    // 未サポートのトークンがあれば失敗
    if expr.contains(&Expr::Unknown) {
        return None
    }

    // 式を評価
    for e in expr {
        match e {
            Expr::Operator(op) => match op {
                OperatorType::Sum     => if !pop2_apply(&mut stack, |a, b| a + b) { return None },
                OperatorType::Product => if !pop2_apply(&mut stack, |a, b| a * b) { return None },
            },
            Expr::Num(n) => stack.push(n),
            Expr::Unknown => panic!("Unknown token not resolved."),
        }
    }

    // スタックに残り1つであれば、それが答え
    if stack.len() == 1 { stack.pop() } else { None }
}

fn pop2_apply<F>(stack: &mut Vec<i32>, apply: F) -> bool where F: Fn(i32, i32) -> i32 {
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
