#![allow(dead_code)]

fn fib(n: u32) -> u32 {
    match n {
        0 => 0,
        1 => 1,
        _ => fib(n - 1) + fib(n - 2),
    }
}

#[test]
fn test_base_case() {
    assert_eq!(fib(0), 0);
    assert_eq!(fib(1), 1);
}

#[test]
fn test_induction_case() {
    assert_eq!(fib(2), 1);
    assert_eq!(fib(26), 121393);
    assert_eq!(fib(38), 39088169);
}
