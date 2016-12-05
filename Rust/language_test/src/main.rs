
fn main() {
    println!("Hello, world!");
}

#[test]
fn array() {
    let xs = [1, 2, 3];
    assert_eq!(xs.len(), 3);
    assert_eq!(xs[0], 1);
    assert_eq!(xs[1], 2);
    assert_eq!(xs[2], 3);

    let xs = [1; 5];
    assert_eq!(xs.len(), 5);
    for i in 0..5 { // Range 0-4
        assert_eq!(xs[i], 1);
    }
}

#[test]
fn slice() {
    let xs = [0, 1, 2, 3, 5];

    let ys = &xs[..];
    assert_eq!(xs, *ys);

    let zs = &xs[1..4];
    assert_eq!(zs.len(), 3);
    assert_eq!(zs[0], 1);
    assert_eq!(zs[1], 2);
    assert_eq!(zs[2], 3);
}

#[test]
fn tuple() {
    let xs: (i32, &str) = (1, "a");
    assert_eq!(xs.0, 1);
    assert_eq!(xs.1, "a");
}

#[test]
#[allow(unused_parens)]
fn tuple_pattern() {
    let (x, y, z) = (1, 2, 3);
    assert_eq!(x, 1);
    assert_eq!(y, 2);
    assert_eq!(z, 3);

    let x = (0,);
    assert_eq!(x.0, 0);

    let x = (0); // 値「0」
    assert_eq!(x, 0);
}

#[test]
fn tuple_index() {
    let xs = (1, 2, 3);
    assert_eq!(xs.0, 1);
    assert_eq!(xs.1, 2);
    assert_eq!(xs.2, 3);
}

#[test]
fn func() {
    fn add(x: i32, y: i32) -> i32 {
        x + y
    }
    let f: fn(i32, i32) -> i32 = add;
    let x = f(1, 2);
    assert_eq!(x, 3);
}

#[test]
fn _if_() {
    let x = 5;
    let y = if x == 5 { 10 } else { 15 };
    assert_eq!(y, 10);
}

#[test]
fn _loop_() {
    let mut i = 0;
    loop {
        if i == 3 {
            break;
        }
        i += 1;
    }
    assert_eq!(i, 3);
}

#[test]
fn _while_() {
    let mut done = true;
    let mut i = 0;
    while done {
        i += 1;
        if i == 5 { 
            done = false;
        }
    }
    assert!(!done);
    assert_eq!(i, 5);
}

#[test]
fn _for_() {
    let mut r = 0;
    for x in 1..11 {
        r += x;
    }
    // for var in expression { // expression is Iterator that use IntoIterator
    // }
    assert_eq!(r, 55);
}

#[test]
fn iter() {
    let v: Vec<i32> = vec![1, 2, 3];
    let mut iter = v.into_iter();
    assert_eq!(iter.next(), Some(1));
    assert_eq!(iter.next(), Some(2));
    assert_eq!(iter.next(), Some(3));
    assert_eq!(iter.next(), None);
}

#[test]
fn iter_for() {
    let v: Vec<i32> = vec![1, 2, 3];
    let mut r = 0;
    for i in v { 
        r += i;
    }
    assert_eq!(r, 6);
}
