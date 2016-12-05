
fn main() {
    println!("Hello, world!");
}

#[test]
#[allow(unused_assignments)]
fn mutate() {
    let mut x = 1;
    x = 2;
    assert_eq!(x, 2);    
}

#[test]
#[allow(unused_variables)]
fn shadowing() {
    let x = 1;
    let x = 2;
    assert_eq!(x, 2);
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
fn enumerate() {
    let mut xs = Vec::new();
    for (i, c) in (5..8).enumerate() {
        let s = format!("{}{}", i, c);
        xs.push(s);
    }
    assert_eq!(xs.len(), 3);
    assert_eq!(xs[0], "05");
    assert_eq!(xs[1], "16");
    assert_eq!(xs[2], "27");
}

#[test]
fn enumerate_vec() {
    let v: Vec<char> = vec!['A', 'B', 'C'];
    let mut xs = Vec::new();
    for (i, c) in v.iter().enumerate() {
        let s = format!("{}: {}", i, c);
        xs.push(s);
    }
    assert_eq!(xs.len(), 3);
    assert_eq!(xs[0], "0: A");
    assert_eq!(xs[1], "1: B");
    assert_eq!(xs[2], "2: C");
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

#[test]
fn loop_label() {
    'outer: for x in 0..3 {
        'inner: for y in 0..3 {
            if x % 2 == 0 { continue 'outer; }
            if y % 2 == 0 { continue 'inner; }
            assert_eq!(x, 1);
            assert_eq!(y, 1);
        }
    }
}

#[test]
fn owner() {
    fn push(mut v: Vec<i32>) -> Vec<i32> {
        v.push(4);
        v // return owner
    }
    let mut v = vec![1, 2, 3];
    v = push(v); // 所有権を返してもらう
    assert_eq!(v.len(), 4);
}

#[test]
fn borrowing() {
    fn sum(v: &Vec<i32>) -> i32 { // 借用
        v.into_iter().fold(0, |acc, &x| acc + x)
    }
    let v = vec![1, 2, 3, 4, 5];
    let total = sum(&v); // 参照を渡す
    assert_eq!(total, 15);
    assert_eq!(v.len(), 5); // 所有権がムーブしてないので使える
}

#[test]
fn mut_reference() {
    fn push(v: &mut Vec<i32>) {
        v.push(4);
    }
    let mut v = vec![1, 2, 3];
    push(&mut v);
    assert_eq!(v.len(), 4);
}

#[test]
fn borrowing_rule() {
    let mut x = 5;
    {
        let y = &mut x;
        *y += 1;
    } // ここで &mut借用 が終わる
    assert_eq!(x, 6); // なので、ここで借用できる
}
