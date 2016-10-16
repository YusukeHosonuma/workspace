fn main() {

    let no_optimized: [i64;14] = [
        0x0000000101e1fd98,
        0x0000000101e1fda0,
        0x0000000101e1fda8,
        0x00007f8d2940cd50,
        0x0000000101e1fdb0,
        0x00007f8d2940ce80,
        0x00007f8d2940ce88,
        0x0000000000000007,
        0x0000000101e1fdb8,
        0x0000000101e1fdb8,
        0x0000000101e1fdc0,
        0x0000000101e1fdc0,
        0x0000000101e1fdc8,
        0x0000000101e1fdd0,
    ];

    let optimized: [i64;14] = [
        0x0000000106d07dd0,
        0x0000000106d07dd8,
        0x0000000106d07de0,
        0x00007f8fc9c0cd00,
        0x0000000106d07de8,
        0x00007f8fc9c0cdf0,
        0x00007f8fc9c0cdf8,
        0x00007f8fc9c0cde0,
        0x0000000106d07df0,
        0x0000000106d07df0,
        0x0000000106d07e00,
        0x0000000106d07e00,
        0x0000000106d07e08,
        0x0000000106d07e10,
    ];

    let iphone_se: [i64;14] = [
        0x000000016fdb5ef8,
        0x000000016fdb5ef0,
        0x000000016fdb5ee8,
        0x0000000170029b50,
        0x000000016fdb5ee0,
        0x0000000170029b90,
        0x0000000170029b98,
        0x00000001836a0250,
        0x000000016fdb5ed8,
        0x000000016fdb5ed8,
        0x000000016fdb5ec8,
        0x000000016fdb5ec8,
        0x000000016fdb5ed0,
        0x000000016fdb5ec0,
    ];

    let playground: [i64;14] = [
        0x000000011b014f80,
        0x000000011b014f88,
        0x000000011b014f90,
        0x000061800002abd0,
        0x000000011b014f98,
        0x000061800002ae10,
        0x000061800002ae18,
        0x0000000000000000,
        0x000000011b014fa0,
        0x000000011b014fa0,
        0x000000011b014fa8,
        0x000000011b014fa8,
        0x000000011b014fb0,
        0x000000011b014fb8,
    ];

    println!("--- no-optimized ---");
    dump_adr_diff(&no_optimized);

    println!("--- optimized ---");
    dump_adr_diff(&optimized);

    println!("--- iphone_se ---");
    dump_adr_diff(&iphone_se);

    println!("--- playground ---");
    dump_adr_diff(&playground);
}

fn dump_adr_diff(stack: &[i64]) {
    let head = stack[0];
    let diff: Vec<i64> = stack.iter().map(|x| x - head).collect();
    for x in &diff {
        println!("{:>4}", x);
    }
}
