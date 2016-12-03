fn fib(n: u32) -> u32 {
	match n {
		0 => 1,
		1 => 1,
		_ => fib(n - 1) + fib(n - 2),
	}
}

fn main() {
	let fib5 = fib(5); // 1, 2, 3, 5, 8
	println!("fib(5): {}", fib5);
}

