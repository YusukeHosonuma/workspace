use std::thread;

static NTHREADS: i32 = 10;

fn main() {

    let mut children: Vec<thread::JoinHandle<_>> = vec![];

    // OSのネイティブスレッドを生成して、ハンドルを格納
    for i in 0..NTHREADS {
        children.push(thread::spawn(move || { // move をつけることで i を借用ではなくコピーする
            println!("this is thread number {}", i)
        }));
    }

    for child in children {
        // スレッドを join して終了まで待機
        let _ = child.join();
    }
}
