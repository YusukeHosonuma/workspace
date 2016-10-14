// チャンネル
// http://rustbyexample.com/std_misc/channels.html

use std::sync::mpsc::{Sender, Receiver, RecvError};
use std::sync::mpsc;
use std::thread;

static NTHREADS: i32 = 3;

fn main() {

    // 送信者と受信者を生成
    let (tx, rx): (Sender<i32>, Receiver<i32>) = mpsc::channel();

    for id in 0..NTHREADS {

        // 送信者は clone() できる
        let thread_tx = tx.clone();

        // スレッドを生成し、その中で送信
        thread::spawn(move || {

            // 送信：
            // - unwrap() してるのは未使用警告を避けるため（必須ではない）
            // - tx をそのまま利用すると所有権システムによりコンパイルエラー
            thread_tx.send(id).unwrap();
            println!("thread {} finished", id);
        });
    }

    // 要求される型は「usize」なので要キャスト
    let mut ids: Vec<Result<i32, RecvError>> = Vec::with_capacity(NTHREADS as usize);

    for _ in 0..NTHREADS {
        // 受信：
        // - データが実際に送信されてくるまでカレントスレッドはブロックする。
        // - 仮に送信される数（今回であれば3）よりも多く受信しようとした場合は、ずっとブロックされる（終了しない）
        ids.push(rx.recv());
    }

    // 結果は`Result<T, RecvError>`となる。（どういうときに失敗するかは不明）
    println!("{:?}", ids); // => [Ok(0), Ok(1), Ok(2)]
}
