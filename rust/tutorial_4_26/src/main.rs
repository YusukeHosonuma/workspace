
const N: i32 = 5; // 定数（固定アドレスは持たない）

static M: i32 = 5; // グローバル変数（インライン化されず、固定アドレスを持つ）

static NAME: &'static str = "Steve"; // static ライフタイムを持つ

// static な変数に格納される値は「Sync」を実装していて、かつ「Drop」を実装していない必要がある
static mut NM: i32 = 5;

fn main() {

    // `static mut`なグローバル変数は複数スレッドからアクセスされる可能性があるため、
    // `unsafe`ブロックで囲む必要がある。
    unsafe {
        NM += 1;
        println!("NM: {}", NM);
    }
}
