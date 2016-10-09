// 4.20. Drop

// Drop トレイト
//
// 値がスコープ外になった時にコードを実行する方法を提供する
//

fn main() {

    // Drop の基本
    {
        struct HasDrop;

        impl Drop for HasDrop {
            fn drop(&mut self) {
                println!("Dropping!");
            }
        }

        let x = HasDrop;
    } // x がスコープ外になるので、drop() が呼び出される

    // 複数宣言されている場合に drop() が呼ばれる順番
    {
        struct Firework {
            strength: i32,
        }

        impl Drop for Firework {
            fn drop(&mut self) {
                println!("BOOM times {}!!!", self.strength);
            }
        }

        // 宣言されたときと逆順、つまり tnt, firecracker の順で drop() が呼び出される
        let firecracker = Firework { strength: 1 };
        let tnt = Firework { strength: 100 };
    }
}
