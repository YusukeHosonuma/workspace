// 4.29. 型間のキャスト

fn main() {

    // as
    {
        let x: i32 = 5;
        let y = x as i64;
    }

    // 数値キャスト
    {
        let one = true as u8;
        let at_sign = 64 as char;
        let two_hundred = -56i8 as u8;
    }

    // ポインタキャスト
    {
        let a = 300 as *const char; // 300番地へのポインタ
        let b = a as u32; // 数値へのキャスト
    }

    // transmute
    {
        use std::mem;

        unsafe {
            let a = [0u8, 0u8, 0u8, 0u8];
            let b = mem::transmute::<[u8; 4], u32>(a);

            // Error: transmute は安全ではないが、最低限、型のサイズが一致しているかはチェックされる
            // let b = mem::transmute::<[u8; 4], u64>(a);
        }
    }
}
