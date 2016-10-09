// 4.35. 生ポインタ

fn main() {

    {
        let x = 5;
        let raw = &x as *const i32;

        let mut y = 10;
        let raw_mut = &mut y as *mut i32;

        let x = 5;
        let raw = &x as *const i32;
        let points_at = unsafe { *raw }; // 生ポインタを参照外しする時、unsafe をつける必要がある
        println!("raw points as {}", points_at);
    }

    // 参照と生ポインタ
    {
        let i: u32 = 1;
        let p_imm: *const u32 = &i as *const u32;

        let mut m: u32 = 2;
        let p_mut: *mut u32 = &mut m;

        // 生ポインタから &T、&mut T への変換
        unsafe {
            let ref_imm: &u32 = &*p_imm;
            let ref_mut: &mut u32 = &mut *p_mut;
        }
    }
}
