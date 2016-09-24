use std::sync::Arc;
use std::cell::RefCell;
use std::cell::Cell;

// 4.10. ミュータビリティ
fn main() {

    {
        let x = 5;
        // Error: デフォルトでイミュータブル
        // x = 6;

        // ミュータブルな変数束縛
        //
        // 束縛がミュータブルであるとき（x）、その束縛が何を指すかを変更して良い（5 -> 6）
        let mut x = 5;
        x = 6;

        // ミュータブル参照
        //
        // y自体はイミュータブルなので他の束縛に変更できないが、参照先は `*y = 5` といった変更可能
        let mut x = 5;
        let y = &mut x; // ミュータブル参照へのイミュータブルな束縛

        // 両方が必要な場合
        //
        // y が他の値を束縛できるし、参照している値を変更することも出来る
        let mut x = 5;
        let mut y = &mut x;

        // mut はパターンの一部として使える
        let (mut x, y) = (5, 6);
    }

    // 内側 vs. 外側のミュータビリティ
    //
    // イミュータビリティの定義：　これは2箇所から差されても安全か？
    {
        // 外側のミュータビリティ
        let x = Arc::new(5);
        let y = x.clone();

        // 内側のミュータビリティ
        let x = RefCell::new(42);
        let y = x.borrow_mut();

        // Runtime error: Rustの借用ルールが実行時に強制され、違反時に panic! が呼び出される
        // => thread 'main' panicked at 'RefCell<T> already borrowed', src/libcore/cell.rs:492
        // let z = x.borrow_mut();
    }

    // フィールド・レベルのミュータビリティ
    //
    // ミュータビリティは、借用（ &mut ）や束縛（ let mut ）に関する属性。
    {
        // Error: 一部がミュータブルで、一部がイミュータブルなフィールドを持つ struct は作れない
        // struct Point {
        //     x: i32,
        //     mut y: i32, // NG
        // }

        struct Point {
            x: i32,
            y: i32,
        }

        let mut a = Point { x: 5, y: 6 };
        a.x = 10;

        let b = Point { x: 5, y: 6 };
        // Error:
        // => rror: cannot assign to immutable field `b.x`
        // b.x = 10;

        // Cell<T> を利用することで、フィールド・レベルのミュータビリティをエミュレートできる
        struct Point2 {
            x: i32,
            y: Cell<i32>,
        }

        let point = Point2 { x: 5, y: Cell::new(6) };
        point.y.set(7);
        println!("y: {:?}", point.y); // ちゃんと 7 に更新される
    }
}
