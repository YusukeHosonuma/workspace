fn main() {

    // 殆どは式で;で終わる

    // 束縛（コメント）
    let x: i32 = 5;
    // x = 10; error: 束縛はイミュータブル

    // パターン
    let (y, z) = (1, 2);

    // ミュータブル
    let mut x2 = 5;
    x2 = 10;

    // 未初期化の変数は使えない
    let x3: i32;
    // println!("The value of x3 is: {}", x3);

    let x4: i32 = 17;
    {
        let y4: i32 = 3;
        println!("The value of x is {} and value of y is {}", x4, y4);
    }
    // Error: ブロック（{}, 関数）の範囲外からはアクセスできない
    // println!("The value of x is {} and value of y is {}", x4, y4);

    // シャドーイング（同じ名前で束縛すると、前の束縛を上書きする）
    // ミュータブルと似ているが、機能としては完全に直行している（らしい）
    {
        let x: i32 = 8;
        {
            println!("{}", x);
            let x = 12;
            println!("{}", x);
        }
        println!("{}", x); // => 8　（12ではない！）
        let x = 42;
        println!("{}", x);
    }

    {
        let mut x: i32 = 1;
        x = 7;
        let x = x; // イミュータブルになって束縛

        let y = 4;
        let y = "I can also be bound to text!"; // 違う型で束縛しなおせる（シャドーイング）
    }
}
