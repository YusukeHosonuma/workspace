// 4.11. 構造体

// 慣習的にアッパーキャメルケースで記述（e.g. PointInSpace）
struct Point {
    x: i32,
    y: i32,
}

fn main() {

    // 構造体
    {
        let origin = Point { x: 0, y: 0 }; // 順番は元の定義順で無くても良い
        println!("The origin is at ({}, {})", origin.x, origin.y);

        let mut point = Point { x: 0, y: 0 };
        point.x = 5;
        println!("The point is at ({}, {})", point.x, point.y);

        // 一時的にミュータブルな構造体にする方法
        let mut point = Point { x: 0, y: 0 };
        point.x = 5;
        let point = point; // この束縛で変更できなくなる
        // Error:
        // point.x = 6;
    }

    // アップデート構文
    //
    // 構造体生成時に `.. コピー元` と指定することで値をコピーして引き継げる
    {
        struct Point3d {
            x: i32,
            y: i32,
            z: i32,
        }

        let mut point = Point3d { x: 0, y: 0, z: 0 };
        point = Point3d { y: 1, .. point }; // x と z はもとの point からコピーされる
    }

    // タプル構造体
    {
        // タプル構造体自体には名前があるが、フィールドには名前がない
        // （フィールド名ではなく、順番によって値が区別される）
        struct Color(i32, i32, i32);
        struct Point(i32, i32, i32);

        // 同じ値を持っていても等しくない（型が違う）
        let black = Color(0, 0, 0);
        let origin = Point(0, 0, 0);

        // 1要素で使う場合、別の型として区別できるような新しい型を作成できる
        // Haskell の newtype と一緒で、「newtype」パターンと呼ばれるらしい
        struct Inches(i32);

        let length = Inches(10);
        let Inches(integer_length) = length; // パターンマッチで取り出し
        println!("length is {} inches", integer_length);
    }

    // Unit-like 構造体
    //
    // 全くメンバを持たない struct を定義できる。
    // 空のタプルである () が unit と呼ばれることから、Unit-like と呼ばれる。
    {
        struct Electron;
        let x = Electron;
    }
}
