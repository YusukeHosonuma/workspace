// A unit struct
struct Nil;

// A tuple struct
struct Pair(i32, f32);

// A struct with two fields
struct Point {
    x: f32,
    y: f32,
}

// 以下のアノテーションをつけることでコンパイル時の警告を回避できる
#[allow(dead_code)]
struct Rectangle { // 他の構造体をフィールドとして宣言できる
    p1: Point,
    p2: Point,
}

fn rect_area(r: Rectangle) -> f32 {
    let Rectangle {
        p1: Point { x: x1, y: y1 },
        p2: Point { x: x2, y: y2 },
    } = r;

    (x2 - x1).abs() * (y2 - y1).abs()
}

fn main() {

    {
        // 構造体の生成
        let rect = Rectangle {
            p1: Point { x: 2.0, y: 3.0 },
            p2: Point { x: 4.0, y: 5.0 },
        };

        // パターンマッチによる構造体の分解（ネスト）
        let Rectangle {
            p1: Point { x: x1, y: y1 },
            p2: Point { x: x2, y: y2 },
        } = rect;

        println!("p1, p2 = ({}, {}), ({}, {})", x1, y1, x2, y2);
    }

    // 構造体の生成
    let point: Point = Point { x: 0.3, y: 0.4 };

    println!("point coordinates: ({}, {})", point.x, point.y);

    // パターンマッチで分解できる
    let Point { x: my_x, y: my_y } = point;

    let _rectangle = Rectangle {
        p1: Point { x: my_y, y: my_x },
        p2: point,
    };
    let area = rect_area(_rectangle);
    println!("_rectangle's area: {}", area);

    // Unit構造体の生成
    let _nil = Nil;

    // タプル構造体の生成
    let pair = Pair(1, 0.1);

    // タプル構造体へのアクセス
    println!("pair contains {:?} and {:?}", pair.0, pair.1);

    // パターンマッチでタプル構造体を分解
    let Pair(integer, decimal) = pair;

    println!("pair contains {:?} and {:?}", integer, decimal);
}
