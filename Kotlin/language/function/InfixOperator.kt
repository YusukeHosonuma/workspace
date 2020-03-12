package language.function

/*
 `infix`を付けて宣言することで、中置呼び出しが利用できるようになる。
 （逆に言えば、Scalaとは違い明示的に宣言しない限りは中置記法は使えない）
 */
infix fun String.to(value: String) = Pair(this, value)

fun main(args: Array<String>) {

    /*
     以下の呼び出しは等価
     */
    println("One".to("1")) // => (One, 1)
    println("One" to "1")  // => (One, 1)

    /*
     分解宣言（destructuring declaration）
     */
    val (name, value) = "Two" to "2"
    println("name:  $name")  // => name:  Two
    println("value: $value") // => value: 2
}
