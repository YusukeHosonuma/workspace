
// `infix`を頭につけることで、中置呼び出しが利用できるようになる
infix fun String.to(value: String) = Pair(this, value)

fun main(args: Array<String>) {

    // 以下の呼び出しは等価
    println("One".to("1"))
    println("One" to "1")

    // 分解宣言（destructuring declaration）
    val (name, value) = "Two" to "2"
    println("name: ${name}")
    println("value: ${value}")
}