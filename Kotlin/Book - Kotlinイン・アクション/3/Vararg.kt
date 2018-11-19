// - `vararg`で可変長引数を意味する
// - Javaと同じく配列として受け取ることが出来る
fun printAll(vararg values: String) {
    println("Class: ${values.javaClass}")
    for (value in values) {
        println(value)
    }
}

fun main(args: Array<String>) {
    
    // いくつも引数を渡せる
    printAll("Apple", "Orange", "Banana")

    // Javaとは違って、配列のままでは渡せないので、スプレッド演算子（spread operator） `*`で引数に展開する必要がある
    val numbers: Array<String> = arrayOf("One", "Two", "Three")
    printAll("Zero", *numbers)
}
