package language.function

/*
 読み取り専用の拡張プロパティ
 */
val String.lastChar: Char
    get() = this[lastIndex] // get()は[]での呼び出しに置き換え可能

/*
 ミュータブルな拡張プロパティ（varで宣言するのに注意）
 */
var StringBuilder.lastChar: Char
    get() = this[lastIndex]
    set(value: Char) {
        this.setCharAt(lastIndex, value)
    }

fun main(args: Array<String>) {

    println("Kotlin".lastChar) // => n

    val sb = StringBuilder("Kotlin?")
    sb.lastChar = '!'
    println(sb) // => Kotlin!
}
