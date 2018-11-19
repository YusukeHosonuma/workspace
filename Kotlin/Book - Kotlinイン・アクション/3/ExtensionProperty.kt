// 読み取り専用の拡張プロパティ
val String.lastChar: Char
    get() = get(length - 1)

// ミュータブルな拡張プロパティ（varで宣言するのに注意）
var StringBuilder.lastChar: Char
    get() = get(length - 1)
    set(value: Char) {
        this.setCharAt(length - 1, value)
    }

fun main(args: Array<String>) {

    println("Kotlin".lastChar)

    val sb = StringBuilder("Kotlin?")
    sb.lastChar = '!'
    println(sb)
}