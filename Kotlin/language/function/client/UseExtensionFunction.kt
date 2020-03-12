package language.function.client

//
// - 異なるパッケージで宣言されたExtension Functionは、staticインポートが必須
// - もちろん `import language.function.*` での一括インポートは有効
//
import language.function.lastChar
import language.function.lastChar as last // `as`で別名をつけることも可能

fun main(args: Array<String>) {
    println("Kotlin".lastChar()) // => n
    println("Kotlin".last())     // => n
}
