package language.function

/*
 拡張関数

 - `String`はレシーバ型（receiver type）
 - `this`は省略可能
 - レシーバ型のカプセル化は破れない（privateメンバへはアクセスできない）
 - 利用するには明示的なインポートが必要

 Javaからはレシーバを第1引数として受け取る静的メソッドにコンパイルされる。

 ```java
 char c = ExtensionFunctionKt.lastChar("Java");
 ```
 */
fun String.lastChar(): Char = this[length - 1]

fun main(args: Array<String>) {

    /**
    - "Kotlin"はレシーバオブジェクト（receiver object）
    - 通常のメソッドとして呼び出せる
     */
    println("Kotlin".lastChar()) // => n
}
