/*
 デフォルトでは＜ファイル名＋Kt＞というクラス名でstaticメソッドが定義されるが、@JvmNameアノテーションで任意の名前に変更可能。
 */
@file:JvmName("StringFunctions")

package language.function

/*
トップレベル関数はJava向けに、以下のようにファイル名をクラス名としてラップされてコンパイルされる。

```java
package strings;

public class FunctionBasicKt {
    public static String joinToString(...) { ... }
}
```
 */

/*
 @JvmOverloadsアノテーションを利用することで、Javaからもデフォルト引数が利用できるようになる。
 （内部的には、引数違いのメソッドがオーバーロード定義される仕組み）
 */
@JvmOverloads
fun <T> Collection<T>.joinToString(
        separator: String = ", ", // デフォルト引数
        prefix: String = "",
        postfix: String = ""
): String {

    val result = StringBuilder()
    result.append(prefix)

    for ((index, element) in this.withIndex()) { // `this`でレシーバオブジェクトを参照
        if (index > 0) result.append(separator)
        result.append(element)
    }

    result.append(postfix)
    return result.toString()
}

/*
 ジェネリクスにせずに、特定の型（ここでは`String`）に限定することも出来る
 */
fun Collection<String>.join(
        separator: String,
        prefix: String,
        postfix: String) = joinToString(separator, prefix, postfix)

fun main(args: Array<String>) {

    val list = listOf(1, 2, 3)

    /*
     通常の呼び出し
     */
    println(list.joinToString("; ", "(", ")"))

    /*
     名前付き引数を利用
     */
    println(list.joinToString(
            separator = ";",
            prefix = "(",
            postfix = ")"
    ))

    /*
     デフォルト引数を利用
     */
    println(list.joinToString(", ", "", ""))
    println(list.joinToString())
    println(list.joinToString("; "))

    /*
     名前付き引数を利用すれば、引数の順序は自由
     */
    println(list.joinToString(postfix = ";", prefix = "#"))

    /*
     `list`の中身がIntなので、レシーバが`Collection<String>`に限定された`join()`は呼び出せない

      ```kotlin
      list.join(",", "(", ")")
      => Error:(88, 5) Kotlin: Type mismatch: inferred type is List<Int> but Collection<String> was expected
      ```
     */
    println(listOf("one", "two", "three").join(",", "(", ")"))
}


