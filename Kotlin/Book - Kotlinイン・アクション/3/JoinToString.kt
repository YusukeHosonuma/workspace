package strings

/*
トップレベル関数はJava向けに以下のようにファイル名をクラス名としてラップされてコンパイルされる。

```
package strings;

public class JoinToString {
    public static String joinToString(...) { ... }
}
```

ファイル名とクラス名を別のものにしたい場合は、@JvmNameアノテーションを利用する。

```
@file:JvmName("StringFunctions")

package strings

fun joinToString(...): String { ... }
```
 */

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

// ジェネリクスにせずに、特定の型（ここではString）に限定することも出来る
fun Collection<String>.join(
    separator: String,
    prefix: String,
    postfix: String) = joinToString(separator, prefix, postfix)

fun main(args: Array<String>) {
    val list = listOf(1, 2, 3)

    // 通常の呼び出し
    println(list.joinToString("; ", "(", ")"))

    // 引数名を明示
    println(list.joinToString(separator = ";", prefix = "(", postfix = ")"))

    // デフォルト引数を利用できる
    println(list.joinToString(", ", "", ""))
    println(list.joinToString())
    println(list.joinToString("; "))
    
    // 名前付き引数を利用すれば、引数の順序は自由
    println(list.joinToString(postfix = ";", prefix = "#"))

    // `list`の中身がIntなので`join`は呼び出せない
    //list.join(",", "(", ")")
    println(listOf("one", "two", "three").join(",", "(", ")"))
}


