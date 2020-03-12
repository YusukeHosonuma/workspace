@file:JvmName("FileSystemConstants")
package language.property

/*
 通常のプロパティ（アクセサメソッド）としてJavaからは見える
 （Kotlinではキャメルケースでの名前が推奨されるっぽい）
 */
val unixLineSeparator = "\n"

//
/*
`const`をつけると`static final`な定数としてJavaから見える

 ```java
 public static final String WINDOWS_LINE_SEPARATOR = "\n";
 ```
 */
const val WINDOWS_LINE_SEPARATOR = "\r\n"

fun main(args: Array<String>) {

    println(unixLineSeparator)
    println(WINDOWS_LINE_SEPARATOR)
}