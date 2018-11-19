// `substringBeforeLast`と`substringAfterLast`という拡張メソッドが使える
fun parsePath(path: String) {
    val directory = path.substringBeforeLast("/")
    val fullName = path.substringAfterLast("/")

    val fileName = fullName.substringBeforeLast(".")
    val extension = fullName.substringAfterLast(".")

    println("Dir: ${directory}, name: ${fileName}, ext: ${extension}")
}

fun parsePathWithRegex(path: String) {
    // トリプルクォート文字列（triple-quoted string）を使うと`\`のエスケープが不要
    val regex = """(.+)/(.+)\.(.+)""".toRegex()
    val matchResult = regex.matchEntire(path)
    if (matchResult != null) {
        val (directory, fileName, extension) = matchResult.destructured
        println("Dir: ${directory}, name: ${fileName}, ext: ${extension}")
    }
}

fun main(args: Array<String>) {

    // .toRegexで明示的に正規表現の型に変換する
    println("12.345-6.A".split("\\.|-".toRegex()))

    // 今回のような単純なケースでは引数に渡すだけでもOK（Javaではこれは出来ない）
    println("12.345-6.A".split(".", "-"))

    parsePath("/Users/yole/kotlin-book/chapter.adoc")
    parsePathWithRegex("/Users/yole/kotlin-book/chapter.adoc")
}