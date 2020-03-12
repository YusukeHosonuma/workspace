package language.`try`

import java.io.*

private fun readNumber(reader: BufferedReader) {

    //
    // `try`を式として利用し、結果を`number`に代入している
    //
    val number: Int? = try {
        Integer.parseInt(reader.readLine())
    } catch (e: NumberFormatException) {
        null // 例外が発生したら`null`を返す
    }

    println("number is $number") // => number is null
}

fun main(args: Array<String>) {
    val reader = BufferedReader(StringReader("not a number"))
    readNumber(reader)
}
