package language.`try`

import java.io.BufferedReader
import java.io.StringReader

private fun readNumber(reader: BufferedReader): Int? {

    //
    // try-catch-finally
    //
    // - Javaと基本的に同じだが結果を返すことが出来る
    //
    try {
        val number = reader.readLine()
        return Integer.parseInt(number)
    } catch (e: NumberFormatException) {
        return null
    } finally {
        reader.close()
    }
}

fun main(args: Array<String>) {
    val reader = BufferedReader(StringReader("239"))
    println(readNumber(reader)) // => 239
}
