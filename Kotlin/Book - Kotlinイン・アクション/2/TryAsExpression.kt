import java.io.*

fun readNumber(reader: BufferedReader) {
    val number: Int? = try {            
        Integer.parseInt(reader.readLine())
    } catch (e: NumberFormatException) {
        null
    }
    println("number is ${number}")
}

fun main(args: Array<String>) {
    val reader = BufferedReader(StringReader("not a number"))
    readNumber(reader)
}
