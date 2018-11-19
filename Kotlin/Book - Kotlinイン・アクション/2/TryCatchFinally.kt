fun readNumber(reader: BufferedReader): Int? {
    try {
        val number = reader.readLine()
        return Integer.parseInt(number)
    } catch (e: NumberFormatException) {
        return null
    } finally {
        reader.close()
    }
}

val reader = BufferedReader(StringReader("239"))
println(readNumber(reader))
