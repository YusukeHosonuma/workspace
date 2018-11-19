class User(val name: String) {
    var address: String = "unspecified"
        set(value: String) {
            // `field`はバッキングフィールドにアクセスするキーワード
            // カスタムsetterの中で`field`が使われていれば、コンパイラはバッキングフィードを用意する
            println("""
                Address was changed for $name:
                "$field" -> "$value".""".trimIndent())
            field = value
        }
}

class LengthCounter {
    var counter: Int = 0
        private set // 他クラスからは`counter`は更新できなくなる（getterは有効）
    
    fun addWord(word: String) {
        counter += word.length
    }
}

fun main(args: Array<String>) {
    val user = User("Alice")
    user.address = "Saitama"

    val lengthCounter = LengthCounter()
    lengthCounter.addWord("Hi!")
    println(lengthCounter.counter)
}