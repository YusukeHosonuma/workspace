// dataクラス
data class Person(val name: String,
                  val age: Int? = null) // ageはnull許容

// mainメソッド（エントリポイント）
fun main(args: Array<String>) {

    // 2人のPersonを作る
    val persons = listOf(Person("Alice"), // ageのデフォルト値はnullなので省略できる
                         Person("Bob", age = 29))

    // 最年長の人物を探す
    val oldest = persons.maxBy { it.age ?: 0 } // ラムダ式、エルビス演算子（?:）
    println("The oldest is: $oldest") // 文字列埋め込み
}

// コンパイルと実行
// $ kotlinc Main.kt -include-runtime -d Main.jar
// $ java -jar Main.jar
// The oldest is: Person(name=Bob, age=29)
