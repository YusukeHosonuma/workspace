// 問題の入力値
val QUESTION_NUMBER = 5

// main関数
fun main(args: Array<String>) {

    // データクラス
    data class Pair<V1, V2>(val fst: V1, val snd: V2)

    // 関数リストを作る
    val fs = listOf(
        Pair("fact",       ::fact),
        Pair("factLoop",   ::factLoop),
        Pair("factExpr",   ::factExpr),
        Pair("factWhen",   ::factWhen),
        Pair("factReduce", ::factReduce),
        Pair("factClass",  ::factClass)
    )

    // 省略時の引数`it`
    val maxLength = fs.map { it.fst.length  }.max()!! // 強制アンラップ

    fs.map {
        val (name, f) = it // デストラクチャリング
        name.padStart(maxLength, ' ') + ": " + f(QUESTION_NUMBER)
    }.forEach {
        println(it)
    }

    val isSuccess = fs.map{ it.snd(QUESTION_NUMBER) }.all { it == 120 }

    // 三項演算子ではなく`if-else`を使う
    val message = if (isSuccess) "そう始まっていたのです！" else "度し難いねぇ..."

    println(message)
    //       fact: 120
    //   factLoop: 120
    //   factExpr: 120
    //   factWhen: 120
    // factReduce: 120
    //  factClass: 120
    // そう始まっていたのです！
}

// 普通のループ
fun fact(n: Int): Int {
    if (n == 1) {
        return 1
    } else {
        return n * fact(n - 1)
    }
}

// ループ
fun factLoop(n: Int): Int {
    var r = 1
    for (i in 2..n) { // Range
        r *= i
    }
    return r // `return`省略は不可
}

// ifを式として利用
fun factExpr(n: Int): Int {
    return if (n == 1) {
               1
           } else {
               n * factExpr(n - 1)
           }
}

// 関数宣言での`when`利用
fun factWhen(n: Int): Int = when (n) {
    1 -> 1
    else -> n * factWhen(n - 1) // whenパターンマッチ
}

// Rangeから`reduce`
fun factReduce(n: Int): Int {
    return (1..n).reduce { n1, n2 -> n1 * n2 } 
}

// クラスを使ってみる
fun factClass(n: Int): Int {
    return Factorial(n).solve()
}

class Factorial(val n: Int) {
    fun solve(): Int {
        return (1..n).reduce { n1, n2 -> n1 * n2 }
    }
}
