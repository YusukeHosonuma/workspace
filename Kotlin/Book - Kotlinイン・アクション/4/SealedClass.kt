// `sealed`で宣言されたクラス内で宣言されたクラスは、サブクラスとなる
// 他ファイルでサブクラスを宣言できないため、Exprのサブクラスはファイル内以外では存在しないことが保証される
sealed class Expr {
    class Num(val value: Int) : Expr()
    class Sum(val left: Expr, val right: Expr) : Expr()
    class Mul(val left: Expr, val right: Expr) : Expr()
}

fun eval(e: Expr): Int = 
    // `sealed`なサブクラスを`when`で判定した場合、網羅性がコンパイル時にチェックされる
    // `else`を使うと網羅性のチェックができなくなるので、それはアンチパターン
    when (e) {
        is Expr.Num -> e.value
        is Expr.Sum -> eval(e.left) + eval(e.right)
        is Expr.Mul -> eval(e.left) * eval(e.right)
    }

fun main(args: Array<String>) {
    val expr = Expr.Sum(Expr.Num(1), Expr.Num(2))
    println(eval(expr))
}
