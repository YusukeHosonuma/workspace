// n番目のフィボナッチ数列を求める
object Fib {

  def fib(n: Int): Int = n match {
      case n if n < 3 => 1 // 1, 1, ...
      case n => fib(n - 2) + fib(n - 1) // 2, 3, 5, ...
  }

  def main(args: Array[String]) = {
    (1 to 10).foreach { i =>
      println(fib(i))
    }
  }
}
