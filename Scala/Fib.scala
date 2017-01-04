// n番目のフィボナッチ数列を求める
object Fib {
  def fib(n:Int):Int = if (n < 3) n else fib(n - 2) + fib(n - 1)
  def main(args:Array[String]) = {
    (1 to 10).foreach { i =>
      println(i + " : " + fib(i))
    }
  }
}
