object SumInFoldLeft {
  def main(args:Array[String]) = {
    val result = (1 to 10).foldLeft(0)(_ + _)
    println("result: " + result)
  }
}