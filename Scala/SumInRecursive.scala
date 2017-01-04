object SumInRecursive {
  def main(args: Array[String]): Unit = {

    if (args.length == 0) {
      println("Usage: scala SumInRecursive 10.")
      return
    }

    val last = args(0).toInt

    def sum(numbers: List[Int]):Int = {
      numbers match {
      	case number :: tail => number + sum(tail)
      	case Nil => 0
      }
    }

    val numbers = (1 to last).toList
    val result = sum(numbers)
    println(result)
  }
}