object FizzBuzzSimple {
  def main(args: Array[String]) = {
    (1 to 20).map {
      case n if (n % 3 == 0) && (n % 5 == 0) => "FizzBuzz"
      case n if (n % 3 == 0) => "Fizz"
      case n if (n % 5 == 0) => "Buzz"
      case n => n.toString
    }.foreach(println)
  }
}
