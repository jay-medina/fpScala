package chapter2

/**
  * Sample Module from FPScala Book
  */
object MyModule {

  def abs(n: Int): Int = if(n < 0) -n else n

  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int =
      if( n <= 0) acc
      else go(n-1, n*acc)

    go(n, 1)
  }

  private def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s value of %d is %d"
    msg.format(name, n, f(n))
  }

  private def formatAbs(n: Int) = formatResult("absolute", n, abs)


  private def formatFactorial(n: Int) = formatResult("factorial", n, factorial)

  def main(args: Array[String]) = {
    println(formatAbs(-42))
    println(formatFactorial(7))
  }
}
