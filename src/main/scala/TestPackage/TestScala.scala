package TestPackage

import scala.io.StdIn

object Solution {

  def evenFibs(n : Long): BigInt = {

    if(n <= 2) 0
    else {
      var total:BigInt = 0
      var f:BigInt = 1
      var next:BigInt = 2

      while(next < n) {
        total += next

        next = f + next
        f = next - f
        next = f + next
        f = next - f
        next = f + next
        f = next - f
      }

      total
    }
  }

  def largePrimeFactor(n: Long): Long = {
    if(n == 2) return 2
    if(isPrime(n)) return n

    val stop = n / 2
    var i = stop

    while(i > 2) {
      if(isPrime(i) && n % i == 0) return i

      i -= 2
    }

    i
  }

  def isPrime(n: Long): Boolean = {
    if(n % 2 == 0) return false

    var i = 3
    val stop = Math.sqrt(n)

    while (i <= stop) {
      if(n % i == 0) return false
      i += 2
    }

    true
  }

  def main(args: Array[String]) {
    val nums = StdIn.readLine().toInt

    for(i <- 0 until nums) {
      val item = StdIn.readLine().toLong
      println (largePrimeFactor(item))
    }

  }

}

