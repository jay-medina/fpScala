package TestPackage

import scala.io.StdIn


object UtopianTree {

  def determineGrowth(n : Int): Long = {
    var growth: Long = 1
    var cycles = n

    if(n == 0) return growth

    while(cycles > 1) {
      growth *= 2
      growth += 1
      cycles -= 2
    }

    if(cycles == 1) return growth * 2

    growth
  }

  def main(args: Array[String]) {
    val t = StdIn.readLine().toInt
    var n = 0

    for(i <- 0 until t) {
      n = StdIn.readLine().toInt
      println(determineGrowth(n))
    }
  }
}
