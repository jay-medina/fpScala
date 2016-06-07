package TestPackage

import scala.io.StdIn

/**
  * Created by jose on 6/5/16.
  */
class TestScala {

}

object Solution {

  def calcAmt(meal: Double, percent: Double): Double =
    meal * (percent / 100.0)

  def main(args: Array[String]) {
    val meal = StdIn.readLine().toDouble
    val tipPercent = StdIn.readLine().toDouble
    val taxPercent = StdIn.readLine().toDouble

    val totalCost = meal + calcAmt(meal, tipPercent) + calcAmt(meal, taxPercent)
    val roundedTotal = Math.round(totalCost)

    println(s"The total meal cost is ${roundedTotal} dollars.")

  }
}
