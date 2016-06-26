package TestPackage

/**
  * Created by jose on 6/25/16.
  */
object BubbleSort {

  def swap(a: Array[Int], i: Int, j: Int): Unit = {
    val temp = a(i)
    a(i) = a(j)
    a(j) = temp
  }

  def bubbleSort(a: Array[Int], n: Int): Int = {
    var totalSwaps = 0
    var numberOfSwaps = 0

    for (i <- 0 until n){
      numberOfSwaps = 0

      for (j <- 0 until (n-1)){

        if (a(j) > a(j + 1)) {
          swap(a, j, j + 1)
          numberOfSwaps+=1
        }
      }

      totalSwaps += numberOfSwaps

      if (numberOfSwaps == 0) {
        return totalSwaps
      }
    }

    totalSwaps
  }

  def main(args: Array[String]) {
    val sc = new java.util.Scanner (System.in)
    val n = sc.nextInt()
    val a = new Array[Int](n)

    for(a_i <- 0 until n) {
      a(a_i) = sc.nextInt()
    }

    val totalSwaps = bubbleSort(a, n)

    println(f"Array is sorted in $totalSwaps swaps.")
    println(f"First Element: ${a(0)}")
    println(f"Last Element: ${a(n-1)}")
  }

}
