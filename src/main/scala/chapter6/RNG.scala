package chapter6


trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG2 {

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = x => x.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def randomPair(rng: RNG): ((Int,Int), RNG) = {
    val (i1, rng2) = rng.nextInt
    val (i2, rng3) = rng2.nextInt

    ((i1,i2), rng3)
  }

  /* exercise 6.1 */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i ,r) = rng.nextInt

    (if (i < 0) (-i + 1) else i, r)
  }

  /* exercise 6.2 */
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)

    (i / (Int.MaxValue.toDouble + 1), r)
  }

  /* exercise 6.3 */
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)

    ((i, d), rng3)
  }

  /* exercise 6.3 */
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i1, d1), rng2) = intDouble(rng)

    ((d1, i1), rng2)
  }

  /* exercise 6.3 */
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)

    ((d1,d2,d3), r3)
  }

  /* exercise 6.4 */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def loop(ct: Int, rng: RNG, acc: List[Int]): (List[Int], RNG) = {
      if(ct <= 0) (acc, rng)
      else {
        val (i1, r1) = rng.nextInt
        loop(ct - 1, r1, i1 :: acc)
      }
    }

    loop(count, rng, Nil)
  }

  /* exercise 6.5 */
  def double_map: Rand[Double] =
    map(nonNegativeInt)(x => x / (Int.MaxValue.toDouble + 1))

  /* TODO exercise 6.6 */
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] = ???


  def main(args: Array[String]) = {

    val myRNG = SimpleRNG(42)
    val (n1, rng2) = myRNG.nextInt

    //println(n1, rng2)

    //println(double(rng2))

    //(ints(4)(myRNG))._1.foreach(println)

    println (double(myRNG))
    println (double_map(myRNG))
  }
}