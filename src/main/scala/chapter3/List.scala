package chapter3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def size[T](li : List[T]): Int = li match {
    case Nil => 0
    case Cons(_, t) => 1 + size(t)
  }

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))


  /* Exercise 3.2 */
  def tail[T](li: List[T]): List[T] = li match {
    case Nil => throw new Error("no tail on empty list")
    case Cons(_, t) => t
  }

  /* Exercise 3.3 */
  def setHead[T](value: T, li: List[T]): List[T] = li match {
    case Nil => throw new Error("cannot set head on empty list")
    case Cons(_, t) => Cons(value, t)
  }

  /* exercise 3.4 */
  def drop[T](l: List[T], n: Int): List[T] = {
    if (n <= 0) l
    else l match {
      case Nil => throw new Error("n is greater than number of items in list")
      case Cons(_, t) => drop(t, n - 1)
    }
  }

  /* exercise 3.5 */
  def dropWhile[T](l : List[T], f: T => Boolean): List[T] = l match {
    case Nil => List()
    case Cons(h, t) => if( f(h) ) dropWhile(t, f) else l
  }

}

object Runner {
  private def exercise3_1(): Unit = {
    val x = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h,t) => h + List.sum(t)
      case _ => 101
    }

    println(x)
  }


  def main(args: Array[String]) {
    exercise3_1()
  }
}


