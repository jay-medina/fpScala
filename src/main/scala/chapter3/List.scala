package chapter3

/* Note 'sealed' means that this trait can only be used in this package */
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def foldRight[A,B](as: List[A], zero: B)( f: (A,B) => B): B =
    as match {
      case Nil => zero
      case Cons(x,xs) => f(x, foldRight(xs,zero)(f))
    }

  def sum(ints: List[Int]): Int =
    foldRight(ints, 0)((x,y) => x + y)

  def product(ds: List[Double]): Double =
    foldRight(ds, 1.0)((x, y) => x + y)


  def apply[A](as: A*): List[A] =
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }




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

  def dropWhileCurried[T](li: List[T])(f: T => Boolean): List[T] = dropWhile(li, f)

  /* exercise 3.6 */
  def init[T](li: List[T]): List[T] = li match {
    case Nil => Nil
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  /* exercise 3.9 */
  def length[T](li : List[T]): Int =
    foldRight(li, 0)((_,y) => 1 + y)

  /* Exercise 3.10 */
  def foldLeft[A,B](as: List[A], zero: B)( f: (B, A) => B): B = {
    def loop(as: List[A], acc: B): B = as match {
      case Nil => acc
      case Cons(x,xs) => loop(xs, f(acc, x))
    }

    loop(as, zero)
  }

  /* exercise 3.11 */
  def length2[T](li : List[T]): Int =
    foldLeft(li, 0)((y, _) => 1 + y)

  def sum2(ints: List[Int]): Int =
    foldLeft(ints, 0)((total,next) => next + total)

  def product2(ds: List[Double]): Double =
    foldLeft(ds, 1.0)((total, n) => n + total)

  /* exercise 3.12 */
  def reverse[T](li: List[T]): List[T] = li match {
    case Nil => List()
    case Cons(x,xs) => append(reverse(xs), List(x))
  }

  def reverseFold[T](li: List[T]): List[T] =
    foldLeft[T, List[T]](li: List[T], List())( (t,n) => append(List(n), t))

  /* exercise 3.14 */
  def appendFold[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((y,total) => Cons(y, total))

  /* exercise 3.15 */
  def flatten[A](li: List[List[A]]): List[A] = li match {
    case Nil => List()
    case Cons(x, xs) => append(x, flatten(xs))
  }

  /* exercise 3.15 */
  def flattenFold[A](li: List[List[A]]): List[A] =
    foldRight[List[A], List[A]] (li, List()) ((y, total) => append(y, total))


  /* exercise 3.18  -- use this to solve 3.16 and 3.17 */
  def map[A,B](as: List[A])(implicit f: A => B): List[B] = as match {
    case Nil => List()
    case Cons(x, xs) => Cons(f(x), map(xs))
  }

  /* exercise 3.19 */
  def filter[A](as: List[A])(implicit f: A => Boolean): List[A] = as match {
    case Nil => List()
    case Cons(x, xs) => if(f(x)) Cons(x, filter(xs)) else filter(xs)
  }

  /* exercise 3.20 */
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    val m = map(as)(f)

    flattenFold(m)
  }

  /* exercise 3.21 */
  def filterUsingFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(x => if(f(x)) List(x) else List())

  /* exercise 3.22 */
  def combineListOfInts(li: List[Int], li2: List[Int]): List[Int] = (li, li2) match {
    case (Cons(x,xs), Cons(y,ys)) => Cons(x + y, combineListOfInts(xs, ys))
    case _ => List()
  }

  /* exercise 3.23 */
  def zipWith[A, B, C](li: List[A], li2: List[B])(combiner: (A, B) => C): List[C] =
    (li, li2) match {
      case (Cons(x,xs), Cons(y,ys)) => Cons(combiner(x,y), zipWith(xs, ys)(combiner))
      case _ => List()
    }

  /* exercise 3.24 */
  def head[A](li: List[A]): A = li match {
    case Nil => throw new Error("No head on empty list")
    case Cons(x, _) => x
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    if(List.length(sub) == 0) true
    else sup match {
      case Nil => false
      case Cons(x,xs) => if(x == head(sub)) hasSubsequence(xs, tail(sub)) || hasSubsequence(xs, sub)
                         else hasSubsequence(xs, sub)
    }
}


