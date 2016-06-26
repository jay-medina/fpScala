package chapter5

import Stream._
trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  /*exercise 5.6 */
  def headOption_FR: Option[A] =
    this.foldRight(None: Option[A])((h, _) => Some(h))

  /* exercise 5.1 */
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  /* exercise 5.2 */
  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => if(n <= 0) this else t().drop(n-1)
  }

  /* exercise 5.2 */
  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => if(n <= 0) Empty else cons(h(), t().take(n-1))
  }

  /* exercise 5.3 */
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => if( p(h()) ){
                          cons(h(), t().takeWhile(p))
                       }
                       else empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  /* exercise 5.7 */
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  /* exercise 5.7 */
  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if(f(h)) cons(h, t) else t)

  /* exercise 5.7 */
  def append[B>:A](a2: => Stream[B]): Stream[B] =
    foldRight(a2)((h, t) => cons(h, t))


  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  /* exercise 5.4 */
  def forAll(p: A => Boolean) :Boolean = this match {
    case Cons(h, t) => p(h()) && t().forAll(p)
    case _ => true
  }

  def forEach(p: A => Unit): Unit = this match {
    case Cons(h,t) => p(h()); t().forEach(p)
    case Empty =>
  }

  /* exercise 5.5 */
  def takeWhile_FR(p: A=> Boolean): Stream[A] =
    foldRight(empty[A])( (h,t) => {
      if(p(h)) cons[A](h, t)
      else empty
    })

  /* exercise 5.14 */
  def startsWith[A](s: Stream[A]): Boolean = (this, s) match {
    case (Cons(h, t), Cons(h2, t2)) => t().startsWith(t2())
    case (Cons(h, t), Empty) => true
    case (Empty, Empty) => true
    case _ => false
  }

  /* exercise 5.15 */
  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Cons(h, t) => Some(Cons(h,t), t())
      case Empty => None
    } append Stream(empty)


}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl

    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if(as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  /* exercise 5.8 */
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  /* exercise 5.9 */
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  /* exercise 5.10 */
  val fibs: Stream[Int] = {
    def f(x: Int, n: Int): Stream[Int] = cons(x, f(n, x + n))

    f(0, 1)
  }

  /* exercise 5.11 */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h, t)) => cons(h, unfold(t)(f))
      case None => empty
    }

  /* exercise 5.12 */
  def ones_unfold: Stream[Int] = unfold(1)(x => Some((x, x)))

  /* exercise 5.12 */
  def constant_unfold[A](a: A): Stream[A] = unfold(a)(x => Some((x,x)))

  /* exercise 5.12 */
  def from_unfold(n : Int): Stream[Int] = unfold(n)(x => Some(x, x + 1))

  /* exercise 5.12 */
  def fibs_unfold: Stream[Int] = unfold((0, 1)) { case (f0, f1) => Some((f0, (f1, f0 + f1))) }

  /* exercise 5.13 */
  def map_unfold[A,B](as: Stream[A])(f: A => B): Stream[B] =
    unfold(as){
      case Cons(h, t) => Some( f(h()), t())
      case _ => None
    }


  /* exercise 5.13 */
  def take_unfold[A](as: Stream[A], n: Int): Stream[A] =
    unfold((as, n)) {
      case (Cons(h, t), left) => if(left <= 0) None else Some( h(), (t(), left-1))
      case _ => None
    }

  /* exercise 5.13 */
  def takeWhile_unfold[A](as: Stream[A])(f: A => Boolean): Stream[A] =
    unfold(as) {
      case Cons(h,t) => {
        val temp = h()
        if(f(temp)) Some(temp, t())
        else None
      }
      case _ => None
    }

  /* exercise 5.13 */
  def zipWith_unfold[A,B,C](li1: Stream[A], li2: Stream[B])(combiner: (A, B) => C): Stream[C] =
    unfold(li1, li2) {
      case ( Cons(h1, t1), Cons(h2, t2)) => Some(combiner(h1(), h2()), (t1(),t2()))
      case _ => None
    }

  /* exercise 5.13 */
  def zipAll[A, B](s1: Stream[A], s2: Stream[B]): Stream[(Option[A],Option[B])] =
    unfold(s1,s2) {
      case ( Cons(h1, t1), Cons(h2, t2)) => Some( (Some(h1()), Some(h2())) , (t1(), t2()))
      case ( Cons(h1, t1), Empty) => Some( (Some(h1()), None), (t1(), Empty) )
      case ( Empty, Cons(h2, t2)) => Some( (None, Some(h2())), (Empty, t2()) )
      case _ => None
    }
}

object StreamRunner {

  def main(args: Array[String]) {
    val s = Stream(1,2,3,4,5,6)

    //println(s)
    //println(s.toList)

    //println(s.drop(2).toList)
    ///println(s.take(2).toList)
    //println(s.takeWhile(x => x < 4).toList)
    //println(s.takeWhile_FR(x => x < 4).toList)

    //println(s.forAll( x => x > 4))
    //println(s.forAll(_ < 10))

    //println(s.map(_ + 10).toList)
    //println(s.filter(_ % 2 == 1).toList)
    //println(s.append(Stream(1,1,1)).toList)

//    println(Stream.ones.take(3).toList)
//    println(Stream.ones_unfold.take(3).toList)
//    println(Stream.constant(4).take(3).toList)
//    println(Stream.constant_unfold(4).take(3).toList)
//    println(Stream.from(4).take(7).toList)
//    println(Stream.from_unfold(4).take(7).toList)
//    println(Stream.fibs.take(10).toList)
//    println(Stream.fibs_unfold.take(10).toList)

    //println(map_unfold(s)(_ + 10).toList)
    //println(take_unfold(s, 2).toList)
    //println(takeWhile_unfold(s)(_ < 4).toList)

    println( s startsWith Stream(1,2))
    Stream(1,2,3).forEach(println)

    Stream(1,2,3).tails.forEach(x => println(x.toList))
  }
}