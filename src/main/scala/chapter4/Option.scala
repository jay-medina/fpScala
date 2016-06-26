package chapter4

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B]
  def flatMap[B](f: A => Option[B]): Option[B]
  def getOrElse[B >: A](default: => B): B
  def orElse[B >: A](ob: => Option[B]): Option[B]
  def filter(f: A => Boolean): Option[A]
}
case class Some[+A](get: A) extends Option[A] {
  def map[B](f: A => B): Option[B] = Some(f(get))
  def flatMap[B](f: A => Option[B]): Option[B] = f(get)
  def getOrElse[B >: A](default: => B): B = get
  def orElse[B >: A](ob: => Option[B]): Option[B] = this
  def filter(f: A => Boolean): Option[A] = if(f(get)) this else None


}
case object None extends Option[Nothing] {
  def map[B](f: Nothing => B): Option[B] = None
  def flatMap[B](f: Nothing => Option[B]): Option[B] = None
  def getOrElse[B >: Nothing](default: => B): B = default
  def orElse[B >: Nothing](ob: => Option[B]): Option[B] = ob
  def filter(f: Nothing => Boolean): Option[Nothing] = None
}

object OptionRunner {

  /* exercise 4.2 */
  def variance(xs: Seq[Double]): Option[Double] =
    if(xs.isEmpty) None
    else {
      mean(xs).flatMap( value =>
        mean(
          xs.map(y => Math.pow(y - value, 2.0))
        )
      )
    }

  /* exercise 4.3 */
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap( aa =>
      b map ( bb =>
        f(aa, bb)
        )
      )

  /* exercise 4.5 */
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    Some(a.map(f).map({case Some(x) => x}))


  /* exercise 4.4 */
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
   a.foldRight[Option[List[A]]] (Some(Nil)) ((x,y) => map2(x,y)(_ :: _))


  def mean(xs: Seq[Double]): Option[Double] =
    if(xs.isEmpty) None
    else Some(xs.sum / xs.length)
}
