package chapter4


sealed trait Either[+E, +A] {
  /* Exercise 4.6 */
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(v) => Left(v)
    case Right(v) => Right(f(v))
  }
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(v) => f(v)
    case Left(v) => Left(v)
  }
  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(v) => this
    case _ => b
  }
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)

}
case class Left[+E](value: E) extends Either[E, Nothing] //Failure case
case class Right[+A](value: A) extends Either[Nothing, A] //Success case


object EitherRunner {

  /* exercise 4.7 */
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    es.foldRight[Either[E, List[A]]] (Right(Nil)) ((x, y) => x.map2(y)(_ :: _))

  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es match {
      case Nil => Right(Nil)
      case h::t => (f(h) map2 traverse(t)(f))(_ :: _)
    }

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if(xs.isEmpty) Left("Mean of empty list!")
    else Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x/y)
    catch {case e: Exception => Left(e)}

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {case e: Exception => Left(e)}

  def main(args: Array[String]) {
    val item = 1

    //Test for map
    val r = Right(item)
    val lft = Left("Cannot map")
    println(r.map(x => x + 2))
    println(lft.map(x => "Cannot print"))

  }

}