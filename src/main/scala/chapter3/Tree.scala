package chapter3

sealed trait Tree[+A]
case class Leaf[A](value:A) extends Tree[A]
case class Branch[A](left:Tree[A], right:Tree[A]) extends Tree[A]

object Tree {

  /* exercise 3.25 */
  def size[A](tree: Tree[A]): Int = tree match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
  }

  /* exercise 3.26 */
  def maximum(tree: Tree[Int]): Int = {
    def loop(t: Tree[Int], big: Int): Int = t match {
      case Leaf(x) => x max big
      case Branch(l, r) => loop(l, big) max loop(r, big)
    }

    loop(tree, Int.MinValue)
  }

  /* exercise 3.27 */
  def depth[A](tree: Tree[A]): Int = {
    def loop(t: Tree[A], d: Int): Int = t match {
      case Leaf(x) => d
      case Branch(l, r) => loop(l, d + 1) max loop(r, d + 1)
    }

    loop(tree, 1)
  }

  /* exercise 3.28 */
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
  }
}
