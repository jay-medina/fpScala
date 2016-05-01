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

  /* TODO: exercise 3.27 */
}
