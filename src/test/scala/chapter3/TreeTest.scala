package chapter3

import org.scalatest.FlatSpec

class TreeTest extends FlatSpec {

  "Tree" should "be a construct of Leaves and branches" in {
    val tree = Leaf(4)

    tree match {
      case Leaf(x) => assert(x == 4)
      case _ => assert(false)
    }
  }

  /* exercise 3.25 */
  it should "have a size of 5" in {
    val tree = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))

    assert(Tree.size(tree) == 5)
  }

  /* exercise 3.26 */
  it should "return the max value" in {
    val tree = Branch(
                 Branch(
                   Leaf(1),
                   Leaf(2)
                 ),
                 Branch(
                   Leaf(-3),
                   Branch(
                     Leaf(5),
                     Leaf(0)
                   )
                 )
               )
    assert(Tree.maximum(tree) === 5)
  }
}
