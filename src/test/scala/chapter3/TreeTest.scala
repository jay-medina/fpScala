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

  /* exercise 3.27 */
  it should "return the depth of a specific tree" in {
    val tree = Branch(
                 Branch(
                   Leaf(1),
                   Branch(
                     Leaf(2),
                     Leaf(0)
                   )
                 ),
                 Branch(
                   Branch(
                     Leaf(5),
                     Leaf(0)
                   ),
                   Leaf(-3)
                 )
               )
    assert(Tree.depth(tree) === 4)
  }

  /* exercise 3.28 */
  it should "map elements of a tree" in {
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

    val r = Branch(
              Branch(
                Leaf(3),
                Leaf(4)
              ),
              Branch(
                Leaf(-1),
                Branch(
                  Leaf(7),
                  Leaf(2)
                )
              )
            )

    val result = Tree.map(tree)(x => x + 2)

    assert(r === result)
  }
}
