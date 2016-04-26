package chapter3

import org.scalatest.FlatSpec

class ListTest extends FlatSpec {

  "A List" should "have size 0" in {
    assert(List() == Nil)
  }

  it should "have size 3" in {
    assert(List.size(List(1,2,3)) === 3)
  }

  /* exercise 3.2 */
  it should "retrieve the tail of a list" in {
    val x = List(1,2,3,4,5)

    assert(List.tail(x) === List(2,3,4,5))
  }

  /* exercise 3.2 */
  it should "throw an error when retrieving the tail" in {
    intercept[Error] {
      List.tail(List())
    }
  }

  /* exercise 3.3 */
  it should "set the head to a different value" in {
    val li = List(1,2,3,4)
    val nLi = List.setHead(10, li)

    assert(nLi === List(10,2,3,4))
    assert(nLi !== li)
  }

  /* exercise 3.4 */
  it should "drop the first n elements from a list" in {
    val li = List(1,2,3,4,5,6)
    val n = 3

    assert(List.drop(li, n) === List(4,5,6))
  }

  /* exercise 3.5 */
  it should "drop while elements are less than 5" in {
    val li = List(1,2,3,4,5,6,7,8)

    assert(List.dropWhile(li, (x: Int) => x < 5) === List(5,6,7,8))
  }
}
