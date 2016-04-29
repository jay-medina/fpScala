package chapter3

import org.scalatest.FlatSpec

class ListTest extends FlatSpec {

  "A List" should "have size 0" in {
    assert(List() == Nil)
  }

  it should "pass exercise 3.1" in {
    val x = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h,t) => h + List.sum(t)
      case _ => 101
    }

    assert(x === 3)
  }

  it should "have size 3" in {
    assert(List.length(List(1,2,3)) === 3)
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

  it should "perform the same drop while test but curried" in {
    val li = List(1,2,3,4,5,6,7,8)

    assert(List.dropWhileCurried(li)(x => x < 5) === List(5,6,7,8))
  }

  /* exercise 3.6 */
  it should "give all elements except the last when calling init" in {
    val li = List(1,2,3,4)

    assert(List.init(li) === List(1,2,3))
  }

  it should "exercise 3.8" in {
    List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))

    assert(true)
  }

  it should "Text fold Left" in {
    val size = List.foldLeft(List(1,2,3), 0)((total, _) => 1 + total)

    assert(size === 3)
  }

  /* exercise 3.12 */
  it should "reverse a given list" in {
    val li = List(1,2,3,4)

    assert(List.reverse(li) === List(4,3,2,1))
  }

  it should "reverse a given list using fold" in {
    val li = List(1,2,3,4)

    assert(List.reverseFold(li) === List(4,3,2,1))
  }

  it should "be able to append two list together using fold" in {
    val li = List(1,2,3)
    val li2 = List(5,6,7)

    assert(List.appendFold(li, li2) === List(1,2,3,5,6,7))
  }

  /* exercise 3.15 */
  it should "be able to flatten a list of lists" in {
    val li = List( List(1,2,3), List(4,5,6), List(7,8,9))

    assert(List.flatten(li) === List(1,2,3,4,5,6,7,8,9))
  }

  /* exercise 3.15 */
  it should "be able to flatten a list of lists using fold" in {
    val li = List( List(1,2,3), List(4,5,6), List(7,8,9))

    assert(List.flattenFold(li) === List(1,2,3,4,5,6,7,8,9))
  }

  /* exercise 3.16 */
  it should "be able to transform a list of ints by adding 1 to each" in {
    val li = List(1,2,3,4,5)

    assert(List.map(li)(x => x + 1) === List(2,3,4,5,6))
  }

  /* exercise 3.17 */
  it should "be able to transform a list of doubles to strings" in {
    val li = List(1.0,2.0,3.0,4.0,5.0)

    assert(List.map(li)(_.toString) === List("1.0","2.0","3.0","4.0","5.0"))
  }

  it should "be able to filter out odd numbers" in {
    val li = List(1,2,3,4,5,6,7,8,9)
    assert(List.filter(li)(x => x % 2 == 0) === List(2,4,6,8))
  }
}
