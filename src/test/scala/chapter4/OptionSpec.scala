package chapter4

import org.scalatest.FlatSpec

class OptionSpec extends FlatSpec{

  "Option" should "be able to contain a value of one type" in {
    val a = Some(4)

    assert(a.get == 4)
  }

  /* Exercise 4.1 */
  it should "be able to contain a none value" in {
    val a = None

    assert(a === None)
  }

  it should "be able to map one value to another" in {
    assert(Some(5).map(x => x + 4) === Some(9))
    assert(None.map(x => 3) === None)
  }

  it should "be able to flatMap an Option of Options" in {
    val a = Some(Some(33))

    assert(a.flatMap(x => x) === Some(33))
    assert(None.flatMap(x => x) === None)
  }

  it should "be able to provide a method to get the value or a default one" in {
    assert(Some(4).getOrElse(7) === 4)
    assert(None.getOrElse(7) === 7)
  }

  it should "be able to provide a method to get an alternative value if None" in {
    assert(Some(4).orElse(Some(7)) === Some(4))
    assert(None.orElse(Some(7)) === Some(7))
  }

  it should "be able to filter a value" in {
    assert(Some(4).filter(x => x == 5) === None)
    assert(Some(5).filter(x => x == 5) === Some(5))
    assert(None.filter(_ == 5) === None)
  }

}
