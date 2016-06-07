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

  it should "be able to give the mean of a sequence of doubles" in {
    val arr = Array(1.0, 2.0, 3.0, 4.0)

    assert(OptionRunner.mean(Array[Double]()) == None)
    assert(OptionRunner.mean(arr) == Some(2.5))
  }

  it should "be able to give the variance of a sequence of doubles" in {
    val arr = Array(1.0,2.0,3.0,4.0)

    assert(OptionRunner.variance(Array[Double]()) == None)
    assert(OptionRunner.variance(arr) == Some(1.25))
  }

  it should "be able to convert a list of options to an option of list of items" in {
    val arr = List(Some(4), Some(5), Some(23))
    val arr2 = List(Some(4), None, Some(23))

    assert(OptionRunner.sequence(arr) == Some(List(4,5,23)))
    assert(OptionRunner.sequence(arr2) == None)
  }

  it should "be able to traverse through a list and convert to a list of options" in {
    val arr = List(4,5,23)
    val result = OptionRunner.traverse(arr)(x => Some(x.toString))

   assert(result == Some(arr.map(_.toString)))
  }

}
