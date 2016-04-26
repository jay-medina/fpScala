def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
  def loop(index: Int): Boolean = {
    if(index >= as.length - 1) true
    else if(ordered(as(index), as(index + 1))) loop(index + 1)
    else false
  }

  loop(0)
}

isSorted[Int](Array(), _ == _)
isSorted[Int](Array(1), _ == _)
! isSorted[Int](Array(3,2,1), (x,y) => x < y)
isSorted[Int](Array(1,2,3), (x,y) => x < y)
! isSorted[String](Array("hello", "babe", "carrot"), (x, y)=> x < y)
isSorted[String](Array("babe", "carrot", "hello"), (x, y)=> x < y)
