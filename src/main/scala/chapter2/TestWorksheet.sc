def findFirst(arr: Array[Int], cb: Int => Boolean): Int = {
  if(arr.length == 0) -1
  else if(cb(arr(0))) arr(0)
  else findFirst(arr.tail, cb)
}

findFirst(Array(7,9,13), _ == 9)

val lessThan = new Function2[Int, Int, Boolean] {
  def apply(a: Int, b: Int) = a < b
}

lessThan(3, 4)
lessThan(4,3)

def partial1[A,B,C](a: A, f: (A,B) => C): B => C = b => f(a,b)

val li = List(1,2,3,4,5)

li.init

List(2).init

Some(4).getOrElse(Some(7))

None getOrElse 7

Some(4) orElse Some(10)

None orElse Some(33)

Some(4) filter( x => x == 5)

None filter(x => true)