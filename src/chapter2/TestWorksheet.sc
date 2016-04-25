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
