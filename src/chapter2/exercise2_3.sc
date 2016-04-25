def curry[A,B,C](f: (A,B) => C): A=> (B => C) =
  a => b => f(a,b)

def adder(A: Int, B: Int): Int = A + B

var curried = curry(adder)
curried(4)(5)