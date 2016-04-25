def curry[A,B,C](f: (A,B) => C): A=> (B => C) =
  a => b => f(a,b)

def adder(A: Int, B: Int): Int = A + B

var curried = curry(adder)
curried(4)(5)

def uncurry[A,B,C](f: A=> B => C): (A, B) => C =
  (x, y) => f(x)(y)

var uncurried = uncurry(curried)

adder(1,2)
curried(1)(2)
uncurried(1,2)