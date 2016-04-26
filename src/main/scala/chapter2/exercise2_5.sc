def compose[A,B,C](f: B => C, g: A=> B): A => C =
  x => f(g(x))

def doubler(a: Int) = a * 2
def printer(value: Int) = value

val temp = compose(printer, doubler)

printer(doubler(4))
temp(4)