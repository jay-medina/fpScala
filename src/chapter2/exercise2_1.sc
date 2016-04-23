def fib(n : Int): Int = {
  if(n <= 1) 0
  if(n == 2) 1

  def loop(curr: Int, prev: Int, counter: Int): Int = {
    if(counter > n) curr
    else loop(curr + prev, curr, counter + 1)
  }

  loop(1,0,3)
}

println(fib(3), fib(4), fib(5), fib(6), fib(7), fib(8), fib(9), fib(10))