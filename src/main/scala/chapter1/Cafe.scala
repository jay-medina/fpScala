package chapter1

class Cafe {

  def buyCoffee(cc: CreditCard): (Coffee, Charge) = {
    val cup = new Coffee
    (cup, Charge(cc, cup.price))
  }

  def buyCoffees(cc: CreditCard, n: Int): (List[Coffee], Charge) = {
    val purchases = List.fill(n)(buyCoffee(cc))
    val (coffees, charges) = purchases.unzip

    (coffees, charges.reduce((c1,c2) => c1.combine(c2)))
  }
}

class CreditCard

class Coffee {
  val price = 1.00
}

case class Charge(cc: CreditCard, amount: Double) {
  def combine(other: Charge): Charge =
    if(cc == other.cc) Charge(cc, amount + other.amount)
    else
      throw new Exception("Can't combine charge to different cards")
}

object Hi {
  def main(args: Array[String]) {
    val c = new Cafe
    val myCC = new CreditCard

    val value = c.buyCoffees(myCC, 20) match { case (_, Charge(_, amount)) => "Amount is " + amount}

    println(value)
  }
}
