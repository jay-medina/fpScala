package chapter1

class Cafe {

  def buyCoffee(cc: CreditCard): Coffee = {
    val cup = new Coffee
    cc.charge(cup.price)

    cup
  }
}

class CreditCard {
  def charge(price: Double) = println("charged CC")
}

class Coffee {
  val price = 1.00
}
