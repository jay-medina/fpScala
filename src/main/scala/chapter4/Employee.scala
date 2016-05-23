package chapter4


case class Employee(name: String, department: String)


object Runner {

  def lookupByName(name: String): Option[Employee] = ???

  val joeDepartment: Option[String] = lookupByName("Joe").map(_.department)

}
