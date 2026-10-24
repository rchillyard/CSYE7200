//

val list: Seq[String] = List("0", "1", "2", "3")
val f: Int => String = list(_)
val g: Int => String = list

f(1)
g(2)
