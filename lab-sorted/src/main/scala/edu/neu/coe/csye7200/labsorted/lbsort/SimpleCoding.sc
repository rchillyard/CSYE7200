import scala.language.postfixOps

val directory = Map("Alice" -> 3, "Bob" -> 1, "Carol" -> 2)
val names = directory.keys
val phones = directory.values

def matchUp(name: String, phone: Int): Option[(String, Int)] = for {
  n <- names.find(_==name)
  p <- phones.find(_==phone)
  d <- directory.get(name) if d == phone
} yield (n, p)

matchUp("Alice", 3)

val xs = LazyList from 1 take 10 foreach println

val xo: Option[Int] = Some(1)
xo.fold(0)(_ * 2)

def foldLeft[A,B](ao: Option[A])(b: B)(f: (B,A) => B): B = ao match {
  case None => b
  case Some(a) => f(b, a)
}