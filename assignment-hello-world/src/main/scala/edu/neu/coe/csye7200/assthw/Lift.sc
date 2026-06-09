// Problem: what if we have a function of the form (A, B) => C
// but we don't actually have an A or a B, but have an Option[A] and an Option[B]?

val intMap = Map(1 -> 2, 2 -> 3)
val strMap = Map(1 -> "Hello World", 2 -> "Goodbye")

val xo: Option[Int] = intMap.get(1)
val so: Option[String] = strMap.get(2)

val repeat: Int => String => String = (n: Int) => (s: String) => s * n

def lift[A, B, C](f: (A, B) => C)(ao: Option[A], bo: Option[B]): Option[C] =
  for {
    a <- ao
    b <- bo
  } yield f(a, b)

// TODO fix this by redefining `repeat`
val repeatLifted: (Option[Int], Option[String]) => Option[String] = lift(repeat.tupled)

repeatLifted(xo, so)