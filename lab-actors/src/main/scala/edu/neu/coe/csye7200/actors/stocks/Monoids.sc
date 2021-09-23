trait Monoid[A] {
  def op(a1: A, a2: A): A
  def identity: A
}

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val identity = ""
  }
  def intMonoid[A] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2
    val identity = 0
  }

trait List[A] {
  def foldLeft[B](z: B)(f: (B, A) => B): B
}

case class Cons[A](h: A, t: List[A]) extends List[A] {
  def foldLeft[B](z: B)(f: (B, A) => B): B = t.foldLeft(f(z,h))(f)
}

case object Nil extends List[Nothing] {
  def foldLeft[B](z: B)(f: (B, Nothing) => B): B = z
}

object List {
  def apply[A](xs: Seq[A]): List[A] = xs match {
    case Seq() => Nil.asInstanceOf[List[A]]
    case h :: t => Cons(h, apply(t))
  }

  def sum[A: Numeric](xs: List[A]): A = {
    val an = implicitly[Numeric[A]]
    xs.foldLeft(an.zero)(an.plus)
  }
}

val xs = List(Seq(1,2,3))

List.sum(xs)
