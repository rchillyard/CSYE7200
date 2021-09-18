def reverse(xs: List[Int]): List[Int] =
  xs match {
    case h :: t => reverse(t) :+ h
    case Nil => Nil
  }

val xs: List[Int] = List(1, 2, 3)
reverse(xs)

for (x  <- xs ) println(x)

def lift[A,B](f: A => B): List[A] => List[B] = a => a map f

val f: Int => String = _.toString

val fLifted = lift(f)
val z1 = fLifted(xs)
println(s"z1: $z1")

def lift2[A,B,C](f:(A,B)=>C):List[(A,B)]=>List[C] = _ map f.tupled

val f2: (String, Any) => String = (w,x) => s"$w->$x"

val f2Lifted: List[(String, Any)] => List[String] = lift2(f2)

val z2 = f2Lifted(Map("Z" -> 1, "q" -> 2).toList)

println(s"z2: $z2")


case class Complex(r: Double, i: Double)

val g: (Double, Double) => Complex = Complex(_, _)

val n: Int = ???
val padding = " " * n