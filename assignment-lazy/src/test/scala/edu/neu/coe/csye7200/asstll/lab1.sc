def dropTwo(xs: Seq[Int]): Seq[Int] = xs match {
  case Nil => Nil
  case _ :: _ :: tail => tail
}

def drop(xs: Seq[Int])(n: Int): Seq[Int] = xs -> n match {
  case (Nil, _) => Nil
  case (ys, 0) => ys
  case (_ :: tail, m) if m > 0 => drop(tail)(m - 1)
  case _ => throw new RuntimeException("logic error")
}

val tuple = (1, Math.PI)
val tuple2 = 1 -> Math.PI
val tuple3 = (1, Math.PI, "Hello World!")
val tuple4 = 1 -> Math.PI -> "Hello World!"

drop(List(1,2,3,4))(2) // should yield 3, 4
drop(Nil)(2) // should yield 3, 4
drop(List(1,2,3,4))(0) // should yield 1, 2, 3, 4
//drop(List(1,2,3,4))(-1)

// You are going to write a functional logger
def log[X](x: X, messageFunction: X => String): X = {
  val w: String = messageFunction(x)
  println(w)
  x
}

def xToString[Any](x: Any): String = s"result: $x"
log(Math.PI*Math.PI, xToString _)

val logSimple: (Double => Double) = log(_, xToString)
val y = logSimple(Math.PI*Math.PI)