import scala.annotation.tailrec

  println("Welcome to the Factorial worksheet")

  def factorial(x: Int): Long = {
    @tailrec def inner(r: Long, i: Int): Long =
      if (i == 0) r else inner(r * i, i - 1)

    inner(1L, x)
  }

  val f5 = factorial(5)
  val f20 = factorial(20)