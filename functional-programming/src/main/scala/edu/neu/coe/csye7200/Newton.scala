package edu.neu.coe.csye7200

import scala.annotation.tailrec
import scala.util._

/**
  * Created by scalaprof on 1/20/17.
  */
case class Newton(f: Double => Double, dfbydx: Double => Double) {

  private def step(x: Double, y: Double) = x - y / dfbydx(x)

  def solve(tries: Int, threshold: Double, initial: Double): Try[Double] = {
    @tailrec def inner(r: Double, n: Int): Try[Double] = {
      val y = f(r)
      if (math.abs(y) < threshold) Success(r)
      else if (n == 0) Failure(new Exception("failed to converge"))
      else inner(step(r, y), n - 1)
    }

    inner(initial, tries)
  }

}

object Newton extends App {
  val newton = Newton({ x => math.cos(x) - x }, { x => -math.sin(x) - 1 })
  newton.solve(10, 1E-10, 1.0) match {
    case Success(x) => println(s"the solution to math.cos(x) - x is $x")
    case Failure(t) => System.err.println(t.getLocalizedMessage)
  }
}
