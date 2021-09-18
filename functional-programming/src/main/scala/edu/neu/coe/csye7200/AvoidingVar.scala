package edu.neu.coe.csye7200

/**
  * @author scalaprof
  */
object AvoidingVar extends App {

  val data = List(0.1, 0.4, 0.2, 0.7, -0.1, 1.1, 0.5)

  {
    var sum = 0.0
    for (i <- data.indices; if i % 2 == 0) sum = sum + data(i)
    println(s"sum of even positions: $sum")
  }

  {
    import scala.annotation.tailrec
    @tailrec def loop(accumulator: Double, indices: List[Int]): Double = indices match {
      case Nil => accumulator
      case h :: t => loop(accumulator + data(h), t)
    }

    val sum = loop(0.0, (for (i <- data.indices; if i % 2 == 0) yield i).toList)
    println(s"sum of even positions: $sum")
  }

  printStatistics(data)
  println(getStatistics(data))

  def printStatistics(xs: Seq[Double]): Unit = {
    var c = 0
    var s = 0.0
    var v = 0.0
    for (x <- xs) {
      if (x >= 0 && x <= 1) {
        c += 1
        s += x
        v += x * x
      }
    }
    println(s"Mean: ${s / c}, Std. Dev: ${math.sqrt(v / c)}")
  }

  def getStatistics(xs: Seq[Double]) = {
    val y = xs filter { x => x >= 0 && x <= 1 }
    val r = y.foldLeft[(Int, Double, Double)]((0, 0, 0)) { case ((c, s, v), x) => (c + 1, s + x, v + x * x) }
    (r._2 / r._1, math.sqrt(r._3 / r._1))
  }

}
