package edu.neu.coe.csye7200.asstfc

import scala.util.{Failure, Success, Try}

/**
  * Solution without using case class. Requires implementing sequence
  */
object CsvColumn extends App {

  val xys: Seq[Try[Int]] = Seq("", "1", "2", "3").filter(_.nonEmpty).map(w => Try(w.toInt))

  // sequence the values of xys...
  val xsy: Try[Seq[Int]] = xys.foldLeft(Try(Seq[Int]())) {
    (_xsy, xy) => for (xs <- _xsy; x <- xy) yield x +: xs
  }


  xsy match {
    case Success(xs) => println(1.0 * xs.sum / xs.length)
    case Failure(x) => println(s"Exception: ${x.getLocalizedMessage}")
  }

}
