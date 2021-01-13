/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200.cache

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util._

case class Portfolio(positions: Seq[Position]) {

  def value(cache: Cache[String, Double]): Future[Double] = {
    val xfs = for (p <- positions) yield for (v <- p.value(cache)) yield v
    // CONSIDER using traverse
    for (xs <- Future.sequence(xfs)) yield xs.sum
  }

}

case class Position(symbol: String, quantity: Double) {
  def value(cache: Cache[String, Double]): Future[Double] = for (v <- cache(symbol)) yield v * quantity
}

object Portfolio {
  private def sequence[X](xys: Seq[Try[X]]): Try[Seq[X]] = xys.foldLeft(Try(Seq[X]())) {
    (xsy, xy) => for (xs <- xsy; x <- xy) yield xs :+ x
  }

  // CONSIDER using traverse
  def parse(ws: Seq[String]): Try[Portfolio] = sequence(ws map Position.parse) map Portfolio.apply
}

object Position {
  private val positionR = """(\w+)\s+(\d+(\.\d+))""".r

  def parse(w: String): Try[Position] = w match {
    case positionR(a, b, _) => Try(Position(a, b.toDouble))
    case _ => Failure(new Exception(s"cannot parse $w as a Position"))
  }

  def value(cache: Cache[String, Double])(w: String): Future[Double] = flatten(for (p <- parse(w)) yield p.value(cache))

  private def flatten[X](xfy: Try[Future[X]]): Future[X] =
    xfy match {
      case Success(xf) => xf
      case Failure(e) => Future.failed(e)
    }

}
