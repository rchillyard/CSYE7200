package edu.neu.coe.csye7200.fp.cache

import edu.neu.coe.csye7200.MonadOps
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.io.Source
import scala.util.Try

object StockReader {

  def getPrice(symbol: String)(implicit resource: String): Future[Double] = MonadOps.flatten(Future(readPrice(symbol)))

  private def readPrice(symbol: String)(implicit resource: String): Try[Double] = {
    def readMatchingPrices = {
      val ws = Source.fromInputStream(getClass.getResourceAsStream(resource)).getLines()
      val was = for (w <- ws.toSeq) yield w.split("""\s+""")
      for (wa <- was; if wa.length > 1; x = wa.head; if x == symbol) yield wa.last
    }

    val wy = MonadOps.flatten(Try(readMatchingPrices.headOption), new Exception(s"no entry matching $symbol"))
    for (w <- wy; x <- Try(w.toDouble)) yield x
  }
}
