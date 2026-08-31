package edu.neu.coe.csye7200.asstmd

import com.phasmidsoftware.tableparser.core.util.FP
import edu.neu.coe.csye7200.CancelOnNotImplemented
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.io.{Codec, Source}
import scala.util.*

/**
 * Created by scalaprof on 9/13/16.
 */
class IngestSpec extends AnyFlatSpec with Matchers with CancelOnNotImplemented {

  behavior of "ingest"

  it should "work for Int" in {
    given Parsable[Int] with {
      def parse(w: String): Try[Int] = Try(w.toInt)
    }

    val source = Source.fromChars(Array('x', '\n', '4', '2'))
    val ingester = new Ingest[Int]()
    val xys = ingester(source).toSeq
    // check that xys has exactly one element, consisting of Success(42) -- 10 points
    // TO BE IMPLEMENTED 
    // END
  }

  it should "work for movie database" in {
    given codec: Codec = Codec("UTF-8")

    // NOTE that you expect to see a number of exceptions thrown. That's OK. We expect that some lines will not parse correctly.
    val msy = Using(Source.fromResource("movie_metadata.csv")) {
      source =>
        val ingester = new Ingest[Movie]()
        val msy: Try[Seq[Movie]] = FP.sequenceForgivingWith(ingester(source).toList) {
          case e: ParseException => System.err.println(e); Success(None)
          case scala.util.control.NonFatal(e) => Failure(e)
        }
        tryOrCancelWith(msy) {
          ms =>
            val ww = ms filter { m => m.production.country == "New Zealand" }
            ww foreach println
            ww should have size 4
        }
    }
  }
}