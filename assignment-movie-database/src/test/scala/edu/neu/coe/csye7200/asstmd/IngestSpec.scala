package edu.neu.coe.csye7200.asstmd

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.io.{Codec, Source}
import scala.util._

/**
  * Created by scalaprof on 9/13/16.
  */
class IngestSpec extends AnyFlatSpec with Matchers {

  behavior of "ingest"

  it should "work for Int" in {
    trait ParsableInt$ extends Parsable[Int] {
      def parse(w: String): Try[Int] = Try(w.toInt)
    }
    implicit object ParsableInt$ extends ParsableInt$
    val source = Source.fromChars(Array('x', '\n', '4', '2'))
    val ingester = new Ingest[Int]()
    val xys = ingester(source).toSeq
    // check that xys has exactly one element, consisting of Success(42) -- 10 points
    // TO BE IMPLEMENTED 
    // END
  }

  it should "work for movie database" in {
    implicit val codec: Codec = Codec("UTF-8")
    // NOTE that you expect to see a number of exceptions thrown. That's OK. We expect that some lines will not parse correctly.
    val msy = Using(Source.fromResource("movie_metadata.csv")){
      source =>
        val ingester = new Ingest[Movie]()
        val mys = for (my <- ingester(source).toList) yield my.recoverWith {
          case e: ParseException => System.err.println(e); my
        }
        for {
          my <- mys
          m <- my.toOption if m.production.country == "New Zealand"
        } yield m
    }
    msy match {
      case Success(ms) =>
        ms.size shouldBe 4
        ms foreach println
      case Failure(x) =>
        fail(x)
    }
  }
}