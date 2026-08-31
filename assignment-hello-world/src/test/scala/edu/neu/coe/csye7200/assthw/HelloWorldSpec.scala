package edu.neu.coe.csye7200.assthw

import edu.neu.coe.csye7200.CancelOnNotImplemented
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.io.Source

/**
  * Created by scalaprof on 8/25/16.
  */
class HelloWorldSpec extends AnyFlatSpec with Matchers with CancelOnNotImplemented {
  behavior of "HelloWorld"
  it should "get the correct greeting" in {
    HelloWorld.greeting shouldBe "Hello World!"
  }

  behavior of "Ingest"
  it should "find the correct number of movies" in {
    given Ingestible[Movie] with {
      def fromStrings(ws: Seq[String]): Movie = Movie.apply(ws)
    }

    val ingester = new Ingest[Movie]()
    val source = Source.fromResource("movie_metadata_5000.csv")
    (for (m <- ingester(source); if m.properties.head == " Black and White") yield m)  should have size 209
    source.close()
  }

}