package edu.neu.coe.csye7200.csv

import org.apache.spark.sql.Dataset
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.Try

class MovieDatabaseAnalyzerTest extends AnyFlatSpec with Matchers {

  behavior of "parseResource"
  it should "get movie_metadata.csv" in {

    val mdy: Try[Dataset[Movie]] = MovieDatabaseAnalyzer("/movie_metadata.csv").dy
    mdy.isSuccess shouldBe true
    mdy foreach {
      d =>
        d.count() shouldBe 1567
        d.show(10)
    }

  }

}
