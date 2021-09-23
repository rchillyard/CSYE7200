package edu.neu.coe.csye7200.csv

import com.phasmidsoftware.parse.RawParsers.WithHeaderRow.RawTableParser
import com.phasmidsoftware.table.Table
import com.phasmidsoftware.table.Table.parse
import com.phasmidsoftware.util.FP.safeResource
import edu.neu.coe.csye7200.csv.MovieDatabaseAnalyzer.getClass
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.Success

class MovieDatabaseAnalyzerTest extends AnyFlatSpec with Matchers {

  behavior of "parseResource"
  it should "get movie_metadata.csv" in {
    Table.parseResource("/movie_metadata.csv", getClass) should matchPattern { case Success(_) => }
  }

}
