package edu.neu.coe.csye7200.parse

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BenchmarkParserSpec extends AnyFlatSpec with Matchers {

  behavior of "BenchmarkParser"

  val p = new BenchmarkParser()
  import p._

  private val prefix1 = "2024-05-28 18:14:18.241 INFO TimeLogger"
  private val quadratic = "n^2"
  private val subQuadratic = "n^(4/3)"

  private val logStrings = Seq(
    "2024-05-29 16:44:08.089 INFO  TimeLogger - 1000@Bucket sort: Raw time per run {mSec}:  .2295",
    "2024-05-29 16:44:08.090 INFO  TimeLogger - 1000@Bucket sort: Normalized time per run {n}:  229.4998"
  )

  private val statsStrings = Seq(
    "2024-05-29 17:51:47.982 INFO  InstrumentedComparatorHelper - 1000@Bucket sort: StatPack {hits: n=1; mean=13818; stdDev=1893; normalized=0.020; copies: n=1; mean=730000; normalized=1.031; inversions: <unset>; swaps: n=1; mean=2955; stdDev=474; normalized=0.004; fixes: <unset>; compares: n=1; mean=3953; stdDev=473; normalized=0.006}",
    "2024-05-29 17:51:52.433 INFO  InstrumentedComparableHelper - 1000@LSD String Sort: StatPack {hits: n=1; mean=100000; normalized=0.646; copies: n=1; mean=40000; normalized=0.258; inversions: <unset>; swaps: n=1; mean=0; normalized=0.000; fixes: <unset>; compares: n=1; mean=0; normalized=0.000}"
  )

  it should "getLogEntries" in {
    val (q1, q2) = p.getLogEntries(logStrings.iterator)
    q1 should have size 1
    q2 should have size 1
  }

  it should "logEntry" in {
    val entry = prefix1 + "- 1000@QuickSort dual pivot: Normalized time per run {n log n}:  17.5553"
    val x: p.ParseResult[LogEntry] = p.parseAll(p.logEntry, entry)
    x.successful shouldBe true
    x.get shouldBe LogEntry(1000, "QuickSort dual pivot", "Normalized", 17.5553)
  }
  it should "instrumentation" in {
    val prefix2 = "2024-05-29 15:08:13.808 INFO  InstrumentedComparatorHelper"
    val entry = prefix2 + "- 1000@Bucket sort : StatPack {hits: n=1; mean=13813; normalized=1.0}"
    val x: p.ParseResult[Instrumentation] = p.parseAll(p.instrumentation, entry)
    x.successful shouldBe true
    x.get shouldBe Instrumentation(1000, "Bucket sort", Statistics(Map("hits" -> 13813.0)))
  }
  it should "statistics 1" in {
    val entry = "hits: n=1; mean=13813; normalized=1.0"
    val x: p.ParseResult[Statistics] = p.parseAll(p.statistics, entry)
    x.successful shouldBe true
    x.get shouldBe Statistics(Map("hits" -> 13813.0))
  }
  it should "statistics 2" in {
    val entry = "hits: n=36138; mean=8217; stdDev=486; normalized=1.158; lookups: n=36138; mean=9264; stdDev=973; normalized=1.305; copies: n=36138; mean=2048; normalized=0.289; inversions: <unset>; swaps: n=36138; mean=3098; stdDev=487; normalized=0.436; fixes: <unset>; compares: n=36138; mean=4120; stdDev=486; normalized=0.580"
    val x: p.ParseResult[Statistics] = p.parseAll(p.statistics, entry)
    x.successful shouldBe true
    x.get shouldBe Statistics(Map("copies" -> 2048.0, "swaps" -> 3098.0, "hits" -> 8217.0, "lookups" -> 9264.0, "compares" -> 4120.0))
    x.get.toString shouldBe "\tcopies:\t2048.0\tswaps:\t3098.0\thits:\t8217.0\tlookups:\t9264.0\tcompares:\t4120.0"
  }
  it should "statsMean 1" in {
    val entry = "hits: n=1; mean=13813; normalized=1.0; stdDev=1.0"
    val x = p.parseAll(p.statsMean, entry)
    x.successful shouldBe true
    x.get shouldBe "hits" -> Some(13813.0)
  }
  it should "statsMean 2" in {
    val entry = "hits: <unset>"
    val x = p.parseAll(p.statsMean, entry)
    x.successful shouldBe true
    x.get shouldBe "hits" -> None
  }
  it should "stats 1" in {
    val entry = "mean=13813"
    val x: p.ParseResult[List[(String, String)]] = p.parseAll(p.stats, entry)
    x.successful shouldBe true
    val expected = List("mean" -> "13813")
    x.get shouldBe expected
  }
  it should "stats 2" in {
    val entry = "n=1; mean=13813; normalized=1.0"
    val x: p.ParseResult[List[(String, String)]] = p.parseAll(p.stats, entry)
    x.successful shouldBe true
    val expected = List("n" -> "1", "mean" -> "13813", "normalized" -> "1.0")
    x.get shouldBe expected
  }
  it should "stats 3" in {
    val entry = "<unset>"
    val x: p.ParseResult[List[(String, String)]] = p.parseAll(p.stats, entry)
    x.successful shouldBe true
    val expected = Nil
    x.get shouldBe expected
  }
  it should "statMode 1" in {
    val entry = "mean=13813"
    val x: p.ParseResult[String ~ String] = p.parseAll(p.statMode, entry)
    x.successful shouldBe true
  }
  it should "statMode 2" in {
    val entry = "n=1; mean=13813; mean=1.0; normalized=1.0; mean=1.0; mean=1.0"
    val x: p.ParseResult[List[String ~ String]] = p.parseAll(p.repsep(p.statMode, p.semiColon), entry)
    x.successful shouldBe true
  }
  it should "mode 1" in {
    val x: p.ParseResult[String] = p.parseAll(p.mode, "stdDev")
    x.successful shouldBe true
  }
  it should "mode 2" in {
    val x: p.ParseResult[String] = p.parseAll(p.mode, "n")
    x.successful shouldBe true
  }
  it should "mode 3" in {
    val x: p.ParseResult[String] = p.parseAll(p.mode, "mean")
    x.successful shouldBe true
  }
  it should "mode 4" in {
    val x: p.ParseResult[String] = p.parseAll(p.mode, "normalized")
    x.successful shouldBe true
  }

  it should "kind1" in {
    val x: p.ParseResult[String] = p.parseAll(p.kind, " Raw time per run {mSec}")
    x.successful shouldBe true
    x.get shouldBe "Raw"
  }
  it should "kind2" in {
    val x: p.ParseResult[String] = p.parseAll(p.kind, " Normalized time per run {n log n}")
    x.successful shouldBe true
    x.get shouldBe "Normalized"
  }
  it should "kind3" in {
    val x: p.ParseResult[String] = p.parseAll(p.kind, " Normalized time per run {" + subQuadratic + "}")
    x.successful shouldBe true
    x.get shouldBe "Normalized"
  }
  it should "kind4" in {
    val x: p.ParseResult[String] = p.parseAll(p.kind, " Normalized time per run {" + quadratic + "}")
    x.successful shouldBe true
    x.get shouldBe "Normalized"
  }

  it should "description1" in {
    val quicksort = "QuickSort dual pivot"
    val x: p.ParseResult[String] = p.parseAll(p.description, quicksort)
    x.successful shouldBe true
    x.get shouldBe quicksort
  }

  it should "description2" in {
    val shellsort = "Shell sort in mode 4"
    val x: p.ParseResult[String] = p.parseAll(p.description, shellsort)
    x.successful shouldBe true
    x.get shouldBe shellsort
  }

  it should "description3" in {
    val shellsort = "string sort ASCII (Ext) with cutoff=256"
    val x: p.ParseResult[String] = p.parseAll(p.description, shellsort)
    x.successful shouldBe true
    x.get shouldBe "string sort ASCII Ext with cutoff"
  }

  it should "prefix1" in {
    val x: p.ParseResult[String] = p.parseAll(p.prefix, prefix1)
    x.successful shouldBe true
    x.get shouldBe prefix1
  }

  it should "date" in {
    val x: p.ParseResult[String] = p.parseAll(p.date, "2024-05-28")
    x.successful shouldBe true
    x.get shouldBe "2024-05-28"
  }

  it should "time" in {
    val x: p.ParseResult[String] = p.parseAll(p.time, "18:14:18.241")
    x.successful shouldBe true
    x.get shouldBe "18:14:18.241"
  }
  it should "formula" in {
    val x: p.ParseResult[String] = p.parseAll(p.formula, quadratic)
    x.successful shouldBe true
    x.get shouldBe quadratic
  }
  it should "anything" in {
    val x: p.ParseResult[String] = p.parseAll(p.formula, subQuadratic)
    x.successful shouldBe true
    x.get shouldBe subQuadratic
  }

  it should "parseBenchmarkLogEntry" in {
    val entry = prefix1 + "- 1000@QuickSort dual pivot: Normalized time per run {n log n}:  17.5553"
    p.parseBenchmarkLogEntry(entry) should matchPattern { case scala.util.Success(_) => }
  }

}