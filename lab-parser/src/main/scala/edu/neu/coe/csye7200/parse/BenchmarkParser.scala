package edu.neu.coe.csye7200.parse

import scala.io.Source
import scala.util.Try
import scala.util.parsing.combinator.JavaTokenParsers

object BenchmarkParser extends App {

  val p: BenchmarkParser = new BenchmarkParser()
  private val inFilename = args.headOption.getOrElse("/Users/rhillyard/IdeaProjects/INFO6205/savedLogs/benchmarkClaudeInstr.txt")
  val source = Source.fromFile(inFilename)
  if (source != null) println(s"Opening $inFilename")
  else throw new RuntimeException(s"$inFilename does not exist")
  val ws: Iterator[String] = for {line <- source.getLines() if line.contains("@")} yield line
  val (ss, ls): (Iterator[String], Iterator[String]) = ws.partition(w => w.contains("StatPack"))
  val (q1, q2) = p.getLogEntries(ls)
  val xs = p.getInstrumentation(ss)

  import java.io._

  private val outFilename = "output.csv"
  val pw = new PrintWriter(new File(outFilename))
  pw.write("N\tKind\tAlgorithm\tvalue\n")
  pw.write((q1 map (_.render)) mkString "\n")
  pw.write("\n")
  pw.write((q2 map (_.render)) mkString "\n")
  pw.write("\n")
  pw.write((xs map (_.render)) mkString "\n")
  pw.write("\n")
  pw.close()
  println(s"$outFilename written")
}

class BenchmarkParser extends JavaTokenParsers {
  def getLogEntries(ls: Iterator[String]): (Seq[LogEntry], Seq[LogEntry]) = {
    val lys: Iterator[Try[LogEntry]] = ls map (w => parseBenchmarkLogEntry(w))
    val lsy: Try[Seq[LogEntry]] = lys.foldLeft(Try(Seq[LogEntry]())) {
      (xsy, xy) => for (xs <- xsy; x <- xy) yield xs :+ x
    }
    lsy match {
      case scala.util.Success(xs) => xs.partition{ e: LogEntry => e.kind == "Raw" }
      case scala.util.Failure(x) => throw x
    }
  }

  def getInstrumentation(ss: Iterator[String]): Seq[Instrumentation] = {
    val z = ss.toSeq map (w => w.replaceAll(",", ""))
    val xys: scala.Seq[Try[Instrumentation]] = z map (w => parseBenchmarkInstrumentation(w))
    val xsy: Try[Seq[Instrumentation]] = xys.foldLeft(Try(Seq[Instrumentation]())) {
      (xsy, xy) => for (xs <- xsy; x <- xy) yield xs :+ x
    }
    xsy match {
      case scala.util.Success(xs: Seq[Instrumentation]) => xs
      case scala.util.Failure(x) => throw x
      case x => throw new Exception(s"getInstrumentation: unable to parse: $x")
    }
  }

  def parseBenchmarkLogEntry(s: String): Try[LogEntry] = parseAll(logEntry, s) match {
    case this.Success(x, _) => scala.util.Success(x)
    case this.Failure(m, _) => scala.util.Failure(ParserException(s"parseBenchmarkLogEntry: unable to parse '$s' because: $m"))
    case this.Error(m, _) => scala.util.Failure(new Exception(m))
  }

  def parseBenchmarkInstrumentation(s: String): Try[Instrumentation] = parseAll(instrumentation, s) match {
    case this.Success(x, _) => scala.util.Success(x)
    case this.Failure(m, _) => scala.util.Failure(ParserException(s"parseBenchmarkInstrumentation: unable to parse '$s' because: $m"))
    case this.Error(m, _) => scala.util.Failure(new Exception(m))
  }

  def logEntry: Parser[LogEntry] = prefix ~ (dash ~> wholeNumber) ~ (at ~> description) ~ (colon ~> kind) ~ (colon ~> floatingPointNumber) ^^ {
    case _ ~ x ~ w ~ k ~ t => LogEntry(x.toInt, w, k, t.toDouble)
  }

  def instrumentation: Parser[Instrumentation] = prefix ~> (dash ~> wholeNumber) ~ (at ~> description <~ ": StatPack {") ~ statistics <~ "}" ^^ {
    case x ~ w ~ s => Instrumentation(x.toInt, w, s)
  }

  def statistics: Parser[Statistics] = repsep(statsMean, semiColon) ^^ (ss => Statistics((for ((key, xo) <- ss; x <- xo) yield key -> x).toMap))

  def statsMean: Parser[(String, Option[Double])] = (ident <~ colon) ~ stats ^^ {
    case key ~ zs =>
      key -> (for ((w, u) <- zs; x <- Try(u.toDouble).toOption) yield (w, x)).toMap.get("mean")
  }

  def stats: Parser[List[(String, String)]] = (unset | repsep(statMode, semiColon) | failure("stats")) ^^ {
    case ms: List[String ~ String] @unchecked => for {a ~ b <- ms} yield a -> b
    case _ => Nil
  }

  def statMode: Parser[String ~ String] = mode ~ (equal ~> decimalNumber)

  def mode: Parser[String] = "normalized" | "stdDev" | "n" | "mean" | failure("unknown mode")

  def unset: Parser[String] = "<unset>"

  def description: Parser[String] = rep(term) ~ opt(wholeNumber) ~ opt(cutoff) ^^ { case xs ~ no ~ _ => xs.mkString(" ") + formatOption(no) }

  def term: Parser[String] = "(" ~> ident <~ ")" | ident <~ equal <~ wholeNumber | ident | failure("badly formed term")

  private def cutoff: Parser[String] = "with cutoff=" ~> wholeNumber

  private def formatOption(no: Option[String]) = no match {
    case Some(n) => s" $n"
    case None => ""
  }

  def kind: Parser[String] = ident <~ "time per run " <~ "{" <~ formula <~ "}"

  def formula: Parser[String] = "mSec" | "n log n" | "n^2" | "n^(4/3)" | "n" | failure("bad formula")

  def prefix: Parser[String] = date ~ time ~ ident ~ ident ^^ { case d ~ t ~ x ~ z => s"$d $t $x $z" }

  def date: Parser[String] = """\d{4}-\d{2}-\d{2}""".r

  def time: Parser[String] = """\d{2}:\d{2}:\d{2}\.\d{3}""".r

  def semiColon: Parser[String] = """;"""

  private def at = """@"""

  private def dash = """-"""

  private def equal = """="""

  private def colon = """:"""
}

case class LogEntry(n: Int, name: String, kind: String, time: Double) {
  def render: String = s"$n\t$kind\t$name\t$time"
}

case class Instrumentation(n: Int, name: String, stats: Statistics) {
  //  def addEntry(key: String, count: Double): Instrumentation = {
//    val entry: (String, Double) = key -> count
//    copy(stats = stats + entry)
//  }
  def render: String = s"$n\t$name\t$stats"
}

case class Statistics(stats: Map[String, Double]) {
  override def toString: String = {
    val result: StringBuilder = new StringBuilder()
    for ((k, v) <- stats) result.append("\t").append(k).append(":\t").append(v)
    result.toString()
  }
}

case class StatsEntries(n: Int, name: String, stats: Map[String, Double]) {
  def addEntry(key: String, count: Double): StatsEntries = {
    val entry: (String, Double) = key -> count
    copy(stats = stats + entry)
  }

  def render: String = s"$n\t$name\t$stats"
}

object StatsEntries {
  def apply(n: Int, name: String, stats: List[(String, Option[Double])]): StatsEntries = apply(n, name, for ((key, xo) <- stats.toMap; x <- xo) yield (key, x))
}