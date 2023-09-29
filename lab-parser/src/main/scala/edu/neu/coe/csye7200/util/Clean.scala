package edu.neu.coe.csye7200.util

import edu.neu.coe.csye7200.util.FileCleaner.{noleak, noleakFlat, sequence}
import java.io.{BufferedWriter, File, FileWriter, Writer}
import java.nio.file.FileSystems.getDefault
import java.nio.file.Files.walk
import java.nio.file.Path
import scala.io.Source
import scala.util.Try
import scala.util.control.NonFatal
import scala.util.parsing.combinator.JavaTokenParsers

class FileCleaner(solution: String, stub: String, terminator: String) extends JavaTokenParsers {

  println(s"FileCleaner created with solution='$solution', stub='$stub', terminator='$terminator'")

  def clean(inputFile: String, outputFile: String)(implicit logger: Logger): Try[Int] =
    clean(new File(inputFile), new File(outputFile))

  def cleanTree(sourcePath: String, destPath: String, toInclude: Path => Boolean, toExclude: Path => Boolean)(implicit logger: Logger): Try[Boolean] = {
    import scala.collection.JavaConverters._
    val sourceDir = getDefault.getPath(sourcePath)
    val destDir = getDefault.getPath(destPath)
    println(s"cleanTree: $sourceDir $destDir")
    val sources = walk(sourceDir).iterator().asScala
    val files = sources.filterNot(toExclude).filter(toInclude)
    val xsy: Try[Seq[Int]] = sequence(for {
      (s, d) <- files map (p => p -> destDir.resolve(sourceDir.relativize(p)))
      _ = ensureCanWriteFile(d.toAbsolutePath.toFile)
    } yield clean(s.toFile, d.toAbsolutePath.toFile)).recover {
      case NonFatal(x) =>
        logger.logError(x.toString)
        Nil
      case x => throw x
    }
    xsy map (xs => xs.forall(_ > 0))
  }

  def clean(inputFile: File, outputFile: File)(implicit logger: Logger): Try[Int] =
    noleakFlat(Try(new BufferedWriter(new FileWriter(outputFile)))) {
      w =>
        noleak(Try(Source.fromFile(inputFile))) { s =>
          println(s"clean $inputFile $outputFile")
          clean(s, w)
        }
    }

  def ensureCanWriteFile(d: File): Unit =
    if (!d.canWrite) {
      val dir = d.getParentFile
      dir.mkdirs()
    }


  override def skipWhitespace: Boolean = false

  val TOBEIMPLEMENTED = "TO BE IMPLEMENTED"

  case class ParsedLine(n: Int, prefix: String, maybeMaybeString: Option[Option[String]], suffix: String) {
    def render(isStub: Boolean): String =
      (maybeMaybeString, suffix) match {
        case (Some(Some(`solution`)), b) => s"$prefix// " + TOBEIMPLEMENTED + s" $b"
        case (Some(Some(a)), b) => s"$prefix// $a$b"
        case (Some(None), b) if !isStub => s"$prefix//$b"
        case (_, b) => s"$prefix$b"
      }
  }

  def line(x: Int): Parser[ParsedLine] = prefix ~ opt(slashes ~> opt(prefix ~> keyword)) ~ anything ^^ {
    case prefix ~ Some(maybeKeyword) ~ comment => ParsedLine(x, prefix, Some(maybeKeyword), comment)
    case prefix ~ None ~ code => ParsedLine(x, prefix, None, code)
  }

  val DEFAULTSTUB = "???"

  def clean(source: Source, destination: Writer)(implicit logger: Logger): Int = {
    // CONSIDER avoiding vars
    var output = true
    var isStub = false

    def process(commentedLine: ParsedLine): String = {
      var transition = false
      var defaultStub = false
      commentedLine.maybeMaybeString match {
        case Some(Some(`solution`)) =>
          logger.logInfo("Solution")
          transition = true
          output = false
        case Some(Some(`stub`)) =>
          output = true
          isStub = true
        case Some(Some(`terminator`)) =>
          if (!isStub) {
            output = true
            defaultStub = true
          } else
            isStub = false
        case _ =>
      }
      if (defaultStub) DEFAULTSTUB else if (transition || output) commentedLine.render(isStub) else ""
    }

    val result = FileCleaner.sequence(for (l <- source.getLines().zipWithIndex) yield parseLine(l)) match {
      case scala.util.Success(cs) =>
        val str = (for (c <- cs) yield process(c)).mkString("\n")
        destination.append(str)
        str.length
      case scala.util.Failure(x) => throw x
    }
    if (isStub || !output)
      logger.logWarning(s"Solution not terminated?: end of source file")
    result
  }

  def parseLine(wl: (String, Int)): Try[ParsedLine] = this.parseAll(line(wl._2), wl._1) match {
    case this.Success(result, _) => scala.util.Success(result)
    case this.Failure(msg, next) => scala.util.Failure(CleanParseException(msg + "@" + next.toString))
    case this.Error(msg, next) => scala.util.Failure(CleanParseException(msg + "@" + next.toString))
  }

  private val prefix = """\s*""".r

  private def anything = """.*""".r

  def slashes: Parser[String] = """//""".r

  private def keyword: Parser[String] = solution | stub | terminator | failure("not a keyword")
}

object FileCleaner {
  def sequence[X](xys: Iterator[Try[X]]): Try[Seq[X]] = xys.foldLeft(Try(Seq[X]())) {
    (xsy, xy) => for (xs <- xsy; x <- xy) yield xs :+ x
  }

  /**
   * TODO Replace with Using, or variation, when we upgrade to 2.13
   */
  def noleak[R <: AutoCloseable, T](resource: => Try[R])(f: R => T): Try[T] = {
    lazy val r = resource
    val result = r map f
    r foreach (_.close())
    result
  }

  /**
   * TODO Replace with Using, or variation, when we upgrade to 2.13
   */
  def noleakFlat[R <: AutoCloseable, T](resource: => Try[R])(f: R => Try[T]): Try[T] = {
    lazy val r = resource
    val result = r flatMap f
    r foreach (_.close())
    result
  }

}
object CleanTree extends App {

  import java.nio.file.Files

  implicit val logger: Logger = Logger(getClass)

  def merge(x: (String, String)): String = if (x._1.nonEmpty) x._1 else x._2

  val arguments = args.toList zipAll(List("", "", "SOLUTION", "STUB", "END"), "", "")
  val List(sourcePath, destPath, solution, stub, terminator) = arguments map merge
  if (sourcePath.isEmpty || destPath.isEmpty) System.err.println("You must provide root paths for source and destination (at least)")
  else {
    val cleaner = new FileCleaner(solution, stub, terminator)
    val toInclude: Path => Boolean = p => Files.isRegularFile(p) && (p.getFileName.toString.endsWith(".java") || p.getFileName.toString.endsWith(".scala"))
    val toExclude: Path => Boolean = p => p.getFileName.toString.startsWith(".")
    val result = cleaner.cleanTree(sourcePath, destPath, toInclude, toExclude).get
    if (result) println("CleanTree complete") else System.err.println("CleanTree: At least one destination file is empty")
  }
}

case class CleanParseException(str: String) extends Exception(str)