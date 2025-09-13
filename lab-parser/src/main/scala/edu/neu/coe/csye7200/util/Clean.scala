package edu.neu.coe.csye7200.util

import edu.neu.coe.csye7200.util.FileCleaner._
import java.io.{BufferedWriter, File, FileWriter, Writer}
import java.nio.file.FileSystems.getDefault
import java.nio.file.Files.walk
import java.nio.file.Path
import scala.collection.immutable
import scala.io.Source
import scala.util.Try
import scala.util.control.NonFatal
import scala.util.matching.Regex
import scala.util.parsing.combinator.JavaTokenParsers

/**
 * The `FileCleaner` class represents a tool designed to clean, process, and transform
 * source code or configuration files based on specific markers (`solution`, `stub`,
 * `terminator`). It reads and writes files, processes directory trees, and provides
 * customizable behavior for handling markers.
 *
 * It extends the `JavaTokenParsers` trait and uses parser combinators for in-depth
 * file content analysis.
 *
 * @constructor Creates a new instance of `FileCleaner` with specified markers for
 *              processing files.
 * @param solution   The marker identifying the start of solution sections in a file.
 * @param stub       The marker identifying stub sections in a file.
 * @param terminator The marker denoting the end of a solution/stub section.
 */
class FileCleaner(solution: String, stub: String, terminator: String) extends JavaTokenParsers {

  logger.logInfo(s"FileCleaner created with solution='$solution', stub='$stub', terminator='$terminator'")

  /**
   * Cleans the content of the specified input file, processes specific markers using a default stub string,
   * and writes the transformed output to the specified output file. Logs relevant information during execution.
   *
   * @param inputFile  the path to the input file to be cleaned
   * @param outputFile the path to the output file where the cleaned content will be written
   * @param logger     an implicit logger for logging information, warnings, or errors
   * @return a Try containing the total length of the cleaned content written to the output file, or a Failure if an error occurs
   */
  def clean(inputFile: String, outputFile: String)(implicit logger: Logger): Try[Int] =
    clean(new File(inputFile), new File(outputFile), getDefaultStub(inputFile))

  /**
   * Cleans a directory tree by filtering files based on include and exclude criteria,
   * and performs operations on the filtered results. The method attempts to clean
   * and optionally write the processed content to a destination directory while ensuring
   * necessary write access and logging relevant information.
   *
   * @param sourcePath the path to the source directory to be processed
   * @param destPath   the path to the destination directory where the processed files are written
   * @param toInclude  a function to filter files that should be included for processing
   * @param toExclude  a function to filter files that should be excluded from processing
   * @param write      a flag indicating whether to write the processed content to the destination directory; defaults to true
   * @param logger     an implicit logger for logging debug, info, warning, or error messages during processing
   * @return a Try containing true if all operations succeeded, or false if any operation failed, while capturing any errors
   */
  def cleanTree(sourcePath: String, destPath: String, toInclude: Path => Boolean, toExclude: Path => Boolean, write: Boolean = true)(implicit logger: Logger): Try[Boolean] = {
    val sourceDir = getDefault.getPath(sourcePath)
    val destDir = getDefault.getPath(destPath)
    logger.logInfo(s"cleanTree: $sourceDir $destDir")
    processFiles(chooseFiles(toInclude, toExclude, sourceDir) map (p => p -> destDir.resolve(sourceDir.relativize(p))), toExclude, write)
  }

  /**
   * Processes a collection of file path pairs by cleaning the content of each source file, transforming it,
   * and writing the result to the corresponding destination file. The processing is logged, and the status is
   * returned based on overall success or failure.
   *
   * @param pathTuples an iterator of tuples, where each tuple contains the source file path as the first element
   *                   and the destination file path as the second element
   * @param write      a flag indicating whether to write the transformed content to the destination file;
   *                   defaults to true
   * @param logger     an implicit logger instance used to log processing information, warnings, and errors
   * @return a Try containing true if all files were successfully processed and written; false if any file
   *         failed to process; or a Failure with an exception if an error occurs
   */
  def processFiles(pathTuples: Iterator[(Path, Path)], toExclude: Path => Boolean, write: Boolean = true)(implicit logger: Logger): Try[Boolean] = {
    val xys = for {
      (s, d) <- pathTuples if !toExclude(s)
//      _ = logger.logInfo(s"processFiles: $s $d")
      destination = d.toAbsolutePath.toFile
      _ = ensureCanWriteFile(destination)
    } yield clean(s.toFile, destination, getDefaultStub(s.toString), write)
    sequence(xys) recover (recoveryFunction) map (xs => xs.forall(_ > 0))
  }

  /**
   * Filters files from a given source directory based on inclusion and exclusion criteria.
   *
   * @param toInclude a function that determines if a file should be included; returns true if the file is included
   * @param toExclude a function that determines if a file should be excluded; returns true if the file is excluded
   * @param sourceDir the path to the source directory containing the files to filter
   * @return an iterator of paths representing the filtered files
   */
  def chooseFiles(toInclude: Path => Boolean, toExclude: Path => Boolean, sourceDir: Path): Iterator[Path] = {
    import scala.collection.JavaConverters._
    walk(sourceDir).iterator().asScala.filterNot(toExclude).filter(toInclude)
  }

  /**
   * Cleans the content of the input file, processes specific markers, and writes the transformed output to the output file
   * if write mode is enabled. The method uses a default stub string for certain replacements and logs relevant information.
   *
   * @param inputFile         the file to be read and cleaned
   * @param outputFile        the file to which the cleaned content is written
   * @param defaultStubString the default string used for stub generation during content processing
   * @param write             a flag indicating whether the cleaned content should be written to the output file
   * @param logger            an implicit logger for logging information, warnings, or errors
   * @return a Try containing the total length of the cleaned content written to the output file, or 0 if write mode is disabled
   */
  def clean(inputFile: File, outputFile: File, defaultStubString: String, write: Boolean = true)(implicit logger: Logger): Try[Int] =
    if (write) {
      noleakFlat(Try(new BufferedWriter(new FileWriter(outputFile)))) {
        w =>
          noleak(Try(Source.fromFile(inputFile))) { s =>
            logger.logInfo(s"clean $inputFile $outputFile '$defaultStubString'")
            clean(w, defaultStubString, s.getLines())
          }
      }
    } else Try {
      logger.logWarning(s"clean (no-write) $inputFile $outputFile '$defaultStubString'")
      0
    }

  /**
   * Determines the default stub string to be used based on the file extension of the provided input file.
   *
   * @param inputFile the file name or path whose default stub string is to be determined; the file extension is used to choose the appropriate stub
   * @return a default stub string associated with the extension of the input file (e.g., Java, Scala, or unknown file types)
   * @throws CleanParseException if the input file extension is not supported
   */
  def getDefaultStub(inputFile: String): String = inputFile match {
    case stubExtensionR("java") => DEFAULTSTUB_JAVA
    case stubExtensionR("scala") => DEFAULTSTUB_SCALA
    case stubExtensionR(_) => DEFAULTSTUB_UNKNOWN
    case _ => throw CleanParseException(s"getDefaultStub: not supported for $inputFile")
  }

  override def skipWhitespace: Boolean = false

  /**
   * Represents a parsed line of source code including its line number, prefix, an optionally wrapped keyword, and suffix.
   *
   * @param n                The line number associated with this parsed line.
   * @param prefix           The prefix of the line, typically used to represent indentation or leading characters.
   * @param maybeMaybeString An optional nested structure containing a keyword or marker related to the line's content.
   * @param suffix           The suffix of the line, which may contain additional code or comments.
   */
  case class ParsedLine(n: Int, prefix: String, maybeMaybeString: Option[Option[String]], suffix: String) {
    val indented = """^(\s+)(\S+)$""".r
    def render(isStub: Boolean): String =
      (maybeMaybeString, suffix) match {
        case (Some(Some(`solution`)), b) =>
          s"$prefix// " + TOBEIMPLEMENTED + s" $b"
        case (Some(Some(`stub`)), b) =>
          discardLine
        case (Some(Some(a)), b) =>
          s"$prefix// $a$b"
        case (Some(None), b) if !isStub =>
          s"$prefix//$b"
        case (_, indented(indent,b)) if isStub =>
          s"$indent$indent$b" // This isn't really correct. Instead of the first copy of indent, we should have a non-empty prefix
        case (_, b) =>
          s"$prefix$b"
      }
  }

  /**
   * Parses a line of text input based on specific combinator patterns and constructs a `ParsedLine` object encapsulating the parsed components.
   *
   * @param x the line number associated with the input line being parsed
   * @return a parser that produces a `ParsedLine` object containing the parsed components of the line
   */
  def line(x: Int): Parser[ParsedLine] = prefix ~ opt(slashes ~> opt(prefix ~> keyword)) ~ anything ^^ {
    case prefix ~ Some(maybeKeyword) ~ comment => ParsedLine(x, prefix, Some(maybeKeyword), comment)
    case prefix ~ None ~ code => ParsedLine(x, prefix, None, code)
  }

  /**
   * Cleans the provided source content, processes specific markers, and writes
   * the cleaned output to the destination. The method uses internal logic to
   * handle different types of markers (e.g., solution, stub, terminator) and
   * ensures proper content transformation based on these markers.
   *
   * @param destination       the writer to which cleaned content is appended
   * @param defaultStubString the default string to use when generating stub content
   * @param lines an Iterator of String
   * @param logger            an implicit logger for logging debug or warning messages
   * @return the total length of the content written to the destination
   */
  def clean(destination: Writer, defaultStubString: String, lines: Iterator[String])(implicit logger: Logger): Int = {
    // CONSIDER avoiding vars
    var output = true
    var isStub = false

    def process(commentedLine: ParsedLine): String = {
      var transition = false
      var defaultStub = false
      commentedLine.maybeMaybeString match {
        case Some(Some(`solution`)) =>
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
      if (defaultStub) {
        // This line is a bit of a kluge, but it's necessary for working with Scala 3
        s"${commentedLine.prefix}${commentedLine.prefix}$defaultStubString"
      } else if (transition || output)
        commentedLine.render(isStub)
      else
        discardLine
    }

    val result = FileCleaner.sequence(for (l <- lines.zipWithIndex) yield parseLine(l)) match {
      case scala.util.Success(cs) =>
        val strings = for {
          c <- cs
          x = process(c) if x != discardLine
        } yield x
        val str = strings.mkString("\n")
        destination.append(str)
        str.length
      case scala.util.Failure(x) => throw x
    }
    if (isStub || !output)
      logger.logWarning(s"Solution not terminated?: end of source file")
    result
  }

  /**
   * Parses a line of input into a structured format encapsulated in a `ParsedLine` object.
   * The method uses combinator parsing to extract specific elements of the line.
   *
   * @param wl a tuple where the first element is the line to be parsed (a string),
   *           and the second element is the line number (an integer)
   * @return a `Try` containing a `ParsedLine` object if parsing succeeds, or a failure with an exception if parsing fails
   */
  def parseLine(wl: (String, Int)): Try[ParsedLine] = this.parseAll(line(wl._2), wl._1) match {
    case this.Success(result, _) => scala.util.Success(result)
    case this.Failure(msg, next) => scala.util.Failure(CleanParseException(msg + "@" + next.toString))
    case this.Error(msg, next) => scala.util.Failure(CleanParseException(msg + "@" + next.toString))
  }

  private val prefix = """\s*""".r

  private def anything = """.*""".r

  def slashes: Parser[String] = """//""".r

  private def keyword: Parser[String] = solution | stub | terminator | failure("not a keyword")

  val DEFAULTSTUB_SCALA = "???"
  val DEFAULTSTUB_JAVA = """throw new RuntimeException("implementation missing");"""
  val DEFAULTSTUB_UNKNOWN = "???"
  val TOBEIMPLEMENTED = "TO BE IMPLEMENTED"
}

/**
 * Utility object for cleaning and processing files, providing several methods
 * to handle transformations, resource handling, and logging for file cleaning purposes.
 */
object FileCleaner {
  lazy val logger: Logger = Logger(getClass)

  /**
   * Ensures that the specified file is writable. If the file is not writable,
   * the method attempts to create its parent directories.
   *
   * @param d the file to check for write access
   * @return Unit does not return a value, but creates the parent directories
   *         if necessary to ensure that the file can be written
   */
  def ensureCanWriteFile(d: File): Unit =
    if (!d.canWrite) {
      val dir = d.getParentFile
      dir.mkdirs()
    }

  /**
   * Converts an iterator of `Try[X]` into a single `Try` containing a sequence of all successes.
   * If any element in the iterator is a failure, the resulting `Try` will also be a failure with the first encountered exception.
   *
   * This method is useful for aggregating multiple computations, each of which may fail, into a single combined result.
   * It accumulates successful results into a sequence or propagates the first failure encountered.
   *
   * @param xys an iterator of `Try` instances, where each `Try` represents a computation that may succeed or fail
   * @tparam X the type of successful results contained within the `Try` instances
   * @return a `Try` of a `Seq` containing all successful results from the iterator, or the first failure if any
   */
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

  /**
   * Processes the input array of strings and merges its elements with predefined defaults to
   * generate a structured list of configuration values.
   *
   * Each input element is paired with a default value and the non-empty element is retained.
   *
   * @param sa An array of strings containing the input arguments to be processed.
   * @return A list of strings representing the processed configuration values, where missing
   *         or empty input values are replaced with predefined defaults.
   */
  def getConfiguration(sa: Array[String]): List[String] = {
    def merge(x: (String, String)): String = if (x._1.nonEmpty) x._1 else x._2

    sa.toList zipAll(List("", "", "", solutionScala, stubScala, endScala), "", "") map merge
  }

  /**
   * A partial function that provides error recovery logic for specific throwable cases.
   *
   *  - For non-fatal exceptions, it logs the error message using the provided logger and returns an empty sequence.
   *  - For other exceptions, it re-throws the exception to ensure it is handled appropriately.
   */
  private val recoveryFunction: PartialFunction[Throwable, Seq[Int]] = {
    case NonFatal(x) =>
      logger.logError(x.toString)
      Nil
    case x => throw x
  }

  val validExtensions = Seq(".java", ".scala", ".sbt", ".sc", ".conf", ".xml", ".properties") // .csv?
  val stubExtensionR: Regex = """^.+\.(\w+)$""".r

  val solutionScala = "SOLUTION"
  val stubScala = "STUB"
  val endScala = "END"

  val discardLine = "//************//"

}

/**
 * The `CleanTree` object is responsible for processing and cleaning a directory tree of source files.
 * It reads configurations from input arguments, processes files based on defined inclusion and exclusion criteria,
 * and applies cleaning rules to each file. The cleaned files are then written to a destination directory.
 * <p>
 * The command-line interface consists of up to six fields:
 * <ol>
 * <li>the source directory</li>
 * <li>the target directory</li>
 * <li>the list of exclusions, starting with "-" and each exclusion ending with :</li>
 * <li>the solution pattern</li>
 * <li>the stub pattern</li>
 * <li>the termination pattern</li>
 * </ol>
 *
 * It utilizes a `FileCleaner` instance to perform the file-specific cleaning logic and leverages a `Logger` for logging.
 *
 * The command line arguments for copying the INFO6205 project to DSAIPG is:
 * /Users/rhillyard/IdeaProjects/INFO6205/src /Users/rhillyard/IdeaProjects/DSAIPG/Java/src -huskySort:admin:life:madhava:MySet:HashCodeSortTest "SOLUTION" SKELETON
 */
object CleanTree extends App {

  import java.nio.file.Files

  implicit lazy val logger: Logger = Logger(getClass)

  val List(sourcePath, destPath, exclusionString, solution, stub, terminator) = getConfiguration(args)
  if (sourcePath.isEmpty || destPath.isEmpty) logger.logError("You must provide root paths for source and destination (at least)")
  else {
    val exclusions = exclusionString.replace("-", "").split(":").map(_.trim).filter(_.nonEmpty).toList
    logger.logInfo(s"CleanTree: $sourcePath $destPath $exclusions $solution $stub $terminator")
    val cleaner = new FileCleaner(solution, stub, terminator)
    val result = cleaner.cleanTree(sourcePath, destPath, toInclude, toExclude(exclusions)).get
    if (result) logger.logInfo("CleanTree complete") else logger.logWarning("CleanTree: At least one destination file is empty")
  }

  /**
   * Determines if the given path should be included in further processing.
   * A path is included if it represents a regular file and has a valid extension.
   *
   * @param p the path to be evaluated
   * @return true if the path is a regular file and its filename has a valid extension, false otherwise
   */
  def toInclude(p: Path): Boolean = {
    val result = Files.isRegularFile(p) && validateExt(p.getFileName.toString)
//    logger.logInfo(s"toInclude: $result $p")
    result
  }

  /**
   * Determines if the given path should be excluded based on the provided list of exclusions.
   *
   * A path is excluded if it satisfies specific criteria described in the `isExcluded` method,
   * such as containing components that match exclusion criteria or having a filename starting with a dot.
   *
   * @param exclusions A list of strings representing exclusion criteria.
   * @param p          The path to evaluate for exclusion.
   * @return true if the path is determined to be excluded, false otherwise.
   */
  def toExclude(exclusions: List[String])(p: Path): Boolean = {
    val result = isExcluded(p, exclusions)
//    logger.logInfo(s"toExclude($exclusions): $result $p")
    result
  }

  /**
   * Determines if the provided path is excluded based on the specified list of exclusions.
   * A path is excluded if its filename starts with a dot or if any component of the path
   * starts with a string present in the exclusions list.
   *
   * The time taken for this method is quadratic: number of path components * number of exclusions.
   *
   * @param path       the path to check for exclusion
   * @param exclusions a list of strings representing exclusion criteria
   * @return true if the path is excluded, false otherwise
   */
  private def isExcluded(path: Path, exclusions: List[String]): Boolean =
    path.getFileName.startsWith(".") ||
//      isDirectory(path, LinkOption.NOFOLLOW_LINKS) &&
              excludedComponent(path, exclusions)


  /**
   * Determines if any component of the specified path starts with a string from the exclusions list.
   * If a match is found, the method logs the excluded path and returns true. Otherwise, it returns false.
   *
   * @param path       the path to evaluate for exclusions
   * @param exclusions a list of strings representing exclusion criteria
   * @return true if any path component matches an exclusion, false otherwise
   */
  private def excludedComponent(path: Path, exclusions: List[String]) = {
    val pathComponentsExcluded: Seq[Boolean] = for {
      x <- getPathComponents(path)
    } yield exclusions.exists(x.startsWith)
    val result = pathComponentsExcluded.exists(b => b)
//    if (result) logger.logInfo(s"exclude: $path")
    result
  }

  /**
   * Extracts the components of the specified path as an immutable indexed sequence of strings.
   *
   * @param path the Path object to extract components from
   * @return an immutable indexed sequence containing the string representations of the path components
   */
  private def getPathComponents(path: Path): immutable.IndexedSeq[String] = {
    val n = path.getNameCount
    for (i <- 0 until n) yield path.getName(i).toString
  }

  /**
   * Validates if the given filename ends with any of the predefined valid extensions.
   *
   * @param filename the name of the file to be validated
   * @return true if the filename ends with one of the valid extensions, false otherwise
   */
  private def validateExt(filename: String): Boolean = (for (ext <- validExtensions if filename.endsWith(ext)) yield ext).nonEmpty
}

/**
 * The `Clean` object is an entry point for a file cleaning application. It is responsible for
 * orchestrating the cleaning process by initializing necessary configurations, creating a
 * `FileCleaner` instance, and executing the cleaning operation.
 *
 * This program reads its configuration from command-line arguments and uses a `Logger` to log
 * relevant information or error messages.
 *
 * The required paths for the source and destination must be provided; otherwise, an error is
 * logged and the program does not proceed. The cleaning process involves handling exclusions,
 * a solution strategy, stubbing, and terminating behavior as configured.
 *
 * Functionality includes:
 * - Reading and validating configuration from input arguments.
 * - Logging errors for invalid input or failures during execution.
 * - Initializing and invoking a `FileCleaner` instance.
 * - Logging operation results and statuses.
 *
 * Implicitly requires a `Logger` for logging activities.
 */
object Clean extends App {

  implicit lazy val logger: Logger = Logger(getClass)
  val result = doClean(args)
  if (result > 0) logger.logInfo("Clean complete") else logger.logWarning("Clean: At least one destination file is empty")

  def doClean(ws: Array[String]): Int = {
    val List(sourcePath, destPath, _, solution, stub, terminator) = getConfiguration(ws)
    if (sourcePath.isEmpty || destPath.isEmpty) {
      logger.logError("You must provide paths for source and destination (at least)")
      -1
    } else {
      val cleaner = new FileCleaner(solution, stub, terminator)
      val triedInt = cleaner.clean(sourcePath, destPath)
      val result: Int = triedInt.get // NOTE: This will throw an exception if there was a failure
      result
    }
  }
}

/**
 * Exception representing a specific failure encountered during the cleaning or parsing process.
 *
 * This exception is typically thrown when errors occur in the context of cleaning operations,
 * especially when dealing with unsupported file types or invalid parsing scenarios.
 *
 * @param str the error message describing the reason for the exception
 */
case class CleanParseException(str: String) extends Exception(str)