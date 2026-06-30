package edu.neu.coe.csye7200.assthw

/**
  * This is the second part of assignment "helloworld."
  * You should be able to run this program with provided csv file using sbt run.
  * You task is to replace line 38 with the following line of code:
  * println((for (m <- ingester(source); if (m.properties(20)=="New Zealand")) yield m).size)
  * Run this program with provided csv file, and submit a screenshot of the result.
  * It should print the number of Kiwi (New Zealand) Movies.
  */

import scala.io.Source

/**
 * Represents a typeclass to transform a sequence of strings into an instance of type `X`.
 *
 * @tparam X the type of the resulting instance after transformation.
 */
trait Ingestible[X] {
  /**
   * Transforms a sequence of strings into an instance of type `X`.
   *
   * @param ws a sequence of strings that will be transformed.
   * @return an instance of type `X` created from the input sequence of strings.
   */
  def fromStrings(ws: Seq[String]): X
}

/**
 * The `Ingest` class enables conversion of data from a `Source` into an iterator of a specified type `T`,
 * where the conversion logic is defined by an implicit `Ingestible[T]` typeclass instance.
 *
 * @tparam T the target type of the ingested elements, requiring an implicit instance of `Ingestible[T]`.
 */
class Ingest[T: Ingestible] extends (Source => Iterator[T]) {
  /**
   * Converts data from the provided `Source` into an `Iterator` of type `T` by processing each line
   * and transforming it into an instance of type `T`. The transformation uses the implicit `Ingestible[T]` typeclass.
   *
   * @param source the input `Source` containing the data to be processed, typically a structured, delimited text source.
   * @return an `Iterator` of elements of type `T`, where each element represents a transformed line from the source.
   */
  def apply(source: Source): Iterator[T] =
    source.getLines().toSeq.map(e => summon[Ingestible[T]].fromStrings(e.split(",").toList)).iterator
}

/**
 * Represents a movie entity, characterized by a collection of property values.
 *
 * @param properties a sequence of string values representing various attributes of the movie.
 */
case class Movie(properties: Seq[String])

/**
 * Main program that ingests movie data from a source file and prints each movie's properties.
 * The source file can either be specified as the first command-line argument or defaults to a resource file named "movie_metadata_5000.csv".
 * The method uses a custom `Ingest` implementation that relies on the `Ingestible` type class to parse and construct `Movie` objects.
 *
 * @param args a variable number of string arguments provided via the command line. The first argument (if provided) specifies the file path to read movie data from.
 * @return Unit as the method is only responsible for side effects such as reading data and printing movie properties to the console.
 */
@main
def ingestMovies(args: String*): Unit = {

  // NOTE This was formerly (Scala 2) handled with a trait IngestibleMovie, and an implicit object IngestibleMovie.
  given Ingestible[Movie] with {
    def fromStrings(ws: Seq[String]): Movie = Movie.apply(ws)
  }

  val ingester = new Ingest[Movie]()
  val source = args.toList match {
    case Nil =>
      Source.fromResource("movie_metadata_5000.csv")
    case h :: _ =>
      Source.fromFile(h)
  }

  for (m <- ingester(source)) println(m.properties.mkString(", "))
  source.close()

  // Please note that an alternative to the definition of source above would be as in the following comment:
  //  val source = Source.fromFile(if (args.length>0) args.head else "movie_metadata_5000.csv")
}