package edu.neu.coe.csye7200.asstmd

import scala.io.Source
import scala.util.*

/**
 * This class represents a Movie from the IMDB data file on Kaggle.
 * Although the limitation on 22 fields in a case class has partially gone away, it's still convenient to group the different attributes together into logical classes.
 *
 * Created by scalaprof on 9/12/16.
 *
 * Common questions in this assignment:
 * 1. Where is main method?
 *    In most case, you don't need to run main method for assignments.
 *    Unit tests are provided to test your implementation.
 *
 * 2. How to understand the whole program in this assignment?
 *    I won't suggest you to understand the whole program in this assignment,
 *    there are some advanced features like `implicit` which hasn't been covered in class.
 *    You should be able to understand it before midterm.
 *    I will suggest you only focus on TO BE IMPLEMENTED fragments in the assignments.
 *
 */
case class Movie(title: String,
                 format: Format,
                 production: Production,
                 reviews: Reviews,
                 director: Principal,
                 actor1: Principal,
                 actor2: Principal,
                 actor3: Principal,
                 genres: Seq[String],
                 plotKeywords: Seq[String],
                 imdb: String)

/**
 * The movie format (including language and duration).
 *
 * @param color       whether filmed in color
 * @param language    the native language of the characters
 * @param aspectRatio the aspect ratio of the film
 * @param duration    its length in minutes
 */
case class Format(color: Boolean, language: String, aspectRatio: Double, duration: Int):
  override def toString =
    s"${if (color) "Color" else "B&W"},$language,$aspectRatio,$duration"

/**
 * The production: its country, year, and financials
 *
 * @param country   country of origin
 * @param budget    production budget in US dollars
 * @param gross     gross earnings (?)
 * @param titleYear the year the title was registered (?)
 */
case class Production(country: String, budget: Int, gross: Int, titleYear: Int):
  /**
   * Determines if the production originates from New Zealand.
   *
   * @return true if the production's country is "New Zealand", false otherwise.
   */
  def isKiwi: Boolean = this match {
    case Production("New Zealand", _, _, _) =>
      true
    case _ =>
      false
  }

  /**
   * Determines if the production is considered "cheap" based on its budget.
   *
   * @return true if the production's budget is less than 1000 USD, false otherwise.
   */
  def isCheap: Boolean = budget < 1000

/**
 * Information about various forms of review, including the content rating.
 */
case class Reviews(imdbScore: Double,
                   facebookLikes: Int,
                   contentRating: Rating,
                   numUsersReview: Int,
                   numUsersVoted: Int,
                   numCriticReviews: Int,
                   totalFacebookLikes: Int)

/**
 * A cast or crew principal
 *
 * @param name          name
 * @param facebookLikes number of FaceBook likes
 */
case class Principal(name: Name, facebookLikes: Int):
  override def toString =
    s"$name ($facebookLikes likes)"

/**
 * A name of a contributor to the production
 *
 * @param first  first name
 * @param middle middle name or initial
 * @param last   last name
 * @param suffix suffix
 */
case class Name(first: String, middle: Option[String], last: String, suffix: Option[String]):
  override def toString =
    s"$first ${middle.getOrElse("")} $last ${suffix.getOrElse("")}}"

/**
 * The US rating
 */
case class Rating(code: String, age: Option[Int]):
  override def toString: String =
    code + age.map("-" + _).getOrElse("")

/**
 * Companion object for the Movie case class, providing utility methods for parsing and manipulating movie data.
 */
object Movie {

  given Parsable[Movie] with {
    /**
     * Method to yield a Try[Movie] from a String representing a line of input of the movie database file.
     *
     * TODO 11 points.
     *
     * @param w a line of input.
     * @return a Try[Movie]
     */
    def parse(w: String): Try[Movie] = {
      // TO BE IMPLEMENTED 
      Try(???)
      // END
    }
  }

  // Expose a pure method that processes a file and returns the count of Kiwi movies.
  def runFile(filename: String): Int = {
    val ingester = new Ingest[Movie]()
    val source = Source.fromFile(filename)
    try {
      val triedMovies: Iterator[Try[Movie]] =
        for (my <- ingester(source)) yield for (m <- my; if m.production.isKiwi) yield m
      val optionalMovies: Seq[Option[Movie]] = triedMovies.toList.map(_.toOption)
      val kiwiMovies: Option[Seq[Movie]] = sequenceOptimistic(optionalMovies)
      kiwiMovies.foreach(_.foreach(println))
      kiwiMovies.getOrElse(Nil).size
    } finally {
      source.close()
    }
  }

  /**
   * Form a list from the elements explicitly specified (by position) from the given list
   *
   * @param list    a list of Strings
   * @param indices a variable number of index values for the desired elements
   * @return a list of Strings containing the specified elements in order
   */
  def elements(list: Seq[String], indices: Int*): List[String] = {
    // Hint: form a new list which is consisted by the elements in list in position indices. Int* means array of Int.
    // 6 points
    val result: Seq[String] = {
      // TO BE IMPLEMENTED 
            ???
    }
    result.toList
  }

  /**
   * Alternative apply method for the Movie class
   *
   * @param ws a sequence of Strings
   * @return a Movie
   */
  def apply(ws: Seq[String]): Movie = {
    // we ignore faceNumber_in_poster since I have no idea what that means.
    val title = ws(11)
    val format = Format(elements(ws, 0, 19, 26, 3))
    val production = Production(elements(ws, 20, 22, 8, 23))
    val reviews = Reviews(elements(ws, 25, 27, 21, 18, 12, 2, 13))
    val director = Principal(elements(ws, 1, 4))
    val actor1 = Principal(elements(ws, 10, 7))
    val actor2 = Principal(elements(ws, 6, 24))
    val actor3 = Principal(elements(ws, 14, 5))
    val plotKeywords = ws(16).split("""\|""").toList
    val genres = ws(9).split("""\|""").toList
    val imdb = ws(17)
    Movie(title, format, production, reviews, director, actor1, actor2, actor3, genres, plotKeywords, imdb)
  }

  /**
   * Converts a sequence of `Try[X]` into a single `Try` containing a sequence of `X`.
   * If any `Try` in the input sequence is `Failure`, the result will be a `Failure` with the same exception.
   * If all `Try`s are `Success`, the result will be a `Success` containing a sequence of the successful values.
   *
   * @param xys a sequence of `Try[X]` objects
   * @tparam X the type of the elements wrapped in the `Try`
   * @return a `Try` containing a sequence of `X`, or a `Failure` if any `Try` in the sequence is a `Failure`
   */
  def sequence[X](xys: Seq[Try[X]]): Try[Seq[X]] = xys.foldLeft(Try(Seq[X]())) {
    (xsy, xy) => for (xs <- xsy; x <- xy) yield xs :+ x
  }

  /**
   * This method will take a sequence of Option[X] and return an option of Seq[X].
   * It is pessimistic: if any of the input elements are None, the result will be None.
   *
   * @param xos a Seq of Option[X].
   * @tparam X the underlying type.
   * @return an Option of Seq[X].
   */
  def sequencePessimistic[X](xos: Seq[Option[X]]): Option[Seq[X]] = xos.foldLeft(Option(Seq[X]())) {
    (xso, xo) => for (xs <- xso; x <- xo) yield xs :+ x
  }

  /**
   * This method will take a sequence of Option[X] and return an option of Seq[X].
   * It is optimistic: if any of the input elements are Some(...), the result will be Some(...).
   * otherwise, if all the input elements are None, the result will be None.
   *
   * @param xos a Seq of Option[X].
   * @tparam X the underlying type.
   * @return an Option of Seq[X].
   */
  def sequenceOptimistic[X](xos: Seq[Option[X]]): Option[Seq[X]] = xos.foldLeft[Option[Seq[X]]](None) {
    (xso: Option[Seq[X]], xo: Option[X]) =>
      xo match {
        case None =>
          xso
        case Some(x) =>
          xso match {
            case Some(xs) =>
              Some(xs :+ x)
            case None =>
              Some(Seq(x))
          }
      }
  }
}

/**
 * Companion object for the Format class.
 *
 * Provides a factory method to create an instance of the Format class based on a list of strings.
 * The method parses the parameters from the list and constructs a Format object. It also handles
 * validation to ensure the list contains the expected number and types of parameters.
 *
 * @throws ParseException if the input list does not match the required format.
 */
object Format {
  def apply(params: List[String]): Format = params match {
    case color :: language :: aspectRatio :: duration :: Nil =>
      Format(color == "Color", language, aspectRatio.toDouble, duration.toInt)
    case _ =>
      throw ParseException(s"logic error in Format: $params")
  }
}

object Production {
  def apply(params: List[String]): Production = params match {
    case country :: budget :: gross :: titleYear :: Nil =>
      Production(country, budget.toInt, gross.toInt, titleYear.toInt)
    case _ =>
      throw ParseException(s"logic error in Production: $params")
  }
}

object Reviews {
  def apply(params: List[String]): Reviews = params match {
    case imdbScore :: facebookLikes :: contentRating :: numUsersReview :: numUsersVoted :: numCriticReviews :: totalFacebookLikes :: Nil =>
      Reviews(imdbScore.toDouble, facebookLikes.toInt, Rating(contentRating), numUsersReview.toInt, numUsersVoted.toInt, numCriticReviews.toInt, totalFacebookLikes.toInt)
    case _ =>
      throw ParseException(s"logic error in Reviews: $params")
  }
}

object Name {
  // NOTE this regex will not parse all names in the Movie database correctly.
  // Nevertheless, it gets most of them.
  private val rName = """^([\p{L}\-\']+\.?)\s*(([\p{L}\-]+\.)\s)?([\p{L}\-\']+\.?)(\s([\p{L}\-]+\.?))?$""".r

  def apply(name: String): Name =
    (for (ws <- rName.unapplySeq(name)) yield for (w <- ws) yield Option(w)) match {
      case Some(Seq(Some(first), _, maybeMiddle, Some(last), _, maybeSuffix)) =>
        Name(first, maybeMiddle, last, maybeSuffix)
      case x =>
        throw ParseException(s"parse error in Name: $name (parsed as $x)")
    }
}

/**
 * Companion object for the `Principal` case class, providing factory methods to create Principal instances.
 */
object Principal {
  def apply(params: List[String]): Principal = params match {
    case name :: facebookLikes :: Nil =>
      Principal(name, facebookLikes.toInt)
    case _ =>
      throw ParseException(s"logic error in Principal: $params")
  }

  def apply(name: String, facebookLikes: Int): Principal =
    Principal(Name(name), facebookLikes)
}

/**
 * The Rating object provides an alternative apply method to instantiate the Rating case class by parsing a single string input.
 * The input string represents a rating in a standard format, such as "PG", "PG-13", or "R".
 * The format can include a code, optionally followed by a dash and a numeric age limit.
 *
 * Invalid inputs such as "Unrated", "Approved", "Not Rated", an empty string, "NULL", or null are explicitly rejected,
 * resulting in a ParseException.
 *
 * The apply method uses a regular expression to extract the components of a valid rating string and construct a Rating instance.
 * If the string includes a numerical age, it is parsed and included in the Rating instance; otherwise, the age is None.
 */
object Rating {
  // Hint: This regex matches three patterns: (\w*), (-(\d\d)), (\d\d), for example "PG-13", the first one matches "PG", second one "-13", third one "13".
  private val rRating = """^(\w*)(-(\d\d))?$""".r

  /**
   * Alternative apply method for the Rating class such that a single String is decoded.
   * We should explicitly reject any of the following strings for the rating (including the null pointer):
   * "Unrated", "Approved", "Not Rated", "", "NULL"
   *
   * @param s a String made up of a code, optionally followed by a dash and a number, e.g. "R" or "PG-13"
   * @return a Rating
   */
  // Hint: This should be similar to the apply method in Object Name. The parameter of apply in case match should be the same as case class Rating
  // You will need to handle two cases based on rRating (not just one)
  // 13 points
  def apply(s: String): Rating = s match {
    case rRating("Unrated" | "Approved" | "Not Rated" | "" | "NULL" | null, _, _) =>
      throw ParseException(s"parse error in Rating: $s")
    // TO BE IMPLEMENTED 
     case rRating(_,_,_) =>
    ???
    // END
    case _ => throw ParseException(s"parse error in Rating: $s")
  }
}

case class ParseException(w: String) extends Exception(w)